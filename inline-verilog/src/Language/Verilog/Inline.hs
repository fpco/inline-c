{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Language.Verilog.Inline
  ( verilog
  , block
  , verbatim
  ) where

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH
import qualified Language.Haskell.TH.Quote as TH

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C

import           Data.Maybe (maybeToList)
import           Control.Monad (msum, void, MonadPlus, when, join)
import           Data.Traversable (for)
import           Control.Monad.IO.Class (liftIO)
import           System.Exit (ExitCode(..))
import           System.Process (readProcessWithExitCode)
import           System.Environment (lookupEnv)
import           Data.Maybe (fromMaybe)
import           System.IO.Temp (withSystemTempDirectory)
import           Data.List (dropWhileEnd, intercalate)
import           Data.Char (isSpace)
import           Text.RawString.QQ (r)
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Pos as Parsec
import           Text.Parser.Char
import           Text.Parser.Combinators
import           Text.Parser.LookAhead
import           Text.Parser.Token
import qualified Text.Parser.Token.Highlight as Highlight
import qualified Data.HashSet as HashSet
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as Aeson
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as BSL
import           Data.IORef
import           System.IO.Unsafe (unsafePerformIO)
import qualified Data.HashSet as HS
import           Foreign.Ptr (Ptr)
import           Data.Word
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Control.Applicative (Alternative)

data PortDirection = In | Out | InOut deriving (Show, Eq)

data DataType = Wire | Reg | Logic | Integer | Real deriving (Show, Eq)

data Range = Range { rangeMSB :: Int, rangeLSB :: Int } deriving (Show, Eq)
rangeWidth :: Range -> Int
rangeWidth (Range msb lsb) = msb - lsb + 1

data Port = Port
  { portDirection :: PortDirection
  , portDataType :: Maybe DataType
  , portRanges :: [Range]
  , portName :: String
  } deriving (Show, Eq)

data Module = Module
  { mName :: String
  , mPorts :: [Port]
  , mBody :: String
  } deriving (Show, Eq)

Aeson.deriveJSON Aeson.defaultOptions ''PortDirection
Aeson.deriveJSON Aeson.defaultOptions ''DataType
Aeson.deriveJSON Aeson.defaultOptions ''Range
Aeson.deriveJSON Aeson.defaultOptions ''Port
Aeson.deriveJSON Aeson.defaultOptions ''Module

type VParser m =
  ( Monad m
  , Functor m
  , Applicative m
  , MonadPlus m
  , Parsing m
  , CharParsing m
  , TokenParsing m
  , LookAheadParsing m
#if (MIN_VERSION_base(4,13,0))
  , MonadFail m
#endif
  )

verilogIdentStart :: [Char]
verilogIdentStart = ['a'..'z'] ++ ['A'..'Z'] ++ ['_']

-- Technically this also supports $, but we use it for things that
-- should also be Haskell identifiers.
verilogIdentLetter :: [Char]
verilogIdentLetter = ['a'..'z'] ++ ['A'..'Z'] ++ ['_'] ++ ['0'..'9']

verilogReservedWords :: HashSet.HashSet String
verilogReservedWords = HashSet.fromList
  [ "always","and","assign","automatic"
  , "begin","buf","bufif0","bufif1"
  , "case","casex","casez","cell"
  , "cmos","config","deassign","default"
  , "defparam","design","disable","edge"
  , "else","end","endcase","endconfig"
  , "endfunction","endgenerate","endmodule","endprimitive"
  , "endspecify","endtable","endtask","event"
  , "for","force","forever","fork"
  , "function","generate","genvar","highz0"
  , "highz1","if","ifnone","incdir"
  , "include","initial","inout","input"
  , "instance","integer","join","larger"
  , "liblist","library","localparam","macromodule"
  , "medium","module","nand","negedge"
  , "nmos","nor","noshow-cancelled","not"
  , "notif0","notif1","or","output"
  , "parameter","pmos","posedge","primitive"
  , "pull0","pull1","pullup","pulldown"
  , "pulsestyle_ondetect","pulsestyle_onevent","rcmos","real"
  , "realtime","reg","release","repeat"
  , "rnmos","rpmos","rtran","rtranif0"
  , "rtranif1","scalared","show-cancelled","signed"
  , "small","specify","specpa","strong0"
  , "strong1","supply0","supply1","table"
  , "task","time","tran","tranif0"
  , "tranif1","tri","tri0","tri1"
  , "triand","trior","trireg","use"
  , "vectored","wait","wand","weak0"
  , "weak1","while","wire","wor"
  , "xnor","xor" ]

verilogIdentStyle :: (TokenParsing m, Monad m) => IdentifierStyle m
verilogIdentStyle = IdentifierStyle
  { _styleName = "Verilog identifier"
  , _styleStart = oneOf verilogIdentStart
  , _styleLetter = oneOf verilogIdentLetter
  , _styleReserved = verilogReservedWords
  , _styleHighlight = Highlight.Identifier
  , _styleReservedHighlight = Highlight.ReservedIdentifier
  }

pPortDirection :: VParser m => m PortDirection
pPortDirection = msum
  [ In <$ reserve verilogIdentStyle "input"
  , Out <$ reserve verilogIdentStyle "output"
  , InOut <$ reserve verilogIdentStyle "inout"
  ]

pDataType :: VParser m => m DataType
pDataType = msum
  [ Wire <$ reserve verilogIdentStyle "wire"
  , Reg <$ reserve verilogIdentStyle "reg"
  , Logic <$ reserve verilogIdentStyle "logic"
  , Integer <$ reserve verilogIdentStyle "integer"
  , Real <$ reserve verilogIdentStyle "real"
  ]

pRange :: VParser m => m Range
pRange = brackets $ do
  msb <- token natural
  _ <- token (char ':')
  lsb <- token natural
  return $ Range (fromIntegral msb) (fromIntegral lsb)

pPort :: VParser m => m Port
pPort = do
  dir <- pPortDirection
  typ <- optional pDataType
  rngs <- many pRange
  name <- ident verilogIdentStyle
  return Port
    { portDirection = dir
    , portDataType = typ
    , portRanges = rngs
    , portName = name
    }

pModule :: VParser m => String -> m Module
pModule mname = do
  reserve verilogIdentStyle "module"
  ports <- lookAhead $ do
    ports <- join . maybeToList <$> optional (parens (commaSep pPort))
    _ <- token (char ';')
    return ports
  body <- many anyChar
  return $ Module mname ports body

newtype VParserImpl t a = VPI (t a)
  deriving (MonadFail, Alternative, Functor, Applicative, Monad, MonadPlus, Semigroup, Parsing, CharParsing, LookAheadParsing)

instance MonadTrans VParserImpl where
  lift = VPI

instance (Monad m, MonadPlus m, CharParsing m) => TokenParsing (VParserImpl m) where
  someSpace = do
    skipSome (satisfy isSpace)
    msum
      [ try (string "//") >> manyTill anyChar (try (char '\n')) >> someSpace
      , try (string "/*") >> manyTill anyChar (try (string "*/")) >> someSpace
      , return ()
      ]

runVParser :: VParserImpl (Parsec.Parsec String ()) a -> Parsec.SourceName -> String -> Either Parsec.ParseError a
runVParser (VPI m) name s = Parsec.parse m name s

runParserInQ :: String -> (forall m. VParser m => m a) -> TH.Q a
runParserInQ s p = do
  loc <- TH.location
  let (line, col) = TH.loc_start loc
  let parsecLoc = Parsec.newPos (TH.loc_filename loc) line col
  let p' = VPI (Parsec.setPosition parsecLoc) *> (spaces >> p) <* eof
  case runVParser p' (TH.loc_filename loc) s of
    Left err -> do
      -- TODO consider prefixing with "error while parsing Verilog" or similar
      fail $ show err
    Right res -> do
      return res

data VerilogFileItem =
    VFIVerbatim String
  | VFIModule Module
  deriving (Eq, Show)
Aeson.deriveJSON Aeson.defaultOptions ''VerilogFileItem

verbatimItem :: VerilogFileItem -> TH.DecsQ
verbatimItem a = C.verbatim (T.unpack (T.decodeUtf8 (BSL.toStrict (Aeson.encode a))))

{-# NOINLINE verilogModuleCounter #-}
verilogModuleCounter :: IORef Int
verilogModuleCounter = unsafePerformIO (newIORef 0)

data ModuleInfo = ModuleInfo
  { miName :: String
  , miInputs :: [(String, TH.Type, String)] -- Name, Haskell, C
  , miOutputs :: [(String, TH.Type, String)] -- Name, Haskell, C
  } deriving (Eq, Show)

portType :: Port -> TH.Q (TH.Type, String)
portType p = case portRanges p of
  [] -> (, "uint8_t") <$> [t| Bool |]
  [n] | rangeWidth n == 1 -> (, "uint8_t") <$> [t| Bool |]
  [n] | rangeWidth n <= 8 -> (, "uint8_t") <$> [t| Word8 |]
  [n] | rangeWidth n <= 16 -> (, "uint16_t") <$> [t| Word16 |]
  [n] | rangeWidth n <= 32 -> (, "uint32_t") <$> [t| Word32 |]
  [n] | rangeWidth n <= 64 -> (, "uint64_t") <$> [t| Word64 |]
  _ -> fail $ "Unsupported port ranges " ++ show (portRanges p)

moduleInfo :: Module -> TH.Q ModuleInfo
moduleInfo m = do
  when (HS.size (HS.fromList (map portName (mPorts m))) /= length (mPorts m)) $
    fail "Duplicated port names"
  (inputs, outputs) <- fmap mconcat $ for (mPorts m) $ \port -> do
    (hTyp, cTyp) <- portType port
    let p = (portName port, hTyp, cTyp)
    case portDirection port of
      In -> return ([p], [])
      Out -> return ([], [p])
      InOut -> fail "InOut not supported yet" -- would be very easy to fix, just lazy
  return ModuleInfo
    { miName = mName m
    , miInputs = inputs
    , miOutputs = outputs
    }

block :: TH.QuasiQuoter
block = TH.QuasiQuoter
  { TH.quoteExp = \s -> do
      -- create C symbol name
      modCount <- liftIO (atomicModifyIORef' verilogModuleCounter (\c -> (c + 1, c + 1)))
      let modn = "_inline_verilog_" ++ show modCount
      -- parse verilog module and emit it in the file
      modu <- runParserInQ s (pModule modn)
      TH.addTopDecls =<< verbatimItem (VFIModule modu)
      -- create FFI type
      minfo <- moduleInfo modu
      ffiImportName <- TH.newName . show =<< TH.newName "inline_verilog"
      let ffiTyp = foldr
            (\(_, ty, _) f -> TH.AppT (TH.AppT TH.ArrowT ty) f)
            (foldr
              (\(_, ty, _) f -> TH.AppT (TH.AppT TH.ArrowT (TH.AppT (TH.ConT ''Ptr) ty)) f)
              (TH.AppT (TH.ConT ''IO) (TH.TupleT 0))
              (miOutputs minfo))
            (miInputs minfo)
      -- create FFI declaration
      ffiDec <- TH.forImpD TH.CCall TH.Unsafe modn ffiImportName (return ffiTyp)
      TH.addTopDecls [ffiDec]
      -- invoke ffi
      TH.appsE (TH.varE ffiImportName : map (TH.varE . TH.mkName) [n | (n, _, _) <- miInputs minfo ++ miOutputs minfo])
  , TH.quotePat = const $ fail "inline-verilog: quotePat not implemented (quoteCode)"
  , TH.quoteType = const $ fail "inline-verilog: quoteType not implemented (quoteCode)"
  , TH.quoteDec = const $ fail "inline-verilog: quoteDec not implemented (quoteCode)"
  }

verbatim :: String -> TH.DecsQ
verbatim s = verbatimItem (VFIVerbatim s)

invokeCommand :: String -> [String] -> IO (String, String)
invokeCommand cmd args = do
  (code, stdout, stderr) <- readProcessWithExitCode cmd args ""
  case code of
    ExitSuccess -> return (stdout, stderr)
    ExitFailure fcode -> fail $ unlines
      [ "Command: " ++ (foldl (\a b -> a ++ " " ++ show b) " " (cmd : args))
      , "Exit code: " ++ show fcode
      , "Output:\n" ++ unlines (map (\l -> "  " ++ l) (lines stdout))
      , "Error:\n" ++ unlines (map (\n -> "  " ++ n) (lines stderr))
      ]

compileVerilog :: String -> TH.Q FilePath
compileVerilog src = do
  -- executables we need
  gpp <- fromMaybe "g++" <$> liftIO (lookupEnv "INLINE_C_CPP_COMPILER")
  verilator <- fromMaybe "verilator" <$> liftIO (lookupEnv "INLINE_C_VERILATOR")
  ld <- fromMaybe "ld" <$> liftIO (lookupEnv "INLINE_C_LINKER")
  -- generate verilog source and collect module infos
  mref :: IORef [ModuleInfo] <- liftIO (newIORef [])
  vsource <- fmap unlines $ for (filter (not . all isSpace) (lines src)) $ \l -> do
    case Aeson.eitherDecode (BSL.fromStrict (T.encodeUtf8 (T.pack l))) of
      Left err -> fail $ "Impossible: could not decode item line: " ++ err
      Right fileItem -> case fileItem of
        VFIVerbatim v -> return v
        VFIModule m -> do
          minfo <- moduleInfo m
          liftIO (modifyIORef' mref (minfo :))
          return ("module " ++ mName m ++ " " ++ mBody m)
  minfos <- liftIO (readIORef mref)
  -- compile all sources in the same dir
  mergedOFile <- TH.addTempFile "o"
  liftIO $ withSystemTempDirectory "inline-verilog" $ \tmpDir -> do
    verilatorRoot <- do
      (rootOut, _) <- invokeCommand verilator ["--getenv", "VERILATOR_ROOT"]
      return (dropWhileEnd isSpace (dropWhile isSpace rootOut))
    let svFile = tmpDir ++ "/src.sv"
    writeFile svFile vsource
    modsObjs <- fmap concat $ for minfos $ \minfo -> do
      let oFile = tmpDir ++ "/" ++ miName minfo ++ ".o"
      _ <- invokeCommand verilator ["--sv", svFile, "--top-module", miName minfo, "--cc", "--build", "--Mdir", tmpDir]
      let cppFile = tmpDir ++ "/" ++ miName minfo ++ ".cpp"
      let inputParams = [cTyp ++ " " ++ n | (n, _, cTyp) <- miInputs minfo]
      let outputParams = [cTyp ++ "* " ++ n | (n, _, cTyp) <- miOutputs minfo]
      let fullParams = intercalate ", " (inputParams ++ outputParams)
      let storeInputs = unlines ["top->" ++ n ++ " = " ++ n ++ ";" | (n, _, _) <- miInputs minfo]
      let storeOutputs = unlines ["*" ++ n ++ " = top->" ++ n ++ ";" | (n, _, _) <- miOutputs minfo]
      T.writeFile cppFile $
        T.replace "MNAME" (T.pack (miName minfo)) $
        T.replace "MPARAMS" (T.pack fullParams) $
        T.replace "MINPUTS" (T.pack storeInputs) $
        T.replace "MOUTPUTS" (T.pack storeOutputs) $ T.pack [r|
          #include "VMNAME.h"
          #include "verilated.h"

          extern "C" void MNAME(MPARAMS) {
            VerilatedContext* contextp = new VerilatedContext;
            VMNAME* top = new VMNAME{contextp, "TOP"};
            MINPUTS
            if (!contextp->gotFinish()) {
                top->eval(); // Triggers the initial block
            }
            top->final();
            MOUTPUTS
            delete top;
            delete contextp;
          }
        |]
      let gppArgs = ["-c", "-I", verilatorRoot ++ "/include", "-I", tmpDir, cppFile, "-o", oFile]
      _ <- invokeCommand gpp gppArgs
      return [oFile, tmpDir ++ "/V" ++ (miName minfo) ++ "__ALL.o"]
    let ldArgs = ["-r", "-o", mergedOFile, tmpDir ++ "/verilated.o", tmpDir ++ "/verilated_threads.o"] ++ modsObjs
    void (invokeCommand ld ldArgs)
  return mergedOFile

verilogCtx :: C.Context
verilogCtx = mempty
  { C.ctxForeignSrcLang = Just TH.RawObject
  , C.ctxRawObjectCompile = Just compileVerilog
  }

verilog :: TH.DecsQ 
verilog = C.context verilogCtx

