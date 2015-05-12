{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
import qualified Language.C.Inline.Cpp as C

C.context C.cppCtx

C.include "<iostream>"

main :: IO ()
main = do
  let x = 3
  [C.stmts| void {
      std::cout << "Hello, world!" << $(int x) << std::endl;
    } |]
