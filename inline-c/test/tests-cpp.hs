{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
import           Language.C.Inline.Cpp

context cppCtx

include "<iostream>"

main :: IO ()
main = do
  [stmts| void {
      std::cout << "Hello, world!\n";
    } |]
