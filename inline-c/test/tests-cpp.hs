{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
import           Language.C.Inline.Cpp

setContext cppCtx

include "<iostream>"

main :: IO ()
main = do
  [c| void {
      std::cout << "Hello, world!\n";
    } |]
