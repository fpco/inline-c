{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
import qualified Language.C.Inline.Cpp as C
import           Foreign.ForeignPtr (withForeignPtr)

C.context C.cppCtx

C.include "<iostream>"
C.include "<vector>"


main :: IO ()
main = do
  [C.stmts| void {
      std::cout << "Hello, world!\n";
    } |]
  vec <- $(C.new "std::vector<int>")
  let vecFPtr = C.unCppPtr vec
  withForeignPtr vecFPtr $ \vecPtr ->
    [C.stmts| void {
        std::vector<int> *vec = (std::vector<int> *) $(void *vecPtr);
        std::cout << vec->size();
      } |]
