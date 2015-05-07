{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
import           Data.Monoid ((<>))
import           Foreign.ForeignPtr (withForeignPtr)
import qualified Language.C.Inline.Cpp as C

C.context (C.cppCtx <> C.cppPtrCtx)

C.include "<iostream>"
C.include "<vector>"

main :: IO ()
main = do
  [C.stmts| void {
      std::cout << "Hello, world!\n";
    } |]
  vec <- $(C.new "std::vector<int>")
  withForeignPtr (C.unCppPtr vec) $ \vecPtr ->
    [C.stmts| void {
        std::vector<int> *vec = (std::vector<int> *) $(void *vecPtr);
        std::cout << vec->size() << std::endl;
      } |]
  let printSize =
        [C.exp| void { std::cout << $cpp-ptr:(std::vector<int> *vec)->size() << std::endl } |]
  printSize
  [C.exp| void { $cpp-ptr:(std::vector<int> *vec)->push_back(1) } |]
  printSize
