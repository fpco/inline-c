
#include "HaskellException.hxx"

HaskellException::HaskellException(std::string renderedExceptionIn, void *haskellExceptionStablePtrIn)
  : haskellExceptionStablePtr(new HaskellStablePtr(haskellExceptionStablePtrIn))
  , displayExceptionValue(renderedExceptionIn)
{
}

HaskellException::HaskellException(const HaskellException &other)
  : haskellExceptionStablePtr(other.haskellExceptionStablePtr)
  , displayExceptionValue(other.displayExceptionValue)
{
}

const char* HaskellException::what() const noexcept {
  return displayExceptionValue.c_str();
}
