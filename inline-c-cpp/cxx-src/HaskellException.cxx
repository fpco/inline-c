
#include "HaskellException.hxx"

HaskellException::HaskellException(std::string renderedExceptionIn, void *haskellExceptionStablePtrIn)
  : displayExceptionValue(renderedExceptionIn)
  , haskellExceptionStablePtr(new HaskellStablePtr(haskellExceptionStablePtrIn))
{
}

HaskellException::HaskellException(const HaskellException &other)
  : displayExceptionValue(other.displayExceptionValue)
  , haskellExceptionStablePtr(other.haskellExceptionStablePtr)
{
}

const char* HaskellException::what() const noexcept {
  return displayExceptionValue.c_str();
}
