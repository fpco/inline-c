
#include "HaskellException.hxx"

HaskellException::HaskellException(std::string renderedExceptionIn, void *haskellExceptionStablePtrIn)
  : renderedException(renderedExceptionIn)
  , haskellExceptionStablePtr(new HaskellStablePtr(haskellExceptionStablePtrIn))
{
}

HaskellException::HaskellException(const HaskellException &other)
  : renderedException(other.renderedException)
  , haskellExceptionStablePtr(other.haskellExceptionStablePtr)
{
}

const char* HaskellException::what() const noexcept {
  return renderedException.c_str();
}
