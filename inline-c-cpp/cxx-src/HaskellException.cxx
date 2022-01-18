#include "HaskellException.hxx"

// see
// <https://stackoverflow.com/questions/28166565/detect-gcc-as-opposed-to-msvc-clang-with-macro>
// regarding how to detect g++ or clang.
// 
// the defined(__clang__) should actually be redundant, since apparently it also
// defines GNUC, but but let's be safe.

#include <cstring>
#include <cstdlib>

#if defined(__GNUC__) || defined(__clang__)
#include <cxxabi.h>
#include <string>
#endif


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

#ifdef	__APPLE__
HaskellException::~HaskellException() _NOEXCEPT
{
  haskellExceptionStablePtr.reset();
}

const char* HaskellException::what() const _NOEXCEPT {
#else
const char* HaskellException::what() const noexcept {
#endif
  return displayExceptionValue.c_str();
}


// see
// <https://stackoverflow.com/questions/561997/determining-exception-type-after-the-exception-is-caught/47164539#47164539>
// regarding how to show the type of an exception.

#if defined(__GNUC__) || defined(__clang__)
const char* currentExceptionTypeName()
{
  int demangle_status;
  return abi::__cxa_demangle(abi::__cxa_current_exception_type()->name(), 0, 0, &demangle_status);
}
#endif

void setMessageOfStdException(const std::exception &e, char** msgStrPtr, char **typStrPtr){
  *msgStrPtr = strdup(e.what());
  setCppExceptionType(typStrPtr);
}

void setCppExceptionType(char** typStrPtr){
#if defined(__GNUC__) || defined(__clang__)
  const char* message = currentExceptionTypeName();
  size_t message_len = strlen(message) + 1;
  *typStrPtr = static_cast<char*>(std::malloc(message_len));
  std::memcpy(*typStrPtr, message, message_len);
#else
  *typStrPtr = NULL;
#endif
}
