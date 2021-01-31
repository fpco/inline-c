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

void setMessageOfStdException(std::exception &e,char** __inline_c_cpp_error_message__){
#if defined(__GNUC__) || defined(__clang__)
  const char* demangle_result = currentExceptionTypeName();
  std::string message = "Exception: " + std::string(e.what()) + "; type: " + std::string(demangle_result);
#else
  std::string message = "Exception: " + std::string(e.what()) + "; type: not available (please use g++ or clang)";
#endif
  size_t message_len = message.size() + 1;
  *__inline_c_cpp_error_message__ = static_cast<char*>(std::malloc(message_len));
  std::memcpy(*__inline_c_cpp_error_message__, message.c_str(), message_len);
}

void setMessageOfOtherException(char** __inline_c_cpp_error_message__){
#if defined(__GNUC__) || defined(__clang__)
  const char* message = currentExceptionTypeName();
  size_t message_len = strlen(message) + 1;
  *__inline_c_cpp_error_message__ = static_cast<char*>(std::malloc(message_len));
  std::memcpy(*__inline_c_cpp_error_message__, message, message_len);
#else
  *__inline_c_cpp_error_message__ = NULL;
#endif
}
