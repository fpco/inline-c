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

/* mallocs a string representing the exception type name or error condition.

   Ideally, this returns a demangled string, but it may degrade to
    - a mangled string if demangling fails,
    - "<unknown exception>" if exception type info is not available,
    - "<no exception>" if no current exception is found.

   The responsibility for freeing the returned string falls on the caller,
   such as handleForeignCatch, which passes the responsibility on to ByteString

 */
#if defined(__GNUC__) || defined(__clang__)
const char* currentExceptionTypeName()
{
  std::type_info *type_info = abi::__cxa_current_exception_type();
  if (!type_info)
    return strdup("<no exception>");

  const char *raw_name = type_info->name();
  if (!raw_name)
    return strdup("<unknown exception>");

  int demangle_status;
  const char *demangled_name = abi::__cxa_demangle(raw_name, 0, 0, &demangle_status);
  if (!demangled_name)
    return strdup(raw_name);

  return demangled_name;
}
#endif

/* Set the message and type strings.

   The responsibility for freeing the returned string falls on the caller,
   such as handleForeignCatch, which passes the responsibility on to a
   ByteString.
 */
void setMessageOfStdException(const std::exception &e, const char** msgStrPtr, const char **typStrPtr){
  *msgStrPtr = strdup(e.what());
  setCppExceptionType(typStrPtr);
}

void setCppExceptionType(const char** typStrPtr){
#if defined(__GNUC__) || defined(__clang__)
  *typStrPtr = currentExceptionTypeName();
#else
  *typStrPtr = NULL;
#endif
}
