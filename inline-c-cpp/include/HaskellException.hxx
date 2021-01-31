
#pragma once

#include "HaskellStablePtr.hxx"
#include <memory>
#include <string>
#include <exception>

/* A representation of a Haskell exception (SomeException), with a precomputed
   exception message from Control.Exception.displayException.

   The std::exception requires that retrieving the message does not mutate the
   exception object and does not throw exceptions.

   This class uses std::shared_ptr for the exception, because its callers can
   not know in advance where and how often the exception will be copied, or when
   it is released.
 */
#ifdef	__APPLE__
class HaskellException : public std::exception {
#else
class HaskellException : public std::exception {
#endif
public:
  std::shared_ptr<HaskellStablePtr> haskellExceptionStablePtr;
  std::string displayExceptionValue;

  HaskellException(std::string displayExceptionValue, void *haskellExceptionStablePtr);
  HaskellException(const HaskellException &);
#ifdef	__APPLE__
  virtual const char* what() const _NOEXCEPT override;
  virtual ~HaskellException() _NOEXCEPT;
#else
  virtual const char* what() const noexcept override;
#endif

};

void setMessageOfStdException(std::exception &e,char** __inline_c_cpp_error_message__);
void setMessageOfOtherException(char** __inline_c_cpp_error_message__);
