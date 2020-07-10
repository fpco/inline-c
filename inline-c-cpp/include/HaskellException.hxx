
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
class HaskellException : public std::exception {
public:
  std::shared_ptr<HaskellStablePtr> haskellExceptionStablePtr;
  std::string displayExceptionValue;

  HaskellException(std::string displayExceptionValue, void *haskellExceptionStablePtr);
  HaskellException(const HaskellException &);
  virtual const char* what() const noexcept override;

};

void setMessageOfStdException(std::exception &e,char** __inline_c_cpp_error_message__);
void setMessageOfOtherException(char** __inline_c_cpp_error_message__);
