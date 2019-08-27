
#pragma once

#include "HaskellStablePtr.hxx"
#include <memory>
#include <string>

class HaskellException : public std::exception {
public:
  std::shared_ptr<HaskellStablePtr> haskellExceptionStablePtr;
  std::string displayExceptionValue;

  HaskellException(std::string displayExceptionValue, void *haskellExceptionStablePtr);
  HaskellException(const HaskellException &);
  virtual const char* what() const noexcept override;

};
