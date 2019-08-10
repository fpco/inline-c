
#include "HaskellStablePtr.hxx"
#include "HsFFI.h"

HaskellStablePtr::~HaskellStablePtr() {
  hs_free_stable_ptr(stablePtr);
}
