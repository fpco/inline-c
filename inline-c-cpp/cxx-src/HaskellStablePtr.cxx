
#include "HaskellStablePtr.hxx"

HaskellStablePtr::~HaskellStablePtr() {
  if (stablePtr != STABLE_PTR_NULL) {
    hs_free_stable_ptr(stablePtr);
  }
}
