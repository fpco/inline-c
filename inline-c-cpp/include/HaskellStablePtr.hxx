
#pragma once

struct HaskellStablePtr {
  void *stablePtr;

  /* Takes ownership of a stable pointer */
  inline HaskellStablePtr(void *s) {
    stablePtr = s;
  }
  /* Calls hs_free_stable_ptr */
  ~HaskellStablePtr();
};
