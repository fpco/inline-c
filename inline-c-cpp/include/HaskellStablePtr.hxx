
#pragma once

#include "HsFFI.h"

#ifndef STABLE_PTR_NULL
#define STABLE_PTR_NULL (static_cast<HsStablePtr>((void *)0))
#endif

/* This is like a newtype that adds a C++ destructor, allowing C++ to call
   hs_free_stable_ptr when the lifetime ends.

   If you need to pass HaskellStablePtr around, you need to use something like
   std::shared_ptr<HaskellStablePtr> to avoid copying the HaskellStablePtr.

   WARNING: If you copy HaskellStablePtr, you must call the original.setNull()
            method in order to prevent a premature/double free when original
            goes out of scope. This does make the original object invalid.
 */
struct HaskellStablePtr {
  HsStablePtr stablePtr;

  /* Takes ownership of a stable pointer. */
  inline HaskellStablePtr(HsStablePtr s) {
    stablePtr = s;
  }

  /* Calls hs_free_stable_ptr if this.stablePtr is not NULL. */
  ~HaskellStablePtr();

  inline void setNull() {
    stablePtr = STABLE_PTR_NULL;
  }

  operator bool() const {
    return stablePtr != STABLE_PTR_NULL;
  }
};
