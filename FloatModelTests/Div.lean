/-
Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Julia M. Himmel
-/
module

import FloatModel
meta import FloatModel

open FloatModel

def a := Float.ofBits (UInt64.ofNat 0xb68ffff8000000ff)
def b := Float.ofBits (UInt64.ofNat 0x3f9080000007ffff)

#eval UnpackedFloat.ofFloat a
#eval UnpackedFloat.ofFloat b

#eval UnpackedFloat.div FloatSpec.binary64 (UnpackedFloat.ofFloat a) (UnpackedFloat.ofFloat b)
