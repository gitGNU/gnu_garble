----------------------------------------------------------------------
--                             G U I L E                            --
--                                                                  --
--              Root package for the Guile hierarchy.               --
--                                                                  --
--                              S p e c                             --
--                                                                  --
--   This file is part of GARBLE, the Guile/Ada Rich Binding for    --
--     Language Extension, a binding for Ada 2005 to Guile, the     --
--        GNU Ubiquitous Intelligent Language for Extension.        --
--                                                                  --
--                    Copyright © 2010 Atom X                       --
--                                                                  --
-- This library is free software; you can redistribute it and/or    --
-- modify it under the terms of the GNU General Public License as   --
-- published by the Free Software Foundation, version 3 of the      --
-- License, or (at your option) any later version.                  --
--                                                                  --
-- This program is distributed in the hope that it will be useful,  --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of   --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU --
-- General Public License for more details.                         --
--                                                                  --
-- You should have received a copy of the GNU General Public        --
-- License along with this library. If not, see                     --
-- <http://www.gnu.org/licenses/>                                   --
----------------------------------------------------------------------


--  <description>
--  This is the root package for the Ada 2005 binding of Guile,
--  the GNU Ubiquitous Intelligent Language for Extension.
--
--  This package defines the standard SCM type, Guile-for-Ada's base
--  type, capable of holding any Scheme value. It also defines the
--  low-level SCM_Bits type, used internally by the Guile C
--  library underlying Guile-for-Ada. Generally this will not be used
--  by client programs, though for some smob extensions it may be
--  required.
--
--  This package also defines the functions Pack and Unpack, to
--  convert between SCM and SCM_Bits.
--  </description>
--  <c_version>1.9.11</c_version>

with System;
with System.Storage_Elements;

package Guile is

   pragma Pure (Guile);

   type SCM is private;
   --  SCM is the user level type designed to hold all types of Scheme
   --  values.  The only non-Guile operation guaranteed to work on SCM
   --  objects is assignment, so any true manipulation of SCM values
   --  should only be done through this library.  SCM <-> Ada-types
   --  conversion is done through the facilities of this library.

   type SCM_Bits is private;
   --  This type corresponds to the Guile-internal type scm_t_bits.
   --  The only time it is really used outside of the library itself
   --  is when implementing certain types of Guile extensions


   function Pack   (X : SCM_Bits) return SCM;
   --  Converts a low-level SCM_Bits to SCM for program use

   function Unpack (X : SCM)      return SCM_Bits;
   --  Converts a high-level SCM to SCM_Bits for low-level
   --  operations such as defining SMOBs

private

   package S renames System;

   --  So internally, Guile defines the type scm_t_bits as a standard
   --  uintptr_t, which is just and integer large enough to hold the
   --  value of a pointer.  The type SCM, on the other hand, is by
   --  default defined as a pointer to a struct with a single char
   --  member.  I'm not nearly familiar enough with C to be able to
   --  explain why the Guile library is defined like this from the
   --  beginning, but from the developers' comments its to compensate
   --  for C's non-existent type checking somehow.
   --
   --  Anyway, we'll ignore all of that and just try to define SCM and
   --  SCM_Bits as types that match the the Guile library types for
   --  size and shape.

   type SCM_Bits is new S.Storage_Elements.Integer_Address;
   type SCM      is new S.Address;

end Guile;
