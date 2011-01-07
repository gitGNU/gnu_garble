----------------------------------------------------------------------
--                             G U I L E                            --
--                                                                  --
--              Root package for the Guile hierarchy.               --
--                                                                  --
--                              B o d y                             --
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

package body Guile is

   --  The Pack and Unpack functions as defined by the Guile library
   --  are actually macros implementing pointer-to-integer conversion
   --  from SCM types (a pointer to a struct with a single char as its
   --  only member) to scm_t_bits types (which are actually uintptr_t
   --  types, integers guaranteed to be the same size as pointers).
   --
   --  The mechanism by which this conversion takes place, and what it
   --  actually means for our code, should not be our concern.  Thus,
   --  we will take advantages of the facilities provided by the
   --  System package and simply do address-to-integer conversions and
   --  vice-versa in an attempt to avoid the needfully complex ways
   --  that C programmers implement type-checking.

   function Pack (X : SCM_Bits) return SCM is
   begin
      --  Put X in a SCM record
      return SCM (To_Address (X));
   end Pack;

   function Unpack (X : SCM) return SCM_Bits is
   begin
      --  Pull X from its structure
      return SCM_Bits (To_Integer (S.Address (X)));
   end Unpack;

end Guile;
