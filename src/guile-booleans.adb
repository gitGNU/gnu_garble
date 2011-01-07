----------------------------------------------------------------------
--                   G U I L E . B O O L E A N S                    --
--                                                                  --
--    Private package for use in implementing boolean functions.    --
--                                                                  --
--                             B o d y                              --
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

with Interfaces.C;
with Interfaces;   use Interfaces;      --  for bit twiddling

package body Guile.Booleans is

   use type C.Int;

   --  The C API command `scm_from_bool' is actually a macro with the
   --  following definition:
   --
   --    #define scm_from_bool(x) ((x) ? SCM_BOOL_T : SCM_BOOL_F)
   --
   --  Besides the use of the blasphemous ternary operator, we now
   --  have two more macros, `SCM_BOOL_T' and `SCM_BOOL_F'.  Their
   --  definitions are:
   --
   --    #define SCM_BOOL_T SCM_MAKIFLAG(4)
   --    #define SCM_BOOL_F SCM_MAKIFLAG(0)
   --
   --  Yay!  More macro indirection.  `SCM_MAKIFLAG' definition:
   --
   --    #define SCM_MAKIFLAG(n) SCM_MAKE_ITAG8 ((n), scm_tc8_flag)
   --
   --  ... and `SCM_MAKE_ITAG8':
   --
   --    #define SCM_MAKE_ITAG8(X, TAG) SCM_PACK (((X) << 8) + TAG)
   --
   --  Luckily we have SCM_PACK defined in GARBLE as Guile.Pack.  But
   --  we aren't quite done yet.
   --
   --  scm_tc8_flag is part of the enum `scm_tc8_tags' and assigned
   --  the value `scm_tc3_imm24 + 0x00'.  It turns out that
   --  `scm_tc3_imm24' is yet another macro used as a constant for the
   --  value 4.
   --
   --  With that, we can now re-implement this entire string of
   --  indirections in Ada, in a clean, readable way.

   function To_Scheme (B : Boolean) return SCM is
      SCM_True  : constant SCM :=
        Pack (SCM_Bits (Shift_Left (4, 8) + Unsigned_64'(4)));
      SCM_False : constant SCM :=
        Pack (SCM_Bits (Shift_Left (0, 8) + Unsigned_64'(4)));
   begin
      if B then
         return SCM_True;
      else
         return SCM_False;
      end if;
   end To_Scheme;

   function To_Ada (B : SCM) return Boolean is

      function Internal (Val : SCM) return C.Int;
      pragma Import (C, Internal, "scm_to_bool");

   begin
      if Internal (B) = 0 then
         return False;
      else
         return True;
      end if;
   end To_Ada;

end Guile.Booleans;
