----------------------------------------------------------------------
--           G U I L E . N U M B E R S . I N T E G E R S            --
--                                                                  --
--                  Interfacing to Guile integers                   --
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
with Config; use Config;

package body Guile.Numbers.Integers is

   package C renames Interfaces.C;

   use type C.Int;

   function Is_Integer (X : SCM) return Boolean is

      function Internal (X : SCM) return C.Int;
      pragma Import (C, Internal, "scm_is_integer");

   begin

      if Internal (X) = 0 then
         return False;
      else
         return True;
      end if;

   end Is_Integer;

   package body Convert_Signed is

      function To_Ada (Val : SCM) return Int is

         function Internal (X   : SCM;
                            Min : Int;
                            Max : Int) return Int;
         pragma Import (C, Internal, "scm_to_signed_integer");

      begin
         return Internal (Val, Int'First, Int'Last);
      end To_Ada;

      function To_Scheme (I : Int) return SCM is

         function Internal (X : Int) return SCM;
         pragma Import (C, Internal, "scm_from_signed_integer");

      begin
         return Internal (I);
      end To_Scheme;
   end Convert_Signed;


   --  **FIXME: Atom X** The following functions are likely _not_
   --  portable; GNAT defines these types for the x86-family
   --  platforms.  What about other architectures?
   --
   --  The ARM guarantees only that the type Integer must include the
   --  range -2**15 + 1 .. 2**15 - 1, and _if an implementation
   --  provides it_, the type Long_Integer must include the range
   --  -2**31 + 1 .. 2**31 - 1.
   --
   --  The GNAT Reference Manual section 4.13 has these definitions:
   --  Short_Short_Integer :  8 bit signed
   --  Short_Integer       : 16 bit signed
   --  Integer             : 32 bit signed
   --  Long_Integer        : 64 bit signed (Alpha VMS only)
   --                        32 bit signed (all other targets)
   --  Long_Long_Integer   : 64 bit signed
   --
   --  A more straight-forward conversion would make use of the
   --  integer types defined in Interfaces, but we want this to be
   --  an _Ada_ binding.
   function To_Ada (Val : SCM) return Short_Short_Integer is

      function Internal (Val : SCM) return Short_Short_Integer;
      --  GNAT defines Short_Short_Integer as 8-bits long
      pragma Import (C, Internal, "scm_to_int8");

   begin
      return Internal (Val);
   end To_Ada;

   function To_Ada (Val : SCM) return Short_Integer is

      function Internal (Val : SCM) return Short_Integer;
      --  GNAT defines Short_Integer as 16-bits long
      pragma Import (C, Internal, "scm_to_int16");

   begin
      return Internal (Val);
   end To_Ada;

   function To_Ada (Val : SCM) return Integer is

      function Internal (Val : SCM) return Integer;
      --  GNAT defines the Integer type as 32-bits long
      pragma Import (C, Internal, "scm_to_int32");
   begin
      return Internal (Val);
   end To_Ada;

   function To_Ada (Val : SCM) return Long_Integer is
      Result : Long_Integer;
   begin
      --  We use a special test, located in Config, for Alphas
      if Running_Alpha then
         declare
            --  On Alpha VMS, GNAT defines Long_Integer as 64-bits
            function Internal (X : SCM) return Long_Integer;
            pragma Import (C, Internal, "scm_to_int64");
         begin
            Result := Internal (Val);
         end;
      else
         declare
            --  On all other platforms, Long_Integer is 32-bits
            function Internal (X : SCM) return Long_Integer;
            pragma Import (C, Internal, "scm_to_int32");
         begin
            Result := Internal (Val);
         end;
      end if;

      return Result;
   end To_Ada;

   function To_Ada (Val : SCM) return Long_Long_Integer is

      function Internal (Val : SCM) return Long_Long_Integer;
      --  GNAT defines Long_Long_Integer as 64-bits long
      pragma Import (C, Internal, "scm_to_int64");

   begin
      return Internal (Val);
   end To_Ada;

end Guile.Numbers.Integers;
