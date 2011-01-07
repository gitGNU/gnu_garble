----------------------------------------------------------------------
--              G U I L E . N U M B E R S . R E A L S               --
--                                                                  --
--                    Interfacing to Guile reals                    --
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
with Guile.Booleans;         use Guile.Booleans;
with Guile.Numbers.Integers; use Guile.Numbers.Integers;

package body Guile.Numbers.Reals is

   package C renames Interfaces.C;

   use type C.Int;

   --  function To_Ada (Val : SCM) return Long_Long_Float is

   --     function Internal (Val : SCM) return Long_Long_Float;
   --     pragma Import (C, Internal, "scm_to_double");

   --  begin
   --     return Internal (Val);
   --  end To_Ada;

   --  function To_Scheme (Val : Long_Long_Float) return SCM is

   --     function Internal (Val : Long_Long_Float) return SCM;
   --     pragma Import (C, Internal, "scm_from_double");

   --  begin
   --     return Internal (Val);
   --  end To_Scheme;



   function Is_Real (X : SCM) return Boolean is
      function Internal (X : SCM) return C.Int;
      pragma Import (C, Internal, "scm_is_real");
   begin
      if Internal (X) = 0 then
         return False;
      else
         return True;
      end if;
   end Is_Real;

   function Is_Rational (X : SCM) return Boolean is
      function Internal (X : SCM) return C.Int;
      pragma Import (C, Internal, "scm_is_rational");
   begin
      if Internal (X) = 0 then
         return False;
      else
         return True;
      end if;
   end Is_Rational;

   function Is_Infinite (X : SCM) return Boolean is
      function Internal (X : SCM) return SCM;
      pragma Import (C, Internal, "scm_inf_p");
   begin
      return To_Ada (Internal (X));
   end Is_Infinite;

   function Is_Not_A_Number (X : SCM) return Boolean is
      function Internal (X : SCM) return SCM;
      pragma Import (C, Internal, "scm_nan_p");
   begin
      return To_Ada (Internal (X));
   end Is_Not_A_Number;



   function Numerator (X : SCM) return SCM is
      function Internal (X : SCM) return SCM;
      pragma Import (C, Internal, "scm_numerator");
   begin
      return Internal (X);
   end Numerator;

   function Denominator (X : SCM) return SCM is
      function Internal (X : SCM) return SCM;
      pragma Import (C, Internal, "scm_denominator");
   begin
      return Internal (X);
   end Denominator;


   function Numerator (X : SCM) return Long_Long_Integer is
      T : SCM := Numerator (X);
   begin
      return To_Ada (T);
   end Numerator;

   function Denominator (X : SCM) return Long_Long_Integer is
      T : SCM := Denominator (X);
   begin
      return To_Ada (T);
   end Denominator;

end Guile.Numbers.Reals;
