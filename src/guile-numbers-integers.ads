----------------------------------------------------------------------
--           G U I L E . N U M B E R S . I N T E G E R S            --
--                                                                  --
--                  Interfacing to Guile integers                   --
--                                                                  --
--                             S p e c                              --
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
--  Due to Guile's nature as a C library, it handles integer values in
--  a very C-centric manner.  This is not to say that Guile is limited
--  by C's number system -- far from it.  Integers within Guile's
--  Scheme processing engine can be arbitrarily large, as in the
--  following example from the Guile documentation:
--
--  (define factorial
--    (lambda (n)
--      (let loop ((n n) (product 1))
--        (if (= n 0)
--            product
--            (loop (- n 1) (* product n))))))
--
--  (factorial 3)
--  => 6
--
--  (factorial 20)
--  => 2432902008176640000
--
--  (- (factorial 45))
--  => -119622220865480194561963161495657715064383733760000000000
--
--  Thus dealing with integers in GARBLE will be limited by Ada's
--  number system, not with Guile's.
--  </description>
--  <c_version>1.9.11</c_version>


package Guile.Numbers.Integers is

   function Is_Integer (X : SCM) return Boolean;
   --  Returns True if X is an integer, False otherwise

   generic
      type Int is range <>;
   package Convert_Signed is
      --  Package for converting signed integer types to/from Guile's
      --  SCM representations.

      function To_Ada (Val : SCM) return Int;
      --  Converts a SCM value to an Int;

      function To_Scheme (I : Int) return SCM;
      --  Converts an Int value to a SCM representation
   end Convert_Signed;

   --  The Guile library provides a series of predefined conversion
   --  functions for the various integer types.  We will take
   --  advantage of any performance improvements for predefined Ada
   --  integer types.

   function To_Ada (Val : SCM) return Short_Short_Integer;
   function To_Ada (Val : SCM) return Short_Integer;
   function To_Ada (Val : SCM) return Integer;
   function To_Ada (Val : SCM) return Long_Integer;
   function To_Ada (Val : SCM) return Long_Long_Integer;

end Guile.Numbers.Integers;
