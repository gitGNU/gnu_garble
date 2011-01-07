----------------------------------------------------------------------
--          G U I L E . N U M B E R S . E X A C T N E S S           --
--                                                                  --
--     Exactness determination and conversion of Guile numbers      --
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
--  Scheme has the concept of ``exactness'', that is, whether a given
--  number can be expressed without fractional error.  The distincion
--  between an exact number, the integer `5', and an inexact number,
--  the real `5.0', which only has no fractional part up to the limits
--  of the precision provided by a given machine architecture.  Guile
--  will only convert from `5.0' to `5' when forced to do so by an
--  invocation of `inexact->exact'; any operations that mixe exact
--  operands with inexact operands will always yield an inexact value;
--  if both operands are exact, the result will be exact.
--  </description

package Guile.Numbers.Exactness is

   function Is_Exact (Z : SCM) return Boolean;
   --  Returns True if the number Z is exact, False otherwise
   --
   --    (exact? 2)
   --    => #t
   --
   --    (exact? 0.5)
   --    => #f
   --
   --    (exact? (/ 2))
   --    => #t

   function Is_Inexact (Z : SCM) return Boolean;
   --  Returns True if the number Z is inexact, False otherwise

   function Inexact_To_Exact (Z : SCM) return SCM;
   --  Returns an exact number that is numerically closest to Z, when
   --  there is one.  For inexact rationals, Guile returns the exact
   --  rational that is numerically equal to the inexact rational.
   --  Inexact complex numbers with a non-zero imaginary part cannot
   --  be made exact

   function Exact_To_Inexact (Z : SCM) return SCM;
   --  Returns the inexact representation of Z

private

   pragma Import (C, Inexact_To_Exact, "scm_inexact_to_exact");
   pragma Import (C, Exact_To_Inexact, "scm_exact_to_inexact");

end Guile.Numbers.Exactness;
