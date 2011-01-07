----------------------------------------------------------------------
--          G U I L E . N U M B E R S . E X A C T N E S S           --
--                                                                  --
--     Exactness determination and conversion of Guile numbers      --
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


with Guile.Booleans;  use Guile.Booleans;

package body Guile.Numbers.Exactness is

   function Is_Exact (Z : SCM) return Boolean is
      function Internal (X : SCM) return SCM;
      pragma Import (C, Internal, "scm_exact_p");
   begin
      return To_Ada (Internal (Z));
   end Is_Exact;


   function Is_Inexact (Z : SCM) return Boolean is
      function Internal (X : SCM) return SCM;
      pragma Import (C, Internal, "scm_inexact_p");
   begin
      return To_Ada (Internal (Z));
   end Is_Inexact;

end Guile.Numbers.Exactness;
