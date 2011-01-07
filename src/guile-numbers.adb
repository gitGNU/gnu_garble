----------------------------------------------------------------------
--                    G U I L E . N U M B E R S                     --
--                                                                  --
--  Parent package for interacting with Guile's numerical tower.    --
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

package body Guile.Numbers is

   package C renames Interfaces.C;

   function Is_Number (Obj : SCM) return Boolean is

      use type C.Int;

      function Internal (Obj : SCM) return C.Int;
      pragma Import (C, Internal, "scm_is_number");

   begin
      if Internal (Obj) = 0 then
         return False;
      else
         return True;
      end if;
   end Is_Number;

end Guile.Numbers;
