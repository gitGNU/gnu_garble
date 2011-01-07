----------------------------------------------------------------------
--                    G U I L E . N U M B E R S                     --
--                                                                  --
--  Parent package for interacting with Guile's numerical tower.    --
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
--  The Guile numerical tower is rich and powerful, capable of
--  representing and manipulating numbers of any size, integer or
--  floating point, real or complex, rational or irrational, exactly
--  or inexactly.
--
--  Integers in Guile have no limit in size.  Real numbers can be
--  represented with the same precision possible from an Ada Long
--  Long Float.  Complex numbers have Long Long Float precision for
--  both the Real and Imaginary parts.
--  </description>
--  <c_version>1.9.11</c_version>


package Guile.Numbers is

   pragma Pure (Guile.Numbers);

   function Is_Number (Obj : SCM) return Boolean;
   --  Return True if Obj is any kind of Scheme number, else False

end Guile.Numbers;
