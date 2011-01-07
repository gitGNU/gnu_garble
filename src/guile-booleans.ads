----------------------------------------------------------------------
--                   G U I L E . B O O L E A N S                    --
--                                                                  --
--    Private package for use in implementing boolean functions.    --
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


--  This package is used internally to these bindings.  The users need
--  never worry about converting from standard Ada Booleans to Guile's
--  representation of boolean types in Scheme.

private package Guile.Booleans is

   --  Converts from standard Ada booleans to Scheme booleans
   function To_Scheme (B : Boolean) return SCM;

   --  Converts from Scheme booleans to standard Ada booleans
   function To_Ada (B : SCM) return Boolean;

end Guile.Booleans;
