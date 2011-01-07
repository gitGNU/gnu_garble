----------------------------------------------------------------------
--                        T E S T _ I N I T                         --
--                                                                  --
--   Testing of initialization of Guile extension language library  --
--                                                                  --
--                        P r o c e d u r e                         --
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


with Guile.Init;
with Ada.Text_IO;  use Ada.Text_IO;

procedure Test_Init is

   --  This function will be passed to Call
   --  from the With_Guile package
   function Call_With_Guile (Input : Integer) return Boolean;
   pragma Convention (C, Call_With_Guile);

   function Call_With_Guile (Input : Integer) return Boolean is
   begin
      Put_Line ("This function called with Guile");
      return True;
   end Call_With_Guile;

   package Test_With_Guile is new
     Guile.Init.With_Guile (Data_Type   => Integer,
                            Return_Type => Boolean);
   pragma Warnings (Off, Test_With_Guile);

   Called : Boolean;

begin

   --  Test the With_Guile package
   Called := Test_With_Guile.Call (Call_With_Guile'Access, 0);
   pragma Assert (Called, "Call_With_Guile failed.");

   --  Initialize Guile
   Guile.Init.Init;

   --  Nothing special at the moment.  Just start a shell.
   Guile.Init.Shell;

end Test_Init;
