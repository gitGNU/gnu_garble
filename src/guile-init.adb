----------------------------------------------------------------------
--                        G U I L E . I N I T                       --
--                                                                  --
--                              B o d y                             --
--                                                                  --
--   This file is part of GARBLE, the Guile/Ada Rich Binding for    --
--     Language Extension, a binding for Ada 2005 to Guile, the     --
--        GNU Ubiquitous Intelligent Language for Extension.        --
--                                                                  --
--                  Copyright © 2009 Atom X Zane                    --
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

with System;
with Interfaces.C;

package body Guile.Init is

   package C renames Interfaces.C;

   --  C-style command line arguments, set from internal GNAT values
   Gnat_Argc : C.int;
   pragma Import (C, Gnat_Argc);
   Gnat_Argv : System.Address;
   pragma Import (C, Gnat_Argv);

   package body With_Guile is

      procedure Boot (Proc : Caller_Proc;
                      Data : Data_Type) is

         type Wrapper_Proc is
           access procedure (Data : Data_Type;
                             Num  : C.Int;
                             Args : System.Address);
         pragma Convention (C, Wrapper_Proc);

         procedure Wrapper (D    : Data_Type;
                            Argc : C.Int;
                            Argv : System.Address);
         pragma Convention (C, Wrapper);

         procedure Wrapper (D    : Data_Type;
                            Argc : C.Int;
                            Argv : System.Address) is
         begin
            --  Call Proc before anything else
            Proc (D);
            --  Purposefully not referencing Argc or Argv.
            --  Should not generate warnings.
         end Wrapper;

         procedure Internal (Argc : C.Int;
                             Argv : System.Address;
                             Proc : Wrapper_Proc;
                             Data : Data_Type);
         pragma Import (C, Internal, "scm_boot_guile");
      begin
         Internal (Gnat_Argc, Gnat_Argv, Wrapper'Access, Data);
      end Boot;

   end With_Guile;

   procedure Shell is

      procedure Internal (Argc : C.Int;
                          Argv : System.Address);
      pragma Import (C, Internal, "scm_shell");

   begin
      Internal (Gnat_Argc, Gnat_Argv);
   end Shell;

end Guile.Init;
