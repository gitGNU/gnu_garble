----------------------------------------------------------------------
--                       G U I L E . I N I T                        --
--                                                                  --
-- Initialization functions of the Guile extension language library --
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
--  Each thread (task?) that wants to use functions from the GARBLE
--  API needs to put itself into Guile mode by either instantiating
--  the With_Guile package and calling Call, e.g.
--
--  declare
--     type My_Data is ...;
--     type My_Return is ...;
--     function Some_Func (D : My_Data) return My_Return;
--
--     package Use_Guile is new With_Guile (My_Data, My_Return);
--
--     My_D : My_Data := ...;
--     My_R : My_Return;
--  begin
--     My_R := With_Guile.Call (Some_Func, My_D);
--  end;
--
--  or, alternatively, calling Init.  The global state of Guile is
--  initialized automatically when the first thread enters Guile
--  mode.
--
--  When a thread wants to block outside of a GARBLE API function, it
--  should leave Guile mode temporarily with ***Guile.Blocking???***
--
--  *** What's this call-with-new-thread and scm_spawn_thread stuff?
--  </description>
--  <c_version>1.9.11</c_version>



package Guile.Init is

   generic
      type Data_Type   is (<>);
      type Return_Type is (<>);
   package With_Guile is

      type Caller_Func is
        access function (Data : Data_Type) return Return_Type;
      pragma Convention (C, Caller_Func);

      type Caller_Proc is
        access procedure (Data : Data_Type);
      pragma Convention (C, Caller_Proc);

      function Call (Func : Caller_Func;
                     Data : Data_Type) return Return_Type;

      procedure Boot (Proc : Caller_Proc; Data : Data_Type);
   private
      pragma Import (C, Call, "scm_with_guile");
   end With_Guile;

   procedure Init;
   procedure Shell;


private
   pragma Import (C, Init, "scm_init_guile");

   --  No binding: scm_boot_guile
   --    Would put the burden on the user of this binding to handle
   --    C-style (int argc, char **argv)-style command-line options
end Guile.Init;
