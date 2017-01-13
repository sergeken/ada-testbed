--                              -*- Mode: Ada -*-
-- Filename        : testbed-lowlevel.adb
-- Description     : Some LowLevel implementations
-- Author          : Serge Robyns (c) 1998 Robyns Consulting & Services
-- Created On      : Wed Dec 13 22:03:57 2000
-- Last Modified By: .
-- Last Modified On: .
-- Update Count    : 0
-- Status          : Stable

with Ada.Unchecked_Conversion;
with System.File_IO;
with System.File_Control_Block;

package body TestBed.Lowlevel is

   function To_Ap is new
     Ada.Unchecked_Conversion (Testbed_IO.File_Type,
                               System.File_Control_Block.Afcb_Ptr);

   procedure Flush (File : in out Testbed_IO.File_Type ) is
   begin
      System.File_IO.Flush(To_Ap(File));
   end Flush;

end TestBed.Lowlevel;
