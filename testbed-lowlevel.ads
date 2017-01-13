--                              -*- Mode: Ada -*-
-- Filename        : testbed-lowlevel.ads
-- Description     : Lowlevel utilities for Testbed
-- Author          : Serge Robyns (c) 1998 Robyns Consulting & Services
-- Created On      : Wed Dec 13 19:33:12 2000
-- Last Modified By: .
-- Last Modified On: .
-- Update Count    : 0
-- Status          : Stable

with Ada.Calendar;
with Ada.Sequential_IO;

package Testbed.Lowlevel is

   Max_Name_Length    : constant := 16;
   Max_Comment_Length : constant := 128;

   subtype Name_Range    is Positive range 1 .. Max_Name_Length;
   subtype Comment_Range is Positive range 1 .. Max_Comment_Length;

   subtype Names    is String(Name_Range);
   subtype Comments is String(Comment_Range);

   type Record_Type is (Suite_Header,
                        Begin_Normal_Test,
                        Begin_Aborting_Test,
                        End_Test_Succeeded,
                        End_Test_Failed,
                        Begin_Run,
                        End_Run_Succeeded,
                        End_Run_Failed,
                        Comment);

   type Log_Record is record
      Entry_Type : Record_Type;
      Time_Stamp : Ada.Calendar.Time;
      Name       : Names;
      Comment    : Comments;
   end record;

   package Testbed_IO is new Ada.Sequential_IO(Log_Record);

   procedure Flush(File: in out Testbed_IO.File_Type);

end Testbed.Lowlevel;
