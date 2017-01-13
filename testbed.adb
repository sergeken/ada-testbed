--                              -*- Mode: Ada -*-
-- Filename        : testbed.adb
-- Description     : Testbed API implementation
-- Author          : Serge Robyns (c) 1998 Robyns Consulting & Services
-- Created On      : Wed Dec 13 19:52:23 2000
-- Last Modified By: .
-- Last Modified On: .
-- Update Count    : 0
-- Status          : Stable

with TestBed.LowLevel;
with Ada.Calendar;

with Ada.Strings.Fixed;

package body TestBed is
   use TestBed.LowLevel;
   use TestBed_IO;
   use Ada.Strings.Fixed;

   Log_File       : File_Type;
   Current_Record : Log_Record;

   procedure Create_Suite (Name : in String;
                           Comment : in String) is
   begin
      if Is_Open(Log_File) then
         raise Bad_Sequence;
      end if;

      Current_Record.Entry_Type := Suite_Header;
      Move(Name, Current_Record.Name);
      Move(Comment, Current_Record.Comment);
      Current_Record.Time_Stamp := Ada.Calendar.Clock;

      Create(Log_File, Name => Name);
      Write(Log_File, Current_Record);
      Flush(Log_File);
   end Create_Suite;


   procedure Open_Suite(Name : in String) is
   begin
      if Is_Open(Log_File) then
         raise Bad_Sequence;
      end if;
      Open(Log_File, Name => Name, Mode => Append_File);
   end Open_Suite;


   procedure Close_Suite is
   begin
      if Is_Open(Log_File) then
         raise Bad_Sequence;
      end if;
      Close(Log_File);
   end Close_Suite;


   procedure Start_Run (Name    : in String;
                        Comment : in String ) is
   begin
      if not Is_Open(Log_File) then
         raise Bad_Sequence;
      end if;

      Current_Record.Entry_Type := Begin_Run;
      Move(Name, Current_Record.Name);
      Move(Comment, Current_Record.Comment);
      Current_Record.Time_Stamp := Ada.Calendar.Clock;

      Write(Log_File, Current_Record);
      Flush(Log_File);
   end Start_Run;


   procedure End_Run (Name      : in String;
                      Succeeded : in Boolean;
                      Comment   : in String ) is
   begin
      if not Is_Open(Log_File) then
         raise Bad_Sequence;
      end if;

      if Succeeded then
         Current_Record.Entry_Type := End_Run_Succeeded;
      else
         Current_Record.Entry_Type := End_Run_Failed;
      end if;
      Move(Name, Current_Record.Name);
      Move(Comment, Current_Record.Comment);
      Current_Record.Time_Stamp := Ada.Calendar.Clock;

      Write(Log_File, Current_Record);
      Flush(Log_File);
   end End_Run;


   procedure Start_Test (Name              : in String;
                         Expected_To_Abort : in Boolean;
                         Comment           : in String ) is
   begin
      if not Is_Open(Log_File) then
         raise Bad_Sequence;
      end if;

      if Expected_To_Abort then
         Current_Record.Entry_Type := Begin_Aborting_Test;
      else
         Current_Record.Entry_Type := Begin_Normal_Test;
      end if;
      Move(Name, Current_Record.Name);
      Move(Comment, Current_Record.Comment);
      Current_Record.Time_Stamp := Ada.Calendar.Clock;

      Write(Log_File, Current_Record);
      Flush(Log_File);
   end Start_Test;


   procedure End_Test (Succeeded : in Boolean;
                       Comment   : in String) is
   begin
      if not Is_Open(Log_File) then
         raise Bad_Sequence;
      end if;
      -- if Current_Record.Type /= Begin_Normal_Test
      --   or Current_Record.Type /= Begin_Aborting_Test then
      -- begin
      --   raise Bad_Sequence;
      -- end if;

      if Succeeded then
         Current_Record.Entry_Type := End_Test_Succeeded;
      else
         Current_Record.Entry_Type := End_Test_Failed;
      end if;
      Move(Comment, Current_Record.Comment);
      Current_Record.Time_Stamp := Ada.Calendar.Clock;

      Write(Log_File, Current_Record);
      Flush(Log_File);
   end End_Test;


   procedure Add_Comment (Comment : in String) is
   begin
      if not Is_Open(Log_File) then
         raise Bad_Sequence;
      end if;

      Current_Record.Entry_Type := TestBed.Lowlevel.Comment;
      Move(Comment, Current_Record.Comment);
      Current_Record.Time_Stamp := Ada.Calendar.Clock;

      Write(Log_File, Current_Record);
      Flush(Log_File);
   end Add_Comment;

end TestBed;
