--                              -*- Mode: Ada -*-
-- Filename        : testrun.adb
-- Description     : TestFile management utility
-- Author          : Serge Robyns
-- Created On      : Wed Dec 13 22:48:18 2000
-- Last Modified By: .
-- Last Modified On: .
-- Update Count    : 0
-- Status          : Unknown, Use with caution!


with Testbed, Testbed.Lowlevel;
use Testbed, Testbed.Lowlevel;
use Testbed.Lowlevel.Testbed_Io;

with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_Io; use Ada.Text_Io;

procedure Testrun is

   package Integer_Io is new Ada.Text_Io.Integer_Io (Integer);
   use Integer_Io;

   Command : Unbounded_String;

   procedure Usage is
   begin
      Put      ("Usage: "); Put(Command_Name); New_Line;
      Put_Line ("       report <suite>");
      Put_Line ("       newsuite <suite> <comment>");
      Put_Line ("       startrun <suite> <run> <comment>");
      Put_Line ("       stoprun-ok <suite> <run> <comment>");
      Put_Line ("       stoprun-nok <suite> <run> <comment>");
   end Usage;

   procedure Report ( Suite : in String ) is
      Number_Of_Runs      : Integer := 0;
      Number_Of_Runs_Ok   : Integer := 0;
      Runs_Results        : Integer range 0..100;
      Number_Of_Tests     : Integer := 0;
      Number_Of_Tests_Ok  : Integer := 0;
      Tests_Results       : Integer range 0..100;
      Log_File            : Testbed_Io.File_Type;
      Current_Record      : Log_Record;
      Suite_Name          : Names;
      Run_Name            : Names;
      Test_Name           : Names;
      Aborting            : Boolean := False;

      procedure Format ( Action : in String; Name : in String;
                                             Comment : in String) is
      begin
         Put (Action); Put (Name);
         Put (" """); Put (Trim(Comment, Right)); Put ("""");
         New_Line;
      end Format;
      pragma Inline (Format);

      procedure Format_Error ( Message: in String; Name : in String) is
      begin
         Put ("*** ERROR *** ");
         Put (Message); Put (" "); Put (Name);
         New_Line;
      end Format_Error;
      pragma Inline (Format_Error);

   begin
      Suite_Name := (others => ' ');
      Run_Name   := (others => ' ');
      Test_Name  := (others => ' ');

      Open (Log_File, Name => Suite, Mode => In_File);
      while not End_Of_File (Log_File) loop
         Read (Log_File, Current_Record);
         case Current_Record.Entry_Type is
            when Suite_Header =>
               if Suite_Name /= "" then
                  Format_Error ("Invalide Suite Header",
                                Current_Record.Name);
               end if;
               Suite_Name := Current_Record.Name;
               Format ("begin suite      ", Current_Record.Name,
                       Current_Record.Comment);
            when Begin_Normal_Test =>
               Number_Of_Tests := Number_Of_Tests + 1;
               if Test_Name /= "" then
                  if Aborting then
                     Number_Of_Tests_Ok := Number_Of_Tests_Ok + 1;
                     Format ("abort test ok    ", Current_Record.Name, "");
                  else
                     Format_Error ("No end test record for", Test_Name);
                  end if;
               end if;
               Format ("begin test       ", Current_Record.Name,
                       Current_Record.Comment);
               Test_Name := Current_Record.Name;
               Aborting := False;
            when Begin_Aborting_Test =>
               Number_Of_Tests := Number_Of_Tests + 1;
               if Test_Name /= "" then
                  if Aborting then
                     Number_Of_Tests_Ok := Number_Of_Tests_Ok + 1;
                     Format ("abort test ok    ", Current_Record.Name, "");
                  else
                     Format_Error ("No end test record for", Test_Name);
                  end if;
               end if;
               Format ("begin abort test " , Current_Record.Name,
                       Current_Record.Comment);
               Test_Name := Current_Record.Name;
               Aborting := True;
            when End_Test_Succeeded =>
               if Aborting then
                  Format_Error ("End Test for aborting test",
                                Test_Name);
               elsif Test_Name = "" then
                  Format_Error ("No start test record for",
                                Current_Record.Name);
               elsif Test_Name /= Current_Record.Name then
                  Format_Error ("No start test record for", Test_Name);
                  Format_Error ("Found one for", Current_Record.Name);
               else
                  Number_Of_Tests_Ok := Number_Of_Tests_Ok + 1;
                  Format ("end test ok      ", Current_Record.Name,
                          Current_Record.Comment);
               end if;
               Test_Name := (others => ' ');
               Aborting := False;
            when End_Test_Failed =>
               if Aborting then
                  Format_Error ("End Test for aborting test",
                                Test_Name);
               elsif Test_Name = "" then
                  Format_Error ("No start test record for",
                                Current_Record.Name);
               elsif Test_Name /= Current_Record.Name then
                  Format_Error ("No start test record for", Test_Name);
                  Format_Error ("Found one for", Current_Record.Name);
               else
                  Format ("*** ERROR *** test  ", Current_Record.Name,
                          Current_Record.Comment);
               end if;
               Test_Name := (others => ' ');
               Aborting := False;
            when Comment =>
               Format ("comment          ", Current_Record.Name,
                       Current_Record.Comment);
            when Begin_Run =>
               Number_Of_Runs := Number_Of_Runs + 1;
               if Run_Name /= "" then
                  Format_Error ("No end run record for", Run_Name);
               end if;
               Format ("begin run        ", Current_Record.Name,
                       Current_Record.Comment);
               Run_Name := Current_Record.Name;
            when End_Run_Succeeded =>
               if Run_Name = "" then
                  Format_Error ("No start run record for",
                                Current_Record.Name);
               elsif Run_Name /= Current_Record.Name then
                  Format_Error ("No start test record for", Run_Name);
                  Format_Error ("Found one for", Current_Record.Name);
               else
                  Number_Of_Runs_Ok := Number_Of_Runs_Ok + 1;
                  Format ("end run          ", Current_Record.Name,
                          Current_Record.Comment);
               end if;
               Run_Name := (others => ' ');
            when End_Run_Failed =>
               if Run_Name = "" then
                  Format_Error ("No start run record for",
                                Current_Record.Name);
               elsif Run_Name /= Current_Record.Name then
                  Format_Error ("No start test record for", Run_Name);
                  Format_Error ("Found one for", Current_Record.Name);
               else
                  Format ("*** ERROR *** run   ", Current_Record.Name,
                          Current_Record.Comment);
               end if;
               Run_Name := (others => ' ');
         end case;
      end loop;
      Close (Log_File);
      if Aborting then
         -- if Test_Name /= "" then
         Number_Of_Tests_Ok := Number_Of_Tests_Ok + 1;
         Format ("abort test ok    ", Test_Name, "");
         -- end if;
      else
         if Run_Name /= "" then
            Format_Error ("No end run record for", Run_Name);
         end if;
      end if;

      if Number_Of_Runs > 0 then
         Runs_Results := (Number_Of_Runs_Ok * 100) / Number_Of_Runs;
      else
         Runs_Results := 0;
      end if;
      if Number_Of_Tests > 0 then
         Tests_Results := (Number_Of_Tests_Ok * 100) / Number_Of_Tests;
      else
         Tests_Results := 0;
      end if;
      New_Line; New_Line;
      Put_Line ("Statistics");
      New_Line;
      Put ("Number of runs : "); Put (Number_Of_Runs);
      Put ("  Success: "); Put (Number_Of_Runs_Ok);
      Put ("  ("); Put (Runs_Results, 3); Put ("%)");
      New_Line;
      Put ("Number of tests: "); Put (Number_Of_Tests);
      Put ("  Success: "); Put (Number_Of_Tests_Ok);
      Put ("  ("); Put (Tests_Results, 3); Put ("%)");
      New_Line;
   end Report;

begin
   if Argument_Count < 2 then
      Usage;
      return;
   end if;
   Command := To_Unbounded_String (Argument(1));
   if Command = "report" then
      if Argument_Count /= 2 then
         Usage;
         return;
      end if;
      Report (Argument(2));
   elsif Command = "newsuite" then
      if Argument_Count /= 3 then
         Usage;
         return;
      end if;
      Create_Suite (Argument(2), Argument(3));
   elsif Command = "startrun" then
      if Argument_Count /= 4 then
         Usage;
         return;
      end if;
      Open_Suite(Argument(2));
      Start_Run (Argument(3), Argument(4));
      Close_Suite;
   elsif Command = "stoprun-ok" then
      if Argument_Count /= 4 then
         Usage;
         return;
      end if;
      Open_Suite(Argument(2));
      End_Run (Argument(3), True, Argument(4));
      Close_Suite;
   elsif Command = "stoprun-nok" then
      if Argument_Count /= 4 then
         Usage;
         return;
      end if;
      Open_Suite(Argument(2));
      End_Run (Argument(3), False, Argument(4));
      Close_Suite;
   else
      Usage;
   end if;

end Testrun;
