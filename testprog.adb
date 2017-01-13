--                              -*- Mode: Ada -*-
-- Filename        : testprog.adb
-- Description     : Sample Testprogram
-- Author          : Serge Robyns (c) 1998 Robyns Consulting & Services
-- Created On      : Wed Dec 13 22:43:25 2000
-- Last Modified By: .
-- Last Modified On: .
-- Update Count    : 0
-- Status          : Unknown, Use with caution!

with Testbed; use Testbed;

procedure Testprog is
   Test_Name : constant String := "serge.tlg";

   procedure Test_001 is
      A, B, C : Integer;
   begin
      Start_Test ( "Test_001", False, "1 + 2 = 3");
      A := 1;
      B := 2;
      C := A + B;
      if C = 3 then
         End_Test ( True, "1 + 2 = 3" );
      else
         End_Test ( False, "1 + 2 /= 3" );
      end if;
   end Test_001;

   procedure Test_002 is
      A, B, C : Integer;
   begin
      Start_Test ( "Test_002", True, "Aborting ... 1 / 0 ");
      A := 1;
      B := 0;
      C := A / B;
      End_Test ( False, "1 + 2 /= 3" );
   end Test_002;

begin
   Open_Suite ( Test_Name );
   Test_001;
   Test_002;
   Close_Suite;

end Testprog;
