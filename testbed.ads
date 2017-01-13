--                              -*- Mode: Ada -*-
-- Filename        : testbed.ads
-- Description     : Testbed API specifications
-- Author          : Serge Robyns (c) 1998 Robyns Consulting & Services
-- Created On      : Wed Dec 13 19:17:04 2000
-- Last Modified By: .
-- Last Modified On: .
-- Update Count    : 0
-- Status          : Stable

package Testbed is

   procedure Create_Suite(Name    : in String;
                          Comment : in String);

   procedure Open_Suite(Name : in String);

   procedure Close_Suite;

   procedure Start_Run (Name    : in String;
                        Comment : in String);

   procedure End_Run (Name      : in String;
                      Succeeded : in Boolean;
                      Comment   : in String);

   procedure Start_Test (Name              : in String;
                         Expected_To_Abort : in Boolean;
                         Comment           : in String);

   procedure End_Test (Succeeded : in Boolean;
                       Comment   : in String);

   procedure Add_Comment (Comment : in String);

   Bad_Sequence : exception;

end Testbed;
