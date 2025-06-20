-------------------------------------------------------------------------------
--  aho_corasick.ads
--
--  Copyright (c) 2025, High Confidence / Jon Andrew
--  All rights reserved.
--
--  This package is an implementation of the Aho-Corasick multi-pattern
--  matcher which supports mixing case-sensitive and case-insensitive rules
--  using a modified Double-Search algorithm.
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------------------------
with Ada.Strings.Text_Buffers;
with SPARK;

package Aho_Corasick with SPARK_Mode is

   --  Use single matrix to store transitions, failures, and outputs
   --  One row per state, one column per char, +1 for failure, and then
   --  one output column per pattern.
   --           Transitions    Failures   Outputs
   --  State:   [A][B][C].......[Fail?][P1][P2][P3]...
   --  State 1: [1][0][2].......[  0  ][ 1][ 0][ 1]...
   --  State 2: [0][1][0].......[  1  ][ 0][ 0][ 1]...

   type Case_Sensitivity is (Case_Sensitive, Case_Insensitive);

   type String_Access is access constant String;

   type Maybe is (Just, None);

   type Integer_Option (O : Maybe := Just) is record
      case O is
         when Just => Value : Integer;
         when None => null;
      end case;
   end record with
      Integer_Literal => From_Integer,
      Put_Image       => Integer_Option_Image;

   ----------------------------------------------------------------------------
   --  From_Integer
   --  Ada 2022 user-defined literal for Integer_Option.
   ----------------------------------------------------------------------------
   function From_Integer (S : String) return Integer_Option with SPARK_Mode;

   ----------------------------------------------------------------------------
   --  Integer_Option_Image
   --  Ada 2022 Integer_Option custom 'Image attribute.
   ----------------------------------------------------------------------------
   procedure Integer_Option_Image (
      Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Value  : Integer_Option) with SPARK_Mode => Off;

   No_Value : constant Integer_Option := (O => None);

   ----------------------------------------------------------------------------
   --  Content Rules
   --
   --  @param Pattern is the text to match against
   --  @param Nocase is whether to ignore case when matching
   --  @param Offset pattern must occur after the first Offset characters of
   --   the start of the stream
   --  @param Depth pattern must occur inside the first Depth characters of the
   --   start of the stream
   --  @param Distance pattern must start after N characters from the end of
   --   the previous match
   --  @param Within pattern must end within N characters from the end of the
   --   previous match
   --
   --  See Snort rules for how the Offset, Depth, Distance, and Within
   --  parameters are used.
   ----------------------------------------------------------------------------
   type Enhanced_Pattern is record
      Pattern  : String_Access;
      Nocase   : Case_Sensitivity := Case_Sensitive;
      Offset   : Integer_Option := (O => None);
      Depth    : Integer_Option := (O => None);
      Distance : Integer_Option := (O => None);
      Within   : Integer_Option := (O => None);
   end record with Dynamic_Predicate =>
      (Pattern /= null and then
       Pattern'Length > 0);

   type Enhanced_Pattern_Access is access constant Enhanced_Pattern;

   subtype Pattern_Array_Index is Positive range 1 .. 65535;

   type Pattern_Array is array (Pattern_Array_Index range <>)
      of not null Enhanced_Pattern_Access;

   type Match is record
      EP             : Enhanced_Pattern_Access := null;
      Start_Position : Integer_Option := No_Value;
      End_Position   : Integer_Option := No_Value;
   end record;

   type Match_Array is array (Positive range <>) of Match;

   ----------------------------------------------------------------------------
   --  Automatons sub-package
   ----------------------------------------------------------------------------
   generic
      Patterns : Pattern_Array;
   package Automatons with SPARK_Mode is
      function Get_Max_States (Patterns : Pattern_Array;
                               Nocase   : Case_Sensitivity)
         return Positive with SPARK_Mode;

      CS_Max_States : constant Natural :=
         Get_Max_States (Patterns, Case_Sensitive);
      CI_Max_States : constant Natural :=
         Get_Max_States (Patterns, Case_Insensitive);

      --  0-255 for chars + 1 for failure + patterns
      Num_Cols : constant Natural := 255 + 1 + Patterns'Length;

      --  Column indices for the automaton matrices
      type Column_Idx is new Positive range 0 .. Num_Cols - 1;

      Failure_Idx      : constant Column_Idx := 256;
      Output_Start_Idx : constant Column_Idx := 257;

      --  Row indices for the automaton matrices
      type CS_State is new Positive range 0 .. CS_Max_States - 1;
      type CI_State is new Positive range 0 .. CI_Max_States - 1;

      CS_Start_State : constant CS_State := 0;
      CI_Start_State : constant CI_State := 0;

      type State_Validity is (Uninitialized,
                              Valid_State,
                              No_Transition,
                              Valid_Output,
                              No_Output);

      type CS_Transition is record
         Valid      : State_Validity := Uninitialized;
         Next_State : CS_State := CS_Start_State;
      end record;

      type CI_Transition is record
         Valid      : State_Validity := Uninitialized;
         Next_State : CI_State := CI_Start_State;
      end record;

      type CS_Matrix is array (CS_State, Column_Idx) of CS_Transition;
      type CI_Matrix is array (CI_State, Column_Idx) of CI_Transition;

      -------------------------------------------------------------------------
      --  Automaton
      --  This record holds the state matrices for both case-sensitive and
      --  case-insensitive automata, along with the current states and
      --  initialization status.
      --  @param CS_States is the case-sensitive state transition matrix
      --  @param CI_States is the case-insensitive state transition matrix
      --  @param CS_Current_State is the current state of the case-sensitive
      --   automaton
      --  @param CI_Current_State is the current state of the case-insensitive
      --   automaton
      --  @param Initialized indicates whether the automaton has been
      --   initialized
      --  @param Stream_Idx is the index of the current stream being processed
      -------------------------------------------------------------------------
      type Automaton is limited record
         CS_States        : CS_Matrix;
         CI_States        : CI_Matrix;

         CS_Current_State : CS_State := CS_Start_State;
         CI_Current_State : CI_State := CI_Start_State;

         Initialized      : Boolean := False;

         Stream_Idx       : Natural := 1;
      end record;

      function Build_Automaton (Patterns : Pattern_Array) return Automaton
         with SPARK_Mode;

   end Automatons;

end Aho_Corasick;
