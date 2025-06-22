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

package Aho_Corasick with SPARK_Mode is

   --  Arbitrary limits, necessary for compile-time verification.
   Max_Pattern_Length     : constant Positive := 1024;
   Max_Number_Of_Patterns : constant Positive := 65535;

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
       Pattern'Length > 0 and then
       Pattern'Length <= Max_Pattern_Length);

   type Enhanced_Pattern_Access is access constant Enhanced_Pattern;

   subtype Pattern_Array_Index is Positive range 1 .. 65535;

   type Pattern_Array is array (Pattern_Array_Index range <>)
      of not null Enhanced_Pattern_Access;

   type Match is record
      EP             : Enhanced_Pattern_Access := null;
      Start_Position : Integer_Option := No_Value;
      End_Position   : Integer_Option := No_Value;
   end record;

   type Match_Array is array (Pattern_Array_Index range <>) of Match;

   function Get_Max_States (Patterns : Pattern_Array;
                            Nocase   : Case_Sensitivity)
      return Positive
   with SPARK_Mode,
      Pre =>
         Patterns'Length > 0 and then
         Patterns'Length <= Pattern_Array_Index'Last;

   ----------------------------------------------------------------------------
   --  Automatons sub-package
   --  Use single matrix to store transitions, failure link, and outputs
   --  One row per state, one column per char, one for failure, and then
   --  one output column per pattern.
   --           Transitions    Failures   Outputs
   --  State:   [A][B][C].......[Fail?][P1][P2][P3]...
   --  State 1: [1][0][2].......[  0  ][ 1][ 0][ 1]...
   --  State 2: [0][1][0].......[  1  ][ 0][ 0][ 1]...
   ----------------------------------------------------------------------------
   generic
      Patterns : Pattern_Array;
   package Automatons with SPARK_Mode is

      CS_Max_States : constant Natural :=
         Get_Max_States (Patterns, Case_Sensitive);
      CI_Max_States : constant Natural :=
         Get_Max_States (Patterns, Case_Insensitive);

      --  256 chars + 1 for failure + patterns
      Num_Cols : constant Natural := 256 + 1 + Patterns'Length;

      --  Column indices for the automaton matrices
      type Column_Idx is new Natural range 0 .. Num_Cols - 1;

      Failure_Idx      : constant Column_Idx := 256;
      Output_Start_Idx : constant Column_Idx := 257;

      --  Row indices for the automaton matrices
      type CS_State_Extended is new Integer range -1 .. CS_Max_States - 1;
      type CI_State_Extended is new Integer range -1 .. CI_Max_States - 1;

      subtype CS_State is CS_State_Extended range 0 .. CS_State_Extended'Last;
      subtype CI_State is CI_State_Extended range 0 .. CI_State_Extended'Last;

      CS_Start_State : constant CS_State := 0;
      CI_Start_State : constant CI_State := 0;

      CS_No_State : constant CS_State_Extended := -1;
      CI_No_State : constant CI_State_Extended := -1;

      type CS_Matrix is array (CS_State, Column_Idx) of CS_State_Extended;
      type CI_Matrix is array (CI_State, Column_Idx) of CI_State_Extended;

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
         CS_States        : CS_Matrix := (others => (others => CS_No_State));
         CI_States        : CI_Matrix := (others => (others => CI_No_State));

         Has_CS           : Boolean := False;
         Has_CI           : Boolean := False;

         CS_Current_State : CS_State := CS_Start_State;
         CI_Current_State : CI_State := CI_Start_State;

         Initialized      : Boolean := False;

         Stream_Idx       : Positive := 1;
      end record;

      -------------------------------------------------------------------------
      --  Build_Automaton
      --  This function initializes the automaton matrices for both
      --  case-sensitive and case-insensitive patterns.
      -------------------------------------------------------------------------
      function Build_Automaton (Patterns : Pattern_Array) return Automaton
         with SPARK_Mode,
              Pre => Patterns'Length > 0 and then
                     Patterns'Length <= Pattern_Array_Index'Last,
              Post => Build_Automaton'Result.Initialized = True;

      -------------------------------------------------------------------------
      --  Find_Matches
      --  This procedure finds matches for the given patterns in the provided
      --  text using the Aho-Corasick algorithm across both the Case-sensitive
      --  and case-insensitive automatons.
      --  @param T        The automaton containing the state matrices.
      --  @param Patterns The patterns to search for.
      --  @param Matches  The array to store the found matches.
      --  @param Text     The input text to search within.
      -------------------------------------------------------------------------
      procedure Find_Matches (T        : in out Automaton;
                              Patterns : Pattern_Array;
                              Matches  : in out Match_Array;
                              Text     : String) with SPARK_Mode,
         Pre => Matches'Length = Patterns'Length and then
                Matches'First  = Patterns'First and then
                Matches'Last   = Patterns'Last;

      -------------------------------------------------------------------------
      --  Reset
      --  This procedure resets the automaton states and indices.
      --  @note This does not reinitialize the automaton matrices, it only
      --   resets the current state and stream index.
      --  @param T The automaton to reset.
      -------------------------------------------------------------------------
      procedure Reset (T : in out Automaton) with SPARK_Mode;
   end Automatons;

end Aho_Corasick;
