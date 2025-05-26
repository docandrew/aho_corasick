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
with Interfaces; use Interfaces;
with Ada.Strings.Text_Buffers;

package Aho_Corasick with SPARK_Mode is

   --  The maximum length of a pattern that can be matched.
   --  This is a constant that can be adjusted based on the expected
   --  length of patterns. This is used for compile-time verification
   --  and avoiding overflow.
   Max_Pattern_Length : constant Natural := 1024;

   --  The maximum number of patterns that can be matched.
   --  This is a constant that can be adjusted based on the expected
   --  number of patterns. This is used for compile-time verification
   --  and avoiding overflow.
   Max_Number_Of_Patterns : constant Natural := 65536;

   --  The maximum number of patterns that can overlap/end at a single state.
   --  This is a constant that can be adjusted based on the expected number
   --  of overlapping patterns. If the number of overlapping patterns exceeds
   --  this value, the automaton will be marked as overloaded and will not
   --  be usable for matching. This is set to 16 by default, but can be
   --  changed by the user to a higher value if needed.
   Max_Overlapping_Patterns : constant Natural := 16;

   type Case_Sensitivity is (Case_Sensitive, Case_Insensitive);

   --  State type for better verification
   --  States are non-negative integers, with -1 as a special "no state" value
   subtype State is Integer range 0 .. Integer'Last;
   subtype State_Or_None is Integer range -1 .. Integer'Last;

   --  State constants for clarity
   No_State    : constant State_Or_None := -1;
   Start_State : constant State := 0;

   type Transition_Array is
      array (State range <>, Character range <>) of State_Or_None;
   type Failure_Array is array (State range <>) of State_Or_None;

   type Pattern_Index_Array is array (Natural range <>) of Natural;

   ----------------------------------------------------------------------------
   --  State_Output
   --  For each state, we store the patterns that matched at that state.
   --
   --  The maximum number of overlapping patterns is defined by the user.
   --  i.e. "admin", "cadmin", "dadmin" would all match at the same state.
   --  See Max_Overlapping_Patterns above.
   ----------------------------------------------------------------------------
   type State_Output is record
      --  The patterns that matched at this state
      Matched_Patterns : Pattern_Index_Array (1 .. Max_Overlapping_Patterns) :=
         (others => 0);

      --  The number of patterns that matched at this state
      Count : Natural := 0;
   end record;

   type Output_Array is array (Natural range <>) of State_Output;

   ----------------------------------------------------------------------------
   --  Simple_Automaton
   --
   --  Implementation of the Aho-Corasick state machine.
   --  @param Initialized - Whether the automaton has been initialized
   --  @param Overloaded - Whether the automaton has been overloaded.
   --    This can happen if the number of overlapping patterns ending
   --    at a state exceeds the maximum number. This should be checked
   --    before using the automaton. If True, the automaton will be
   --    unusable.
   --  @param Transitions - The transitions for each state. This is a
   --    two-dimensional array where the first dimension is the state
   --    and the second dimension is the character. The value is the
   --    next state to transition to when the character is encountered.
   --  @param Failures - The failure states for each state. This is a
   --    one-dimensional array where the index is the state. The value
   --    is the state to transition to when there is no transition for
   --    the character encountered.
   --  @param Outputs - The outputs for each state. This is a
   --    one-dimensional array where the index is the state. The value
   --    is a record containing the patterns that matched at that state
   --    and the number of patterns that matched.
   --  @param Current_State - The current state of the automaton.
   ----------------------------------------------------------------------------
   type Simple_Automaton (Max_States : State) is limited record
      Initialized : Boolean := False;
      Overloaded  : Boolean := False;

      Transitions : Transition_Array (0 .. Max_States,
         Character'First .. Character'Last);
      Failures : Failure_Array (0 .. Max_States);
      Outputs : Output_Array (0 .. Max_States);

      Current_State : State := Start_State;
   end record;

   ----------------------------------------------------------------------------
   --  Automaton
   --
   --  This is the main automaton that combines both case-sensitive and
   --  case-insensitive matching. It contains two Simple_Automaton instances,
   --  one for case-sensitive matching and one for case-insensitive matching.
   --  The automaton is initialized with the patterns provided and can then
   --  be used to find matches in a text.
   --  @param Initialized - Whether the automaton has been initialized
   --  @param Overloaded - Whether either of the case-sensitive or 
   --   case-insitive automata have been overloaded. This can happen if the
   --   number of overlapping patterns ending at a state exceeds the maximum
   --   number. This should be checked before using the automaton. If True,
   --   the automaton will be unusable, and calls to Find_Matches will
   --   fail.
   --  @param CS_Automaton - The case-sensitive automaton
   --  @param CI_Automaton - The case-insensitive automaton
   --  @param Stream_Index - The position in the overall stream from which
   --    matches occur. This is used for streaming text. If the text is not
   --    streaming, this should be reset to 0 manually or by calling
   --    Build_Automaton again.
   ----------------------------------------------------------------------------
   type Automaton (CS_States : Natural; CI_States : Natural) is limited record
      Initialized : Boolean := False;
      Overloaded  : Boolean := False;

      CS_Automaton : Simple_Automaton (CS_States);
      CI_Automaton : Simple_Automaton (CI_States);

      --  If streaming text, this is the position from the beginning
      --  of all streaming.
      Stream_Index : Natural := 0;
   end record;

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

   subtype Pattern_Count is Positive range 1 .. Max_Number_Of_Patterns;

   type Pattern_Array is array (Pattern_Count range <>)
      of not null Enhanced_Pattern_Access;

   type Match is record
      EP             : Enhanced_Pattern_Access := null;
      Start_Position : Integer_Option := No_Value;
      End_Position   : Integer_Option := No_Value;
   end record;

   type Match_Array is array (Positive range <>) of Match;

   ----------------------------------------------------------------------------
   --  Build_Automaton
   --
   --  Create a new pattern matcher. Determines the parameters for the
   --  Aho-Corasick state machine/automaton from the patterns provided. The
   --  automaton can then be used to find matches in a text. Mixing
   --  case-sensitive and case-insensitive patterns is supported.
   --  @param Patterns is the array of patterns to match against.
   --    This should be an array of Enhanced_Pattern_Access, which allows
   --    for more complex matching rules such as case-insensitivity, offsets,
   --    and depth limits (see Snort rules for how these are used)
   --  @return The automaton that can be used to find matches in a text.
   ----------------------------------------------------------------------------
   function Build_Automaton (Patterns : Pattern_Array)
      return Automaton
         with
            Pre => Patterns'Length > 0,
            Post => Build_Automaton'Result.Initialized = True;

   ----------------------------------------------------------------------------
   --  Find_Matches
   --
   --  Find all matches of patterns in the text. If called on
   --  subsequent texts, matching will continue from the
   --  last position in the text. This function can therefore
   --  be used for streaming text. If this is not desired,
   --  reset the automaton by calling Build_Automaton again or by manually
   --  resetting the Automaton.Stream_Index field to 0.
   --
   --  @param T is the automaton to use for matching. It must be
   --    initialized with Build_Automaton before use.
   --  @param Patterns is the array of patterns to match against.
   --    This should be the same array used to build the automaton.
   --  @param Matches is the output array where matches will be stored.
   --  @param Text is the text to search for matches in.
   --  @return The matches found in the text. The matches will be
   --    stored in the Matches array of the automaton which is statically
   --    allocated based on the number of patterns provided.
   ----------------------------------------------------------------------------
   procedure Find_Matches (T        : in out Automaton;
                           Patterns : Pattern_Array;
                           Matches  : in out Match_Array;
                           Text     : String)
      with Pre => T.Initialized and then
                  not T.Overloaded and then
                  Patterns'Length > 0 and then
                  Patterns'Length <= Max_Number_Of_Patterns and then
                  Matches'Length >= Patterns'Length and then
                  (for all P of Patterns => P /= null) and then
                  (for all P of Patterns => P.Pattern /= null) and then
                  (for all P of Patterns => P.Pattern'Length <=
                     Max_Pattern_Length);

   ----------------------------------------------------------------------------
   --  Reset
   --  Reset the automaton to its initial state. This will clear the current
   --  state and the stream index, allowing for a fresh start with new text.
   --  This is useful for non-streaming text where you want to start over
   --  without rebuilding the automaton.
   --  @param T is the automaton to reset. It must be initialized.
   ----------------------------------------------------------------------------
   procedure Reset (T : in out Automaton)
      with Pre => T.Initialized and then not T.Overloaded;

end Aho_Corasick;
