-------------------------------------------------------------------------------
--  aho_corasick.adb
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
with Ada.Containers;
with SPARK.Containers.Formal.Doubly_Linked_Lists;

package body Aho_Corasick with SPARK_Mode is

   package Queue_Package is
      new SPARK.Containers.Formal.Doubly_Linked_Lists
        (Element_Type => Integer);

   ----------------------------------------------------------------------------
   --  From_Integer
   --  Ada 2022 integer literal to an Integer_Option.
   ----------------------------------------------------------------------------
   function From_Integer (S : String) return Integer_Option is
      ((O => Just, Value => Integer'Value (S))) with SPARK_Mode;

   ----------------------------------------------------------------------------
   --  Integer_Option_Image
   --  Ada 2022 Integer_Option custom 'Image attribute.
   ----------------------------------------------------------------------------
   procedure Integer_Option_Image (
      Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Value  : Integer_Option) with SPARK_Mode => Off
   is
   begin
      case Value.O is
         when Just =>
            Output.Put (Integer'Image (Value.Value));
         when None =>
            Output.Put ("None");
      end case;
   end Integer_Option_Image;

   ----------------------------------------------------------------------------
   --  Char_To_Index
   --  Inline wrapper around Character'Pos
   ----------------------------------------------------------------------------
   function Char_To_Index (C : Character) return Natural is
     (Character'Pos (C)) with Inline, SPARK_Mode;

   ----------------------------------------------------------------------------
   --  To_Lower
   --  This function converts a character to lowercase if it is an uppercase
   --  letter and returns it unchanged otherwise. For case-insensitive
   --  matching.
   ----------------------------------------------------------------------------
   function To_Lower (C : Character) return Character with
      Inline, SPARK_Mode is
   begin
      if C in 'A' .. 'Z' then
         return Character'Val (Char_To_Index (C) + 32);
      else
         return C;
      end if;
   end To_Lower;

   ----------------------------------------------------------------------------
   --  Sum_Lengths
   --  This function computes the sum of lengths of all case-sensitive or
   --  case-insensitive patterns in the array, giving us the total number of
   --  states needed in the automaton.
   ----------------------------------------------------------------------------
   function Sum_Lengths (Patterns : Pattern_Array;
                         Nocase   : Case_Sensitivity) return Natural
      with SPARK_Mode,
      Pre => Patterns'Length > 0 and then
         (for all P of Patterns => P /= null) and then
         (for all P of Patterns => P.Pattern /= null) and then
         (for all P of Patterns => P.Pattern'Length > 0) and then
         (for all P of Patterns => P.Pattern'Length <= Max_Pattern_Length),
      Post => Sum_Lengths'Result <= Integer'Last - 1
   is
      Result : Natural := 0;
   begin
      for I in Patterns'Range loop
         pragma Loop_Invariant (Result <=
            Max_Pattern_Length * Integer (I - Patterns'First + 1));
         pragma Loop_Invariant (I - Patterns'First + 1 <=
            Integer (Patterns'Length));

         if Patterns (I).Nocase = Nocase then
            Result := Result + Patterns (I).Pattern'Length;
         end if;
      end loop;

      return Result;
   end Sum_Lengths;

   ----------------------------------------------------------------------------
   --  Get_Max_States
   --  This function computes the maximum number of states needed for the
   --  automaton based on the sum of lengths of the patterns.
   ----------------------------------------------------------------------------
   function Get_Max_States (Patterns : Pattern_Array;
                            Nocase   : Case_Sensitivity)
      return Natural
      with SPARK_Mode,
         Pre => Patterns'Length > 0
   is
   begin
      if Patterns'Length = 0 then
         return 0;
      end if;

      return Sum_Lengths (Patterns, Nocase) + 1; --  +1 for the initial state
   end Get_Max_States;

   ----------------------------------------------------------------------------
   --  Add_Pattern_to_State
   --  This procedure adds a pattern index to the state output, ensuring that
   --  we do not exceed the maximum number of overlapping patterns.
   --  If the maximum is exceeded, it sets the Overloaded flag to True.
   ----------------------------------------------------------------------------
   procedure Add_Pattern_to_State (Outputs       : in out State_Output;
                                   Pattern_Index : Positive;
                                   Overloaded    : out Boolean)
      with SPARK_Mode
   is
   begin
      --  Check if we have room for another pattern
      if Outputs.Count < Outputs.Matched_Patterns'Length then
         Outputs.Count := Outputs.Count + 1;
         Outputs.Matched_Patterns (Outputs.Count) := Pattern_Index;
         Overloaded := False;
      else
         --  Too many overlapping patterns
         Overloaded := True;
      end if;
   end Add_Pattern_to_State;

   ----------------------------------------------------------------------------
   --  Merge_Outputs
   --  This procedure merges the outputs of two state outputs, ensuring that
   --  no duplicate patterns are added to the destination state output.
   ----------------------------------------------------------------------------
   procedure Merge_Outputs (Dest       : in out State_Output;
                            Source     : State_Output;
                            Overloaded : out Boolean)
      with SPARK_Mode
   is
   begin
      Overloaded := False;
      for I in 1 .. Source.Count loop
         declare
            Pattern_Index : constant Positive := Source.Matched_Patterns (I);
            Found : Boolean := False;
         begin
            --  Check if the pattern is already in the destination state
            for J in 1 .. Dest.Count loop
               pragma Loop_Invariant (Dest.Count <= Max_Overlapping_Patterns);
               if Dest.Matched_Patterns (J) = Pattern_Index then
                  Found := True;
                  exit;
               end if;
            end loop;

            --  Add if not duplicated and we have room
            if not Found then
               declare
                  Local_Overloaded : Boolean;
               begin
                  Add_Pattern_to_State (Dest, Pattern_Index, Local_Overloaded);
                  if Local_Overloaded then
                     Overloaded := True;
                  end if;
               end;
            end if;
         end;
      end loop;
   end Merge_Outputs;

   ----------------------------------------------------------------------------
   --  Build_Trie
   --  This procedure builds the basic trie structure from patterns, creating
   --  states and transitions but not failure links.
   ----------------------------------------------------------------------------
   procedure Build_Trie (T         : in out Simple_Automaton;
                         Patterns  : Pattern_Array;
                         Nocase    : Case_Sensitivity;
                         States    : in out Natural)
      with SPARK_Mode
   is
   begin
      --  Build trie for transitions, only for those patterns which match
      --  the specified case-sensitivity.

      declare
         Initial_States : constant Natural := States with Ghost;
      begin

         for I in Patterns'Range loop

            if Patterns (I).Nocase = Nocase then
               declare
                  Current_State : State := Start_State;
                  Pattern : constant access constant String :=
                     Patterns (I).Pattern;
                  Overload : Boolean;
               begin
                  --  Insert each character of the pattern into the trie
                  for C of Pattern.all loop
                     --  Create new state if necessary. If this pattern is
                     --  case-insensitive, normalize to lowercase.
                     declare
                        CN : constant Character :=
                           (if Nocase = Case_Insensitive then
                              To_Lower (C)
                           else C);
                     begin
                        if T.Transitions (Current_State, CN) = No_State then
                           States := States + 1;
                           T.Transitions (Current_State, CN) := States;
                        end if;

                        Current_State := T.Transitions (Current_State, CN);
                     end;
                  end loop;

                  --  Add current word in output links
                  Add_Pattern_to_State (
                     T.Outputs (Current_State), I, Overload);

                  if Overload then
                     T.Overloaded := True;
                  end if;
               end;
            end if;
         end loop;

         pragma Assert (States >= Initial_States);
      end;
   end Build_Trie;

   ----------------------------------------------------------------------------
   --  Add_Default_Transitions
   --  This procedure adds default transitions from state 0 for characters
   --  that don't have explicit transitions.
   ----------------------------------------------------------------------------
   procedure Add_Default_Transitions (T : in out Simple_Automaton)
      with SPARK_Mode
   is
   begin
      --  Add edge from state 0 to all characters that don't have one already
      for C in Character'Range loop

         --  Ensure transition is non-negative
         if T.Transitions (Start_State, C) = No_State then
            --  Loop back to start state
            T.Transitions (Start_State, C) := Start_State;
         end if;

      end loop;
   end Add_Default_Transitions;

   ----------------------------------------------------------------------------
   --  Build_Failure_Links
   --  This procedure builds the failure links using BFS (breadth-first search)
   --  algorithm for the Aho-Corasick automaton.
   ----------------------------------------------------------------------------
   procedure Build_Failure_Links (T      : in out Simple_Automaton;
                                  States : Natural)
      with SPARK_Mode, Always_Terminates
   is
      Queue : Queue_Package.List (
         Capacity => Ada.Containers.Count_Type (States + 1));
      Queue_Size : Natural := 0 with Ghost;
   begin
      --  Initialize failure links
      T.Failures := [others => -1];

      --  Failure is computed in BFS order using queue
      --  Take every possible input character
      for C in Character'Range loop
         if T.Transitions (0, C) > 0 then
            --  If there is a transition from the start state,
            --  add it to the queue

            --  Failure link to start state
            T.Failures (T.Transitions (0, C)) := 0;
            Queue.Append (T.Transitions (0, C));
            Queue_Size := Queue_Size + 1;
         end if;
      end loop;

      while not Queue.Is_Empty loop
         pragma Loop_Variant (Decreases => Queue_Size);
         declare
            Current_State_Int : constant Integer := Queue.First_Element;
            Current_State : constant State := Current_State_Int;
         begin
            Queue.Delete_First;
            Queue_Size := Queue_Size - 1;

            --  For removed state, find failure link for those characters
            --  that do not have a transition
            for C in Character'Range loop

               --  If transition function is defined for character and state
               if T.Transitions (Current_State, C) /= -1 then
                  declare
                     --  Failure link for removed state
                     Failure_State : Integer := T.Failures (Current_State);
                  begin
                     --  Find deepest node labeled by suffix of string from
                     --  root to current state
                     while T.Transitions (Failure_State, C) = -1 loop
                        pragma Loop_Variant (Decreases => Failure_State);
                        Failure_State := T.Failures (Failure_State);
                     end loop;

                     Failure_State := T.Transitions (Failure_State, C);
                     T.Failures (T.Transitions (Current_State, C)) :=
                        Failure_State;

                     declare
                        Target_State : constant Integer :=
                           T.Transitions (Current_State, C);
                     begin
                        if Target_State /= Failure_State then
                           declare
                              Local_Overloaded : Boolean;
                              Failure_Output : constant State_Output :=
                                 T.Outputs (Failure_State);
                           begin
                              Merge_Outputs (
                                 T.Outputs (Target_State),
                                 Failure_Output,
                                 Local_Overloaded);
                              if Local_Overloaded then
                                 T.Overloaded := True;
                              end if;
                           end;
                        end if;
                     end;

                     --  Insert next level of Trie into queue
                     Queue.Append (T.Transitions (Current_State, C));
                     Queue_Size := Queue_Size + 1;
                  end;
               end if;
            end loop;
         end;
      end loop;
   end Build_Failure_Links;

   ----------------------------------------------------------------------------
   --  Build_Simple_Automaton
   --  This function builds the Aho-Corasick automaton from the given patterns.
   --  It constructs a trie for the patterns, adds failure links, and prepares
   --  the automaton for matching.
   ----------------------------------------------------------------------------
   function Build_Simple_Automaton (Patterns : Pattern_Array;
                                    Nocase   : Case_Sensitivity)
      return Simple_Automaton with SPARK_Mode
   is
      States : Natural := 1; -- Start state
   begin
      return T : Simple_Automaton (Get_Max_States (Patterns, Nocase)) do
         --  Initialize automaton structure
         T.Transitions := [others => [others => No_State]];
         T.Failures := [others => No_State];
         T.Outputs := [others => <>];

         Build_Trie (T, Patterns, Nocase, States);
         Add_Default_Transitions (T);
         Build_Failure_Links (T, States);

         T.Initialized := True;
         T.Current_State := Start_State;
      end return;
   end Build_Simple_Automaton;

   ----------------------------------------------------------------------------
   --  Build_Automaton
   --  Wrapper for both case-sensitive and case-insensitive sub-automata.
   ----------------------------------------------------------------------------
   function Build_Automaton (Patterns : Pattern_Array) return Automaton
      with SPARK_Mode
   is
      CS_States : constant Natural :=
         Get_Max_States (Patterns, Case_Sensitive);
      CI_States : constant Natural :=
         Get_Max_States (Patterns, Case_Insensitive);
   begin
      return T : Automaton := (
         CS_States => CS_States,
         CI_States => CI_States,
         CS_Automaton => Build_Simple_Automaton (Patterns, Case_Sensitive),
         CI_Automaton => Build_Simple_Automaton (Patterns, Case_Insensitive),
         others => <>)
      do
         T.Stream_Index := 0;
         T.Overloaded := T.CS_Automaton.Overloaded or else
                         T.CI_Automaton.Overloaded;
         T.Initialized := not T.Overloaded;
      end return;
   end Build_Automaton;

   ----------------------------------------------------------------------------
   --  Check_Match_Position
   --  For patterns which specify distance/offset/depth/within modifiers, this
   --  function checks if the current position in the text matches the pattern
   --  based on the match start and end locations, stream index, and
   --  previous match end position.
   --  @param Pattern            The pattern to match.
   --  @param Start_Pos          The starting position of this match (relative
   --   to the start of the stream).
   --  @param End_Pos            The ending position of this match (relative
   --   to start of the stream).
   --  @param Stream_Index       Current position within the input stream
   --  @param Prev_Match_End The end position of the previous match.
   --  @return True if the position matches the pattern, otherwise False.
   ----------------------------------------------------------------------------
   function Check_Match_Position (Pattern        : Enhanced_Pattern_Access;
                                  Start_Pos      : Natural;
                                  End_Pos        : Natural;
                                  Prev_Match_End : Integer)
      return Boolean with SPARK_Mode
   is
   begin
      --  Depth/Offset are used to limit matches relative to stream position.
      --  Distance/Within are used to limit matches relative to the previous
      --  match position. Note that Start_Pos and End_Pos are already
      --  adjusted by the Stream_Index, so they are relative to the stream.

      --  Depth: pattern must end within the first Pattern.Depth chars of
      --  the stream.
      if Pattern.Depth /= No_Value then
         if End_Pos > Pattern.Depth.Value then
            return False; -- Pattern ends after the allowed depth
         end if;
      end if;

      --  Offset: pattern must start after the first Pattern.Offset chars of
      --  the stream.
      if Pattern.Offset /= No_Value then
         if Start_Pos < Pattern.Offset.Value then
            return False; -- Pattern starts before the allowed offset
         end if;
      end if;

      --  Distance: pattern must start within N bytes of previous match
      if Pattern.Distance /= No_Value then
         if Prev_Match_End /= -1 then
            if Start_Pos < Prev_Match_End + Pattern.Distance.Value then
               return False; --  Pattern starts before the allowed distance
            end if;
         else
            return False; --  Fail if no previous match but distance is set
         end if;
      end if;

      --  Within: pattern must end within N bytes of previous match
      if Pattern.Within /= No_Value then
         if Prev_Match_End /= -1 then
            if End_Pos > Prev_Match_End + Pattern.Within.Value then
               return False; --  Pattern ends after the allowed within
            end if;
         else
            return False; --  Fail if no previous match but within is set
         end if;
      end if;

      return True;
   end Check_Match_Position;

   ----------------------------------------------------------------------------
   --  Get_Prev_Match_End
   --  Given a Matches array and a _current_ pattern index, this function
   --  returns the end position of the preceding match, or -1 if no previous
   --  match exists for that pattern.
   ----------------------------------------------------------------------------
   function Get_Prev_Match_End (Matches       : Match_Array;
                                Pattern_Index : Positive) return Integer
      with SPARK_Mode is
   begin
      if Pattern_Index = Matches'First then
         --  First pattern, so no previous match
         return -1;
      end if;

      if Matches (Pattern_Index - 1).EP = null then
         --  Previous pattern hasn't been matched
         return -1;
      end if;

      --  Return the end position of the previous match
      return Matches (Pattern_Index - 1).End_Position.Value;
   end Get_Prev_Match_End;

   ----------------------------------------------------------------------------
   --  Find_Next_State
   --  This procedure finds the next state in the Aho-Corasick automaton based
   --  on the current state and the next input character.
   ----------------------------------------------------------------------------
   procedure Find_Next_State (T          : in out Simple_Automaton;
                              Next_Input : Character) with SPARK_Mode
   is
      Answer : State_Or_None := T.Current_State;
   begin
      --  Fast path: direct transition exists
      if T.Transitions (Answer, Next_Input) /= No_State then
         T.Current_State := T.Transitions (Answer, Next_Input);
         return;
      end if;

      --  Slow path: follow failure links
      --  Unrolled first iteration for common case
      Answer := T.Failures (Answer);

      if Answer >= Start_State and then
         T.Transitions (Answer, Next_Input) /= No_State
      then
         T.Current_State := T.Transitions (Answer, Next_Input);
         return;
      end if;

      --  If transition not defined, follow failure links
      while Answer >= Start_State and then
         T.Transitions (Answer, Next_Input) = No_State loop

         Answer := T.Failures (Answer);
      end loop;

      T.Current_State := T.Transitions (Answer, Next_Input);
   end Find_Next_State;

   ----------------------------------------------------------------------------
   --  Find_Matches
   ----------------------------------------------------------------------------
   procedure Find_Matches (T        : in out Automaton;
                           Patterns : Pattern_Array;
                           Matches  : in out Match_Array;
                           Text     : String) with SPARK_Mode
   is
      C  : Character;
      TCS : Simple_Automaton renames T.CS_Automaton;
      TCI : Simple_Automaton renames T.CI_Automaton;
   begin
      for I in Text'Range loop

         C := Text (I);

         Find_Next_State (TCS, C);
         Find_Next_State (TCI, To_Lower (C));

         --  See if there are any matches in the current state.
         if TCS.Outputs (TCS.Current_State).Count > 0 then
            --  Look up the patterns that matched at this state
            for J in 1 .. TCS.Outputs (TCS.Current_State).Count loop
               declare
                  --  matched pattern index
                  MPI : constant Positive :=
                     TCS.Outputs (TCS.Current_State).Matched_Patterns (J);

                  Start_Pos      : constant Natural :=
                     I - Patterns (MPI).Pattern'Length + 1 + T.Stream_Index;

                  End_Pos        : constant Natural := I + T.Stream_Index;
                  Prev_Match_End : constant Integer :=
                     Get_Prev_Match_End (Matches, MPI);
               begin
                  if Check_Match_Position (
                     Pattern        => Patterns (MPI),
                     Start_Pos      => Start_Pos,
                     End_Pos        => End_Pos,
                     Prev_Match_End => Prev_Match_End)
                  then
                     Matches (MPI).EP := Patterns (MPI);
                     Matches (MPI).Start_Position :=
                        (O => Just, Value => Start_Pos);
                     Matches (MPI).End_Position :=
                        (O => Just, Value => End_Pos);
                  end if;
               end;
            end loop;
         end if;

         --  Now case-insensitive matches
         if TCI.Outputs (TCI.Current_State).Count > 0 then
            for J in 1 .. TCI.Outputs (TCI.Current_State).Count loop
               declare
                  MPI : constant Positive :=
                     TCI.Outputs (TCI.Current_State).Matched_Patterns (J);

                  Start_Pos      : constant Natural :=
                     I - Patterns (MPI).Pattern'Length + 1 + T.Stream_Index;

                  End_Pos        : constant Natural := I + T.Stream_Index;
                  Prev_Match_End : constant Integer :=
                     Get_Prev_Match_End (Matches, MPI);
               begin
                  if Check_Match_Position (
                     Pattern        => Patterns (MPI),
                     Start_Pos      => Start_Pos,
                     End_Pos        => End_Pos,
                     Prev_Match_End => Prev_Match_End)
                  then
                     Matches (MPI).EP := Patterns (MPI);
                     Matches (MPI).Start_Position :=
                        (O => Just, Value => Start_Pos);
                     Matches (MPI).End_Position :=
                        (O => Just, Value => End_Pos);
                  end if;
               end;
            end loop;
         end if;
      end loop;

      T.Stream_Index := T.Stream_Index + Text'Length;
   end Find_Matches;

   ----------------------------------------------------------------------------
   --  Reset
   ----------------------------------------------------------------------------
   procedure Reset (T : in out Automaton) with SPARK_Mode is
   begin
      T.CS_Automaton.Current_State := Start_State;
      T.CI_Automaton.Current_State := Start_State;
      T.Stream_Index := 0;
   end Reset;

end Aho_Corasick;
