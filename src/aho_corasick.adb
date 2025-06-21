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
with Ada.Text_IO;
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
      ((O => Just, Value => Integer'Value (S))) with SPARK_Mode => Off;

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
   --  Automatons sub-package
   ----------------------------------------------------------------------------
   package body Automatons is
      -------------------------------------------------------------------------
      --  Column
      --  Helper to get index of a column in the character part of the matrix.
      -------------------------------------------------------------------------
      function Column (C : Character) return Column_Idx is
         (Column_Idx (Character'Pos (C))) with SPARK_Mode;

      -------------------------------------------------------------------------
      --  Column
      --  Helper to get index of a column in the output part of the matrix
      -------------------------------------------------------------------------
      function Column (Pattern_Idx : Pattern_Array_Index) return Column_Idx is
         (Output_Start_Idx + Column_Idx (Pattern_Idx) - 1) with SPARK_Mode,
            Pre => Pattern_Idx in 1 .. Patterns'Length and then
                   Column_Idx (Pattern_Idx) <= Column_Idx'Last;

      -------------------------------------------------------------------------
      --  Sum_Lengths
      --  This function computes the sum of lengths of all case-sensitive or
      --  case-insensitive patterns in the array, giving us the total number of
      --  states needed in the automaton.
      -------------------------------------------------------------------------
      function Sum_Lengths (Patterns : Pattern_Array;
                            Nocase   : Case_Sensitivity) return Natural
         with SPARK_Mode
      is
         Result : Natural := 0;
      begin
         for I in Patterns'Range loop

            --  Only count patterns that match the specified case-sensitivity
            if Patterns (I).Nocase = Nocase then
               Result := Result + Patterns (I).Pattern'Length;
            end if;
         end loop;

         return Result;
      end Sum_Lengths;

      -------------------------------------------------------------------------
      --  Get_Max_States
      --  This function computes the maximum number of states needed for the
      --  automaton based on the sum of lengths of the patterns.
      -------------------------------------------------------------------------
      function Get_Max_States (Patterns : Pattern_Array;
                               Nocase   : Case_Sensitivity)
         return Positive with SPARK_Mode
      is
      begin
         return Sum_Lengths (Patterns, Nocase) + 1; --  +1 for initial state
      end Get_Max_States;

      -------------------------------------------------------------------------
      --  Helper functions for the automaton matrices
      -------------------------------------------------------------------------
      procedure Set_CS_Character_Transition (
         Matrix     : in out CS_Matrix;
         From_State : CS_State;
         Char       : Character;
         To_State   : CS_State)
      with SPARK_Mode,
         Post =>
            Matrix (From_State, Column (Char)).Valid =
               Valid_State 
         and then
            Matrix (From_State, Column (Char)).Next_State =
               To_State;

      procedure Set_CS_Character_Transition (
         Matrix     : in out CS_Matrix;
         From_State : CS_State;
         Char       : Character;
         To_State   : CS_State) with SPARK_Mode
      is
         Col : constant Column_Idx := Column_Idx (Char_To_Index (Char));
      begin
         Matrix (From_State, Col) := (Valid      => Valid_State,
                                      Next_State => To_State);
      end Set_CS_Character_Transition;

      -------------------------------------------------------------------------
      --  Set_CI_Character_Transition
      -------------------------------------------------------------------------
      procedure Set_CI_Character_Transition (
         Matrix     : in out CI_Matrix;
         From_State : CI_State;
         Char       : Character;
         To_State   : CI_State)
      with SPARK_Mode,
         Post =>
            Matrix (From_State, Column (To_Lower (Char))).Valid =
               Valid_State
         and then
            Matrix (From_State, Column (To_Lower (Char))).Next_State =
               To_State;

      procedure Set_CI_Character_Transition (
         Matrix     : in out CI_Matrix;
         From_State : CI_State;
         Char       : Character;
         To_State   : CI_State) with SPARK_Mode
      is
         Col : constant Column_Idx := Column_Idx (
               Char_To_Index (To_Lower (Char)));
      begin
         Matrix (From_State, Col) := (Valid      => Valid_State,
                                      Next_State => To_State);
      end Set_CI_Character_Transition;

      -------------------------------------------------------------------------
      --  Set_CS_Failure_Link
      -------------------------------------------------------------------------
      procedure Set_CS_Failure_Link (
         Matrix     : in out CS_Matrix;
         From_State : CS_State;
         To_State   : CS_State)
      with SPARK_Mode,
         Post =>
            Matrix (From_State, Failure_Idx).Valid = Valid_State
         and then
            Matrix (From_State, Failure_Idx).Next_State = To_State;

      procedure Set_CS_Failure_Link (
         Matrix     : in out CS_Matrix;
         From_State : CS_State;
         To_State   : CS_State) with SPARK_Mode
      is
      begin
         Matrix (From_State, Failure_Idx) := (Valid      => Valid_State,
                                              Next_State => To_State);
      end Set_CS_Failure_Link;

      -------------------------------------------------------------------------
      --  Set_CI_Failure_Link
      -------------------------------------------------------------------------
      procedure Set_CI_Failure_Link (
         Matrix     : in out CI_Matrix;
         From_State : CI_State;
         To_State   : CI_State)
      with SPARK_Mode,
         Post =>
            Matrix (From_State, Failure_Idx).Valid = Valid_State
         and then
            Matrix (From_State, Failure_Idx).Next_State = To_State;

      procedure Set_CI_Failure_Link (
         Matrix     : in out CI_Matrix;
         From_State : CI_State;
         To_State   : CI_State) with SPARK_Mode
      is
      begin
         Matrix (From_State, Failure_Idx) := (Valid      => Valid_State,
                                              Next_State => To_State);
      end Set_CI_Failure_Link;

      -------------------------------------------------------------------------
      --  Set_CS_Pattern_Output
      -------------------------------------------------------------------------
      procedure Set_CS_Pattern_Output (
         Matrix      : in out CS_Matrix;
         State       : CS_State;
         Pattern_Idx : Pattern_Array_Index;
         Does_Match  : Boolean) with SPARK_Mode,
         Pre => Pattern_Idx in 1 .. Patterns'Length and then
                Column (Pattern_Idx) <= Column_Idx'Last,
         Post => (if Does_Match then
                    Matrix (State, Column (Pattern_Idx)).Valid =
                      Valid_Output
                  else
                    Matrix (State, Column (Pattern_Idx)).Valid =
                      No_Output);

      procedure Set_CS_Pattern_Output (
         Matrix      : in out CS_Matrix;
         State       : CS_State;
         Pattern_Idx : Pattern_Array_Index;
         Does_Match  : Boolean) with SPARK_Mode
      is
         Col : constant Column_Idx := Column (Pattern_Idx);
         New_State : constant CS_Transition :=
            (Valid      => (if Does_Match then Valid_Output else No_Output),
             Next_State => CS_Start_State);
      begin
         Matrix (State, Col) := New_State;
      end Set_CS_Pattern_Output;

      -------------------------------------------------------------------------
      --  Set_CI_Pattern_Output
      -------------------------------------------------------------------------
      procedure Set_CI_Pattern_Output (
         Matrix      : in out CI_Matrix;
         State       : CI_State;
         Pattern_Idx : Pattern_Array_Index;
         Does_Match  : Boolean) with SPARK_Mode,
         Pre => Pattern_Idx in 1 .. Patterns'Length and then
                Column (Pattern_Idx) <= Column_Idx'Last,
         Post => (if Does_Match then
                    Matrix (State, Column (Pattern_Idx)).Valid =
                      Valid_Output
                  else
                    Matrix (State, Column (Pattern_Idx)).Valid =
                      No_Output);

      procedure Set_CI_Pattern_Output (
         Matrix      : in out CI_Matrix;
         State       : CI_State;
         Pattern_Idx : Pattern_Array_Index;
         Does_Match  : Boolean) with SPARK_Mode
      is
         Col : constant Column_Idx := Column (Pattern_Idx);
         New_State : constant CI_Transition :=
            (Valid      => (if Does_Match then Valid_Output else No_Output),
             Next_State => CI_Start_State);
      begin
         Matrix (State, Col) := New_State;
      end Set_CI_Pattern_Output;

      -------------------------------------------------------------------------
      --  Get_CS_Character_Transition
      --  This function retrieves the character transition for a given state
      --  and character in the case-sensitive automaton.
      -------------------------------------------------------------------------
      function Get_CS_Character_Transition (
         Matrix     : CS_Matrix;
         From_State : CS_State;
         Char       : Character) return CS_Transition
         is (Matrix (From_State, Column (Char))) with SPARK_Mode;

      -------------------------------------------------------------------------
      --  Get_CI_Character_Transition
      -------------------------------------------------------------------------
      function Get_CI_Character_Transition (
         Matrix     : CI_Matrix;
         From_State : CI_State;
         Char       : Character) return CI_Transition
         is (Matrix (From_State, Column (Char))) with SPARK_Mode;

      -------------------------------------------------------------------------
      --  Get_CS_Failure_Link
      --  This function retrieves the failure link for a given state in the
      --  case-sensitive automaton.
      -------------------------------------------------------------------------
      function Get_CS_Failure_Link (
         Matrix     : CS_Matrix;
         From_State : CS_State) return CS_State
            is (Matrix (From_State, Failure_Idx).Next_State)
         with SPARK_Mode,
            Pre => Matrix (From_State, Failure_Idx).Valid = Valid_State,
            Post => Get_CS_Failure_Link'Result in CS_State'Range;
      
      -------------------------------------------------------------------------
      --  Get_CI_Failure_Link
      -------------------------------------------------------------------------
      function Get_CI_Failure_Link (
         Matrix     : CI_Matrix;
         From_State : CI_State) return CI_State
            is (Matrix (From_State, Failure_Idx).Next_State)
         with SPARK_Mode,
            Pre => Matrix (From_State, Failure_Idx).Valid = Valid_State,
            Post => Get_CI_Failure_Link'Result in CI_State'Range;

      -------------------------------------------------------------------------
      --  Has_Pattern_Output_CS
      --  This function checks if a given state has a pattern output in the
      --  case-sensitive automaton.
      -------------------------------------------------------------------------
      function Has_Pattern_Output_CS (
         Matrix        : CS_Matrix;
         State         : CS_State;
         Pattern_Index : Positive) return Boolean
            is (Matrix (State, Column (Pattern_Index)).Valid = Valid_Output)
         with SPARK_Mode,
              Pre => Pattern_Index in 1 .. Patterns'Length and then
                     Column (Pattern_Index) <= Column_Idx'Last;

      -------------------------------------------------------------------------
      --  Has_Pattern_Output_CI
      -------------------------------------------------------------------------
      function Has_Pattern_Output_CI (
         Matrix        : CI_Matrix;
         State         : CI_State;
         Pattern_Index : Positive) return Boolean
            is (Matrix (State, Column (Pattern_Index)).Valid = Valid_Output)
         with SPARK_Mode,
            Pre => Pattern_Index in 1 .. Patterns'Length and then
                  Column (Pattern_Index) <= Column_Idx'Last;

      -------------------------------------------------------------------------
      --
      --  Trie-building functions
      --
      -------------------------------------------------------------------------

      -------------------------------------------------------------------------
      --  Insert_CS_Pattern
      --  Insert a single case-sensitive pattern into the automaton.
      --  @param Matrix The case-sensitive automaton matrix.
      --  @param Pattern The pattern to insert.
      --  @param Pattern_Idx The index of the pattern in the Patterns array.
      --  @param Next_Available_State The next available state in the automaton
      --  @param Final_State The final state after inserting the pattern.
      -------------------------------------------------------------------------
      procedure Insert_CS_Pattern (
         Matrix                : in out CS_Matrix;
         Pattern               : String;
         Pattern_Idx           : Pattern_Array_Index;
         Next_Available_State  : in out CS_State)
         with SPARK_Mode,
            Pre => 
               Pattern_Idx in 1 .. Patterns'Length and then
               Pattern'Length > 0 and then
               Next_Available_State + CS_State (Pattern'Length)
                  <= CS_State'Last,
            Post =>
               Next_Available_State <= Next_Available_State'Old +
                  CS_State (Pattern'Length);
      
      procedure Insert_CS_Pattern (
         Matrix                : in out CS_Matrix;
         Pattern               : String;
         Pattern_Idx           : Pattern_Array_Index;
         Next_Available_State  : in out CS_State) with SPARK_Mode
      is
         Current_State : CS_State := CS_Start_State;
      begin
         for I in Pattern'Range loop
            declare
               Char : constant Character := Pattern (I);
               Existing_Transition : constant CS_Transition :=
                  Get_CS_Character_Transition (Matrix,
                                               Current_State,
                                               Char);
            begin
               if Existing_Transition.Valid = Valid_State then
                  --  Transition already exists, reuse it.
                  Current_State := Existing_Transition.Next_State;       
               else
                  --  Create a new transition for this character
                  pragma Assert (Next_Available_State < CS_State'Last);

                  Set_CS_Character_Transition (Matrix,
                                               Current_State,
                                               Char,
                                               Next_Available_State);

                  Current_State        := Next_Available_State;
                  Next_Available_State := Next_Available_State + 1;
               end if;
            end;
         end loop;

         --  Mark this state as matching the pattern
         Set_CS_Pattern_Output (Matrix, Current_State, Pattern_Idx, True);
      end Insert_CS_Pattern;

      -------------------------------------------------------------------------
      --  Insert_CI_Pattern
      --  Insert a single case-insensitive pattern into the automaton.
      --  @param Matrix The case-insensitive automaton matrix.
      --  @param Pattern The pattern to insert.
      --  @param Pattern_Idx The index of the pattern in the Patterns array.
      --  @param Next_Available_State The next available state in the automaton
      -------------------------------------------------------------------------
      procedure Insert_CI_Pattern (
         Matrix                : in out CI_Matrix;
         Pattern               : String;
         Pattern_Idx           : Pattern_Array_Index;
         Next_Available_State  : in out CI_State)
         with SPARK_Mode,
            Pre => 
               Pattern_Idx in 1 .. Patterns'Length and then
               Pattern'Length > 0 and then
               Next_Available_State + CI_State (Pattern'Length)
                  <= CI_State'Last,
            Post =>
               Next_Available_State <= Next_Available_State'Old +
                  CI_State (Pattern'Length);
      
      procedure Insert_CI_Pattern (
         Matrix                : in out CI_Matrix;
         Pattern               : String;
         Pattern_Idx           : Pattern_Array_Index;
         Next_Available_State  : in out CI_State) with SPARK_Mode
      is
         Current_State : CI_State := CI_Start_State;
      begin
         for I in Pattern'Range loop
            declare
               Char : constant Character := To_Lower (Pattern (I));
               Existing_Transition : constant CI_Transition :=
                  Get_CI_Character_Transition (Matrix,
                                               Current_State,
                                               Char);
            begin
               if Existing_Transition.Valid = Valid_State then
                  --  Transition already exists, reuse it.
                  Current_State := Existing_Transition.Next_State;       
               else
                  --  Create a new transition for this character
                  pragma Assert (Next_Available_State < CI_State'Last);

                  Set_CI_Character_Transition (Matrix,
                                               Current_State,
                                               Char,
                                               Next_Available_State);

                  Current_State        := Next_Available_State;
                  Next_Available_State := Next_Available_State + 1;
               end if;
            end;
         end loop;

         --  Mark this state as matching the pattern
         Set_CI_Pattern_Output (Matrix, Current_State, Pattern_Idx, True);
      end Insert_CI_Pattern;

      -------------------------------------------------------------------------
      --  Build_CS_Trie
      --  This function builds the case-sensitive trie for the automaton
      --  based on the provided patterns.
      -------------------------------------------------------------------------
      procedure Build_CS_Trie (Matrix   : in out CS_Matrix;
                               Patterns : Pattern_Array)
         with SPARK_Mode,
            Pre =>
               (for all I in Patterns'Range =>
                  Patterns(I) /= null and then
                  Patterns(I).Pattern /= null and then
                  Patterns(I).Pattern'Length > 0);
      
      procedure Build_CS_Trie (Matrix   : in out CS_Matrix;
                               Patterns : Pattern_Array) with SPARK_Mode
      is
         Next_State  : CS_State := 1;   --  Start after initial state
      begin
         for I in Patterns'Range loop
            pragma Loop_Invariant (Next_State >= 1);
            pragma Loop_Invariant (Next_State <= CS_State'Last);

            if Patterns(I).Nocase = Case_Sensitive then
               Insert_CS_Pattern (Matrix,
                                  Patterns (I).Pattern.all,
                                  I,
                                  Next_State);
            end if;
         end loop;
      end Build_CS_Trie;

      -------------------------------------------------------------------------
      --  Build_CI_Trie
      --  This function builds the case-sensitive trie for the automaton
      --  based on the provided patterns.
      -------------------------------------------------------------------------
      procedure Build_CI_Trie (Matrix   : in out CI_Matrix;
                               Patterns : Pattern_Array)
         with SPARK_Mode,
            Pre =>
               (for all I in Patterns'Range =>
                  Patterns(I) /= null and then
                  Patterns(I).Pattern /= null and then
                  Patterns(I).Pattern'Length > 0);
      
      procedure Build_CI_Trie (Matrix   : in out CI_Matrix;
                               Patterns : Pattern_Array) with SPARK_Mode
      is
         Next_State  : CI_State := 1;   --  Start after initial state
      begin
         for I in Patterns'Range loop
            pragma Loop_Invariant (Next_State >= 1);
            pragma Loop_Invariant (Next_State <= CI_State'Last);

            if Patterns(I).Nocase = Case_Sensitive then
               Insert_CI_Pattern (Matrix,
                                  Patterns (I).Pattern.all,
                                  I,
                                  Next_State);
            end if;
         end loop;
      end Build_CI_Trie;

      -------------------------------------------------------------------------
      --  Build_CS_Failure_Links
      -------------------------------------------------------------------------
      procedure Build_CS_Failure_Links (Matrix : in out CS_Matrix)
         with SPARK_Mode
      is
         package Queue_Package is new
            SPARK.Containers.Formal.Doubly_Linked_Lists (
               Element_Type => CS_State);
         use Queue_Package;

         Queue : Queue_Package.List (
            Capacity => Ada.Containers.Count_Type (CS_Max_States));

         ----------------------------------------------------------------------
         --  Initialize_Depth1_Failure_Links
         --  This procedure initializes the failure links for depth 1 states
         --  in the case-sensitive automaton. It sets the failure links for
         --  all characters that have transitions from the start state.
         ----------------------------------------------------------------------
         procedure Initialize_Depth1_Failure_Links (Matrix : in out CS_Matrix)
         with Pre => Queue.Is_Empty and then
                     Matrix'Length = CS_Max_States and then
                     Matrix (CS_Start_State, Failure_Idx).Valid =
                       Uninitialized;

         procedure Initialize_Depth1_Failure_Links (Matrix : in out CS_Matrix)
         is
            begin
               --  Initialize the failure links for depth 1 states
               for C in Character'Range loop
                  declare
                     T : constant CS_Transition :=
                        Get_CS_Character_Transition (
                           Matrix     => Matrix,
                           From_State => CS_Start_State,
                           Char       => C);
                  begin
                     if T.Valid = Valid_State then
                        --  Set the failure link for this character
                        Set_CS_Failure_Link (Matrix,
                                             T.Next_State,
                                             CS_Start_State);
                        Queue.Append (T.Next_State);
                     end if;
                  end;
               end loop;
            end Initialize_Depth1_Failure_Links;

            -------------------------------------------------------------------
            --  Find_Failure_Link
            --  This function finds the failure link for a given state and
            --  character, updating the matrix accordingly.
            -------------------------------------------------------------------
            procedure Find_Failure_Link (
               Matrix        : in out CS_Matrix;
               From_State    : CS_State;
               To_State      : CS_State;
               Char          : Character)
            with 
               Pre =>
                  Matrix (From_State, Failure_Idx).Valid = Valid_State,
               Post =>
                  Matrix (To_State, Failure_Idx).Valid = Valid_State;

            procedure Find_Failure_Link (
               Matrix        : in out CS_Matrix;
               From_State    : CS_State;
               To_State      : CS_State;
               Char          : Character)
            is
               B : CS_State := Get_CS_Failure_Link (Matrix, From_State);
               Found_Failure : Boolean := False;
            begin
               --  Follow failure links until we find a valid transition
               --  or reach the start state. For provability, we will bound the
               --  loop by the maximum possible states
               for I in 1 .. CS_State'Last loop
                  exit when B = CS_Start_State or else Found_Failure;

                  declare
                     BT : constant CS_Transition :=
                        Get_CS_Character_Transition (
                           Matrix     => Matrix,
                           From_State => B,
                           Char       => Char);
                  begin
                     if BT.Valid = Valid_State then
                        --  Found a valid transition, set the failure link
                        Set_CS_Failure_Link (Matrix, To_State, BT.Next_State);
                        Found_Failure := True;
                     else
                        --  Follow the failure link to the next state
                        B := Get_CS_Failure_Link (Matrix, B);
                     end if;
                  end;
               end loop;

               if not Found_Failure then
                  -- We either reached root (B = CS_Start_State) or hit
                  -- iteration limit. In both cases, check if root has a
                  -- transition on this character. This should never happen.
                  declare
                     Root_Trans : constant CS_Transition :=
                        Get_CS_Character_Transition (
                           Matrix, CS_Start_State, Char);
                  begin
                     if Root_Trans.Valid = Valid_State then
                        -- Root has a transition on this character
                        Set_CS_Failure_Link (
                           Matrix, To_State, Root_Trans.Next_State);
                     else
                        -- No transition anywhere for this character -
                        -- failure link points to root
                        Set_CS_Failure_Link (
                           Matrix, To_State, CS_Start_State);
                     end if;
                  end;
               end if;               
            end Find_Failure_Link;

            -------------------------------------------------------------------
            --  Process_State_For_Failure_Links
            --  This procedure processes a state to set its failure links
            --  based on the transitions from its failure link.
            -------------------------------------------------------------------
            procedure Process_State_For_Failure_Links (
               Matrix        : in out CS_Matrix;
               Current_State : CS_State)
            with Pre =>
               not Queue.Is_Empty and then
               Matrix (Current_State, Failure_Idx).Valid = Valid_State;
             
            procedure Process_State_For_Failure_Links (
               Matrix        : in out CS_Matrix;
               Current_State : CS_State)
            is
            begin
               for C in Character'Range loop
                  declare
                     T : constant CS_Transition :=
                        Get_CS_Character_Transition (
                           Matrix     => Matrix,
                           From_State => Current_State,
                           Char       => C);
                  begin
                     if T.Valid = Valid_State then
                        --  Found next state, compute its failure link
                        Find_Failure_Link (
                           Matrix, Current_State, T.Next_State, C);
                        
                        --  Add the next state to the queue for processing
                        Queue.Append (T.Next_State);
                     end if;
                  end;
               end loop;
            end Process_State_For_Failure_Links;

         begin
            Queue.Clear;
            Initialize_Depth1_Failure_Links (Matrix);

            while not Queue.Is_Empty loop
               declare
                  Current_State : CS_State := Queue.First_Element;
               begin
                  pragma Assert (not Queue.Is_Empty);

                  Queue.Delete_First;
                  Process_State_For_Failure_Links (Matrix, Current_State);            
               end;
            end loop;
         end Build_CS_Failure_Links;

      -------------------------------------------------------------------------
      --  Build_CI_Failure_Links
      -------------------------------------------------------------------------
      procedure Build_CI_Failure_Links (Matrix : in out CI_Matrix)
         with SPARK_Mode
      is
         package Queue_Package is new
            SPARK.Containers.Formal.Doubly_Linked_Lists (
               Element_Type => CI_State);
         use Queue_Package;

         Queue : Queue_Package.List (
            Capacity => Ada.Containers.Count_Type (CI_Max_States));

         ----------------------------------------------------------------------
         --  Initialize_Depth1_Failure_Links
         --  This procedure initializes the failure links for depth 1 states
         --  in the case-sensitive automaton. It sets the failure links for
         --  all characters that have transitions from the start state.
         ----------------------------------------------------------------------
         procedure Initialize_Depth1_Failure_Links (Matrix : in out CI_Matrix)
         with Pre => Queue.Is_Empty and then
                     Matrix'Length = CI_Max_States and then
                     Matrix (CI_Start_State, Failure_Idx).Valid =
                       Uninitialized;

         procedure Initialize_Depth1_Failure_Links (Matrix : in out CI_Matrix)
         is
            begin
               --  Initialize the failure links for depth 1 states
               for C in Character'Range loop
                  declare
                     T : constant CI_Transition :=
                        Get_CI_Character_Transition (
                           Matrix     => Matrix,
                           From_State => CI_Start_State,
                           Char       => C);
                  begin
                     if T.Valid = Valid_State then
                        --  Set the failure link for this character
                        Set_CI_Failure_Link (Matrix,
                                             T.Next_State,
                                             CI_Start_State);
                        Queue.Append (T.Next_State);
                     end if;
                  end;
               end loop;
            end Initialize_Depth1_Failure_Links;

            -------------------------------------------------------------------
            --  Find_Failure_Link
            --  This function finds the failure link for a given state and
            --  character, updating the matrix accordingly.
            -------------------------------------------------------------------
            procedure Find_Failure_Link (
               Matrix        : in out CI_Matrix;
               From_State    : CI_State;
               To_State      : CI_State;
               Char          : Character)
            with 
               Pre =>
                  Matrix (From_State, Failure_Idx).Valid = Valid_State,
               Post =>
                  Matrix (To_State, Failure_Idx).Valid = Valid_State;

            procedure Find_Failure_Link (
               Matrix        : in out CI_Matrix;
               From_State    : CI_State;
               To_State      : CI_State;
               Char          : Character)
            is
               B : CI_State := Get_CI_Failure_Link (Matrix, From_State);
               Found_Failure : Boolean := False;
            begin
               --  Follow failure links until we find a valid transition
               --  or reach the start state. For provability, we will bound the
               --  loop by the maximum possible states
               for I in 1 .. CI_State'Last loop
                  exit when B = CI_Start_State or else Found_Failure;

                  declare
                     BT : constant CI_Transition :=
                        Get_CI_Character_Transition (
                           Matrix     => Matrix,
                           From_State => B,
                           Char       => Char);
                  begin
                     if BT.Valid = Valid_State then
                        --  Found a valid transition, set the failure link
                        Set_CI_Failure_Link (Matrix, To_State, BT.Next_State);
                        Found_Failure := True;
                     else
                        --  Follow the failure link to the next state
                        B := Get_CI_Failure_Link (Matrix, B);
                     end if;
                  end;
               end loop;

               if not Found_Failure then
                  -- We either reached root (B = CI_Start_State) or hit
                  -- iteration limit. In both cases, check if root has a
                  -- transition on this character. This should never happen.
                  declare
                     Root_Trans : constant CI_Transition :=
                        Get_CI_Character_Transition (
                           Matrix, CI_Start_State, Char);
                  begin
                     if Root_Trans.Valid = Valid_State then
                        -- Root has a transition on this character
                        Set_CI_Failure_Link (
                           Matrix, To_State, Root_Trans.Next_State);
                     else
                        -- No transition anywhere for this character -
                        -- failure link points to root
                        Set_CI_Failure_Link (
                           Matrix, To_State, CI_Start_State);
                     end if;
                  end;
               end if;               
            end Find_Failure_Link;

            -------------------------------------------------------------------
            --  Process_State_For_Failure_Links
            --  This procedure processes a state to set its failure links
            --  based on the transitions from its failure link.
            -------------------------------------------------------------------
            procedure Process_State_For_Failure_Links (
               Matrix        : in out CI_Matrix;
               Current_State : CI_State)
            with Pre =>
               not Queue.Is_Empty and then
               Matrix (Current_State, Failure_Idx).Valid = Valid_State;
             
            procedure Process_State_For_Failure_Links (
               Matrix        : in out CI_Matrix;
               Current_State : CI_State)
            is
            begin
               for C in Character'Range loop
                  declare
                     T : constant CI_Transition :=
                        Get_CI_Character_Transition (
                           Matrix     => Matrix,
                           From_State => Current_State,
                           Char       => C);
                  begin
                     if T.Valid = Valid_State then
                        --  Found next state, compute its failure link
                        Find_Failure_Link (
                           Matrix, Current_State, T.Next_State, C);
                        
                        --  Add the next state to the queue for processing
                        Queue.Append (T.Next_State);
                     end if;
                  end;
               end loop;
            end Process_State_For_Failure_Links;

         begin
            Queue.Clear;
            Initialize_Depth1_Failure_Links (Matrix);

            while not Queue.Is_Empty loop
               declare
                  Current_State : CI_State := Queue.First_Element;
               begin
                  pragma Assert (not Queue.Is_Empty);

                  Queue.Delete_First;
                  Process_State_For_Failure_Links (Matrix, Current_State);            
               end;
            end loop;
         end Build_CI_Failure_Links;

      -------------------------------------------------------------------------
      --  Build_Automaton
      --  This function initializes the automaton matrices for both
      --  case-sensitive and case-insensitive patterns.
      -------------------------------------------------------------------------
      function Build_Automaton (Patterns : Pattern_Array) return Automaton
         with SPARK_Mode
      is
         CS_States : CS_Matrix;
         CI_States : CI_Matrix;
      begin
         --  CS_Operations.Set_Character_Transition
         --    (Matrix     => CS_States,
         --     From_State => CS_Start_State,
         --     Char       => ' ',
         --     To_State   => CS_Start_State);

         return Automaton'
           (CS_States        => CS_States,
            CI_States        => CI_States,
            CS_Current_State => CS_Start_State,
            CI_Current_State => CI_Start_State,
            Initialized      => True,
            Stream_Idx       => 1);
      end Build_Automaton;

   end Automatons;

end Aho_Corasick;
