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
         (Output_Start_Idx + Column_Idx (Pattern_Idx) - 1) with SPARK_Mode;

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
