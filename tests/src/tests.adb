-------------------------------------------------------------------------------
--  tests.adb
--
--  Copyright (c) 2025, High Confidence / Jon Andrew
--  All rights reserved.
--
--  Test suite for the Aho-Corasick pattern matching library
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------------------------
with Ada.Real_Time; use Ada.Real_Time;
with Aho_Corasick; use Aho_Corasick;
with Ada.Text_IO; use Ada.Text_IO;

procedure Tests is
   Success_Count : Natural := 0;
   Total_Tests   : Natural := 0;

   procedure Assert (Condition : Boolean; Test_Name : String) is
   begin
      Total_Tests := Total_Tests + 1;
      if Condition then
         Success_Count := Success_Count + 1;
         Put_Line ("Pass: " & Test_Name);
      else
         Put_Line ("Fail: " & Test_Name);
      end if;
   end Assert;

   ----------------------------------------------------------------------------
   --  Test_Integer_Options
   --  Ensure integer options are correctly defined and user-defined literals
   --  function properly
   ----------------------------------------------------------------------------
   procedure Test_Integer_Options is
      --  Test integer options
      Option1 : constant Integer_Option := 42;
      Option2 : constant Integer_Option := 100;
      Option3 : constant Integer_Option := No_Value;
      Option4 : constant Integer_Option := 69;

   begin
      Put_Line ("=== Testing Integer_Option Type w/ Literals ===");

      Assert (Option1 = (Just, 42),
              "Integer_Option 1 defined correctly");
      Assert (Option2 = (Just, 100),
              "Integer_Option 2 defined correctly");
      Assert (Option3 = (O => None),
              "Integer_Option 3 (No_Value) defined correctly");
      Assert (Option3 = No_Value,
              "Integer_Option 3 (No_Value) matches No_Value constant");
      Assert (Option4 = 69,
              "Integer_Option 4 compared correctly");
   end Test_Integer_Options;

   procedure Test_State_Counting is
      --  Test state counting functionality
      Patterns : constant Pattern_Array (1 .. 3) :=
        [new Enhanced_Pattern'(Pattern => new String'("abc"), others => <>),
         new Enhanced_Pattern'(Pattern => new String'("def"), others => <>),
         new Enhanced_Pattern'(Pattern => new String'("ghi"), others => <>)];
      
      Max_States : constant Natural :=
         Get_Max_States (Patterns, Case_Sensitive);
   begin
      Put_Line ("=== Testing State Counting ===");

      Put_Line ("Max states for 3 patterns: " & Max_States'Image);
      Assert (Max_States = 10, "Max states for 3 patterns is 10");

      --  Check if the state count matches expected values
      --  For 3 patterns, we expect:
      --  - 'abc' adds 3 states (a, b, c)
      --  - 'def' adds 3 more states (d, e, f)
      --  - 'ghi' adds 3 more states (g, h, i)
      --  - Plus the root state
   end Test_State_Counting;

   ----------------------------------------------------------------------------
   --  Test case 1: Simple pattern matching
   ----------------------------------------------------------------------------
   procedure Basic_Test_1 is
      Patterns : constant Aho_Corasick.Pattern_Array (1 .. 2) :=
        [new Enhanced_Pattern'(Pattern => new String'("abc"), others => <>),
         new Enhanced_Pattern'(Pattern => new String'("def"), others => <>)];
      Matches  : Match_Array (Patterns'Range);

      package Matcher is new Aho_Corasick.Automatons (Patterns);
      use Matcher;

      T : Automaton := Build_Automaton (Patterns);
      Text     : constant String := "abcdefghi";
   begin
      Put_Line ("=== Testing Basic Pattern Matching ===");

      Find_Matches (T, Patterns, Matches, Text);

      for Match of Matches loop
         if Match.EP /= null then
            Ada.Text_IO.Put_Line ("Found pattern: " & Match.EP.Pattern.all &
                                  " at positions " &
                                  Match.Start_Position'Image & " to "
                                  & Match.End_Position'Image);
         end if;
      end loop;

      Assert (Matches (1).EP.Pattern /= null, "'abc' found in Basic_Test_1");
      Assert (Matches (2).EP.Pattern /= null, "'def' found in Basic_Test_1");
   end Basic_Test_1;

   --  ----------------------------------------------------------------------------
   --  --  Test case 1b: Case-insensitive pattern matching
   --  ----------------------------------------------------------------------------
   --  procedure Basic_Test_1_Nocase is
   --     Patterns : constant Aho_Corasick.Pattern_Array (1 .. 2) :=
   --       [new Enhanced_Pattern'(Pattern => new String'("ABC"),
   --                              Nocase  => Case_Insensitive,
   --                              others => <>),
   --        new Enhanced_Pattern'(Pattern => new String'("dEf"),
   --                              Nocase  => Case_Insensitive,
   --                              others => <>)];

   --     T : Aho_Corasick.Automaton := Aho_Corasick.Build_Automaton (Patterns);
   --     Matches  : Aho_Corasick.Match_Array (Patterns'Range);
   --     Text     : constant String := "abcdefghi";
   --  begin
   --     Put_Line ("=== Testing Case-Insensitive Patterns ===");

   --     Aho_Corasick.Find_Matches (T, Patterns, Matches, Text);

   --     for Match of Matches loop
   --        if Match.EP /= null then
   --           Ada.Text_IO.Put_Line ("Found pattern: " & Match.EP.Pattern.all &
   --                                 " at positions " &
   --                                 Match.Start_Position'Image & " to "
   --                                 & Match.End_Position'Image);
   --        end if;
   --     end loop;

   --     Assert (Matches (1).EP.Pattern /= null,
   --        "'ABC' found (case-insensitive) in Basic_Test_1b");
   --     Assert (Matches (2).EP.Pattern /= null,
   --        "'dEf' found (case-insensitive) in Basic_Test_1b");
   --  end Basic_Test_1_Nocase;

   --  ----------------------------------------------------------------------------
   --  --  Test case 2: No matches
   --  ----------------------------------------------------------------------------
   --  procedure Basic_Test_2 is
   --     Patterns : constant Aho_Corasick.Pattern_Array (1 .. 1) :=
   --       [new Enhanced_Pattern'(Pattern => new String'("xyz"),
   --                              Nocase  => Case_Sensitive,
   --                              others => <>)];
   --     T : Aho_Corasick.Automaton := Aho_Corasick.Build_Automaton (Patterns);
   --     Matches  : Aho_Corasick.Match_Array (Patterns'Range);
   --     Text     : constant String := "abcdefabc";
   --  begin
   --     Put_Line ("=== Testing No Matches ===");

   --     Aho_Corasick.Find_Matches (T, Patterns, Matches, Text);

   --     for Match of Matches loop
   --        if Match.EP /= null then
   --           Ada.Text_IO.Put_Line ("Found pattern: " & Match.EP.Pattern.all &
   --                                 " at positions " &
   --                                 Match.Start_Position'Image & " to "
   --                                 & Match.End_Position'Image);
   --        else
   --           Ada.Text_IO.Put_Line ("No matches found.");
   --        end if;
   --     end loop;

   --     Assert (Matches (1).EP = null,
   --        "No matches found in Basic_Test_2");
   --  end Basic_Test_2;

   --  ----------------------------------------------------------------------------
   --  --  Test case 3: matches after updating the text
   --  ----------------------------------------------------------------------------
   --  procedure Basic_Test_3 is
   --     Patterns : constant Aho_Corasick.Pattern_Array (1 .. 1) :=
   --       [new Enhanced_Pattern'(Pattern => new String'("xyz"),
   --                              others => <>)];
   --     T : Aho_Corasick.Automaton := Aho_Corasick.Build_Automaton (Patterns);
   --     Matches  : Aho_Corasick.Match_Array (Patterns'Range);
   --     Text     : constant String := "abcdefabcx";
   --     Text2    : constant String := "yzabcdefabc";
   --  begin
   --     Put_Line ("=== Testing Matches after streaming ===");

   --     Aho_Corasick.Find_Matches (T, Patterns, Matches, Text);

   --     for Match of Matches loop
   --        if Match.EP /= null then
   --           Ada.Text_IO.Put_Line ("Found pattern: " & Match.EP.Pattern.all &
   --                                 " at positions " &
   --                                 Match.Start_Position'Image & " to "
   --                                 & Match.End_Position'Image);
   --        else
   --           Ada.Text_IO.Put_Line ("No matches found.");
   --        end if;
   --     end loop;

   --     Assert (Matches (1).EP = null,
   --             "No matches found in Basic_Test_3 before streaming");

   --     --  Now find matches in the updated text
   --     Aho_Corasick.Find_Matches (T, Patterns, Matches, Text2);

   --     for Match of Matches loop
   --        if Match.EP /= null then
   --           Ada.Text_IO.Put_Line ("After streaming, found pattern: " &
   --                                 Match.EP.Pattern.all &
   --                                 " at positions " &
   --                                 Match.Start_Position'Image & " to "
   --                                 & Match.End_Position'Image);
   --        else
   --           Ada.Text_IO.Put_Line ("No matches found after streaming.");
   --        end if;
   --     end loop;

   --     Assert (Matches (1).EP /= null,
   --             "Found 'xyz' in Basic_Test_3 after streaming");
   --  end Basic_Test_3;

   --  ----------------------------------------------------------------------------
   --  --  Test_Mixed_Case_Patterns
   --  ----------------------------------------------------------------------------
   --  procedure Test_Mixed_Case_Patterns is
   --     Patterns : constant Aho_Corasick.Pattern_Array (1 .. 2) :=
   --       [new Enhanced_Pattern'(Pattern => new String'("ABC"),
   --                              Nocase  => Case_Insensitive,
   --                              others => <>),
   --        new Enhanced_Pattern'(Pattern => new String'("dEf"),
   --                              Nocase  => Case_Sensitive,
   --                              others => <>)];

   --     T : Aho_Corasick.Automaton := Aho_Corasick.Build_Automaton (Patterns);
   --     Matches  : Aho_Corasick.Match_Array (Patterns'Range);
   --     Text     : constant String := "abcdEfghi";
   --  begin
   --     Put_Line ("=== Testing Mixed Case Patterns ===");

   --     Aho_Corasick.Find_Matches (T, Patterns, Matches, Text);

   --     for Match of Matches loop
   --        if Match.EP /= null then
   --           Ada.Text_IO.Put_Line ("Found pattern: " & Match.EP.Pattern.all &
   --                                 " at positions " &
   --                                 Match.Start_Position'Image & " to "
   --                                 & Match.End_Position'Image);
   --        end if;
   --     end loop;

   --     Assert (Matches (1).EP.Pattern /= null,
   --        "'ABC' found (case-insensitive) in Test_Mixed_Case_Patterns");
   --     Assert (Matches (2).EP.Pattern /= null,
   --        "'dEf' found (case-sensitive) in Test_Mixed_Case_Patterns");
   --  end Test_Mixed_Case_Patterns;

   --  ----------------------------------------------------------------------------
   --  --  Test_Performance_vs_Naive
   --  --  Compare against naive string search for performance validation
   --  ----------------------------------------------------------------------------
   --  procedure Test_Performance_vs_Naive is
   --     --  Large text simulation (network packet payload)
   --     Text : constant String := [1 .. 8192 => 'A'] &
   --                               "GET /admin/config.php?cmd=ls" &
   --                               [1 .. 4096 => 'B'] &
   --                               "javascript:alert('xss')" &
   --                               [1 .. 2048 => 'C'];

   --     --  Security patterns commonly found in IDS rules
   --     Pattern1 : aliased constant String := "admin";
   --     Pattern2 : aliased constant String := "config.php";
   --     Pattern3 : aliased constant String := "javascript:";
   --     Pattern4 : aliased constant String := "alert(";
   --     Pattern5 : aliased constant String := "cmd=";

   --     Patterns : constant Pattern_Array :=
   --        [new Enhanced_Pattern'(
   --           Pattern1'Unchecked_Access,
   --           Nocase => Case_Sensitive,
   --           others => <>),
   --         new Enhanced_Pattern'(
   --           Pattern2'Unchecked_Access,
   --           Nocase => Case_Sensitive,
   --           others => <>),
   --         new Enhanced_Pattern'(
   --           Pattern3'Unchecked_Access,
   --           Nocase => Case_Sensitive,
   --           others => <>),
   --         new Enhanced_Pattern'(
   --           Pattern4'Unchecked_Access,
   --           Nocase => Case_Sensitive,
   --           others => <>),
   --         new Enhanced_Pattern'(
   --           Pattern5'Unchecked_Access,
   --           Nocase => Case_Sensitive,
   --           others => <>)
   --        ];

   --     A : Automaton := Build_Automaton (Patterns);
   --     Matches : Match_Array (1 .. 20);

   --     Start_Time, End_Time : Time;
   --     AC_Duration : Time_Span;

   --     --  Performance variables
   --     Iterations : constant := 1000;
   --  begin
   --     Put_Line ("=== Testing Performance ===");

   --     --  Warm up caches
   --     for I in 1 .. 10 loop
   --        A.Reset;
   --        Find_Matches (A, Patterns, Matches, Text);
   --     end loop;

   --     --  Time optimized Aho-Corasick
   --     Start_Time := Clock;
   --     for I in 1 .. Iterations loop
   --        A.Reset;
   --        Find_Matches (A, Patterns, Matches, Text);
   --     end loop;

   --     End_Time := Clock;
   --     AC_Duration := End_Time - Start_Time;

   --     Put_Line ("Aho-Corasick " & Iterations'Image & " iterations: " &
   --               Duration'Image (To_Duration (AC_Duration)) & " seconds");

   --     --  Verify correctness
   --     A.Reset;
   --     Find_Matches (A, Patterns, Matches, Text);

   --     --  Should find all 5 patterns
   --     Assert (Matches (1).EP.Pattern /= null,
   --        "Performance test - pattern 1 found");
   --     Assert (Matches (2).EP.Pattern /= null,
   --        "Performance test - pattern 2 found");
   --     Assert (Matches (3).EP.Pattern /= null,
   --        "Performance test - pattern 3 found");
   --     Assert (Matches (4).EP.Pattern /= null,
   --        "Performance test - pattern 4 found");
   --     Assert (Matches (5).EP.Pattern /= null,
   --        "Performance test - pattern 5 found");

   --     --  Performance requirement: < 3ms for 14KB text with 5 patterns
   --     --  No hard and fast rule here, and benchmarks can vary depending
   --     --  on system, but we can use this to test relative performance
   --     --  as we make changes.
   --     Assert (To_Duration (AC_Duration) < 0.3,
   --              "Performance test - fast enough for real-time IDS");
   --  end Test_Performance_vs_Naive;

   --  ----------------------------------------------------------------------------
   --  --  Test_High_Pattern_Count
   --  --  Test with many patterns (typical IDS rule set)
   --  ----------------------------------------------------------------------------
   --  procedure Test_High_Pattern_Count is

   --     --  Helper to create pattern strings
   --     function Make_Pattern (Index : Natural) return Enhanced_Pattern_Access is
   --        Pattern_Str : constant String := "pattern_" &
   --           (if Index < 10 then "00" & Index'Image (2 .. Index'Image'Last)
   --            elsif Index < 100 then "0" & Index'Image (2 .. Index'Image'Last)
   --            else Index'Image (2 .. Index'Image'Last));
   --     begin
   --        return new Enhanced_Pattern'(Pattern => new String'(Pattern_Str),
   --                Nocase  => Case_Sensitive,
   --                others => <>);
   --     end Make_Pattern;

   --     Pattern_Ptrs : constant Pattern_Array (1 .. 200) := [for J in 1 .. 200 =>
   --        Make_Pattern (J)];

   --     Text : constant String :=
   --        "This text contains pattern050 and pattern150 to test";
   --     Matches : Match_Array (1 .. 50);

   --     Start_Time, End_Time : Time;
   --  begin
   --     Put_Line ("=== Testing High Pattern Count ===");

   --     --  Build automaton
   --     Start_Time := Clock;

   --     declare
   --        A : Automaton := Build_Automaton (Pattern_Ptrs);
   --     begin

   --        End_Time := Clock;

   --        Put_Line ("Built automaton with 200 patterns in: " &
   --                 Duration'Image (To_Duration (End_Time - Start_Time)) &
   --                 " seconds");

   --        --  Test search performance
   --        Start_Time := Clock;
   --        Find_Matches (A, Pattern_Ptrs, Matches, Text);
   --        End_Time := Clock;

   --        Put_Line ("Search with 200 patterns in: " &
   --                 Duration'Image (To_Duration (End_Time - Start_Time)) &
   --                 " seconds");

   --        Assert (To_Duration (End_Time - Start_Time) < 0.001,
   --              "High pattern count - search under 1ms");
   --     end;
   --  end Test_High_Pattern_Count;

   --  -------------------------------------------------------------------------
   --  --  Test_Security_Evasion_Resistance
   --  --  Test against common evasion techniques
   --  -------------------------------------------------------------------------
   --  procedure Test_Security_Evasion_Resistance is
   --     Pattern1 : aliased String := "script";
   --     Pattern2 : aliased String := "eval(";
   --     Pattern3 : aliased String := "document.cookie";

   --     Patterns : constant Pattern_Array :=
   --        [new Enhanced_Pattern'(
   --           Pattern => Pattern1'Unchecked_Access,
   --           Nocase  => Case_Sensitive,
   --           others => <>),
   --         new Enhanced_Pattern'(
   --           Pattern => Pattern2'Unchecked_Access,
   --           Nocase  => Case_Sensitive,
   --           others => <>),
   --         new Enhanced_Pattern'(
   --           Pattern => Pattern3'Unchecked_Access,
   --           Nocase  => Case_Sensitive,
   --            others => <>)
   --        ];

   --     A : Automaton := Build_Automaton (Patterns);
   --     Matches : Match_Array (1 .. 10);

   --     --  Various evasion attempts
   --     Evasion1 : constant String :=
   --        "<script>alert(1)</script>";  -- Basic
   --     Evasion2 : constant String :=
   --        "eval(String.fromCharCode(97,108,101,114,116))"; -- Encoded
   --     Evasion3 : constant String := "x=document.cookie; steal(x);";

   --     Match_Count : Natural;
   --  begin
   --     Put_Line ("=== Testing Security Evasion Resistance ===");

   --     --  Test each evasion technique
   --     Find_Matches (A, Patterns, Matches, Evasion1);

   --     Match_Count := 0;

   --     for I in Matches'Range loop
   --        exit when Matches (I).EP = null;
   --        Match_Count := Match_Count + 1;
   --     end loop;

   --     Assert (Match_Count >= 1,
   --        "Evasion resistance - basic script tag detected");

   --     --  Reset for next test
   --     A.Reset;

   --     Find_Matches (A, Patterns, Matches, Evasion2);

   --     Match_Count := 0;

   --     for I in Matches'Range loop
   --        exit when Matches (I).EP = null;
   --        Match_Count := Match_Count + 1;
   --     end loop;

   --     Assert (Match_Count >= 1, "Evasion resistance - eval detected");

   --     --  Reset for next test
   --     A.Reset;

   --     Find_Matches (A, Patterns, Matches, Evasion3);

   --     Match_Count := 0;

   --     for I in Matches'Range loop
   --        exit when Matches (I).EP = null;
   --        Match_Count := Match_Count + 1;
   --     end loop;

   --     Assert (Match_Count >= 1, "Evasion resistance - steal detected");

   --  end Test_Security_Evasion_Resistance;

   --  -------------------------------------------------------------------------
   --  --  Test_Memory_Safety
   --  --  Ensure no buffer overflows or memory corruption
   --  -------------------------------------------------------------------------
   --  procedure Test_Memory_Safety is
   --     Pattern1 : aliased String := "A";
   --     EPattern1 : aliased Enhanced_Pattern :=
   --        (Pattern => Pattern1'Unchecked_Access,
   --         Nocase  => Case_Sensitive,
   --         others => <>);
   --     Patterns : constant Pattern_Array := [
   --        1 => EPattern1'Unchecked_Access];

   --     A : Automaton := Build_Automaton (Patterns);
   --     Matches : Match_Array (1 .. 5);

   --     --  Test with various edge cases
   --     Empty_Text : constant String := "";
   --     Single_Char : constant String := "A";
   --     Large_Text : constant String := [1 .. 10000 => 'A'];

   --  begin
   --     Put_Line ("=== Testing Memory Safety ===");
   --     Find_Matches (A, Patterns, Matches, Empty_Text);
   --     Assert (True, "Memory safety - empty text handled");

   --     --  Single character
   --     A.Reset;
   --     Find_Matches (A, Patterns, Matches, Single_Char);
   --     Assert (Matches (1).EP.Pattern /= null,
   --        "Memory safety - single char match");

   --     --  Large text
   --     A.Reset;
   --     Find_Matches (A, Patterns, Matches, Large_Text);
   --     Assert (True, "Memory safety - large text handled");
   --  end Test_Memory_Safety;

   --  ----------------------------------------------------------------------------
   --  --  Test_Position_Modifiers_Basic
   --  --  Test basic offset, depth, distance, and within functionality
   --  ----------------------------------------------------------------------------
   --  procedure Test_Position_Modifiers_Basic is
   --     --  Test patterns with various position constraints
   --     Pattern1 : aliased constant String := "GET";
   --     Pattern2 : aliased constant String := "HTTP";
   --     Pattern3 : aliased constant String := "Host";

   --     Patterns : constant Pattern_Array :=
   --        [new Enhanced_Pattern'(
   --           Pattern => Pattern1'Unchecked_Access,
   --           Nocase  => Case_Sensitive,
   --           Offset  => 0,             --  Must start at position 0
   --           Depth   => 3,             --  Must end within first 3 bytes
   --           others => <>),
   --         new Enhanced_Pattern'(
   --           Pattern => Pattern2'Unchecked_Access,
   --           Nocase  => Case_Sensitive,
   --           Distance => 1,            --  At least 1 byte after previous match
   --           Within   => 20,           --  Within 20 bytes of previous match
   --           others => <>),
   --         new Enhanced_Pattern'(
   --           Pattern => Pattern3'Unchecked_Access,
   --           Nocase  => Case_Sensitive,
   --           Distance => 6,            --  At least 5 bytes after previous match
   --           others => <>)
   --        ];

   --     A : Automaton := Build_Automaton (Patterns);
   --     Matches : Match_Array (1 .. 10);

   --     --  Valid HTTP request structure
   --     Valid_Text : constant String := "GET /page HTTP/1.1" & ASCII.CR &
   --                                      ASCII.LF & "Host: example.com";

   --     --  Invalid: GET not at start
   --     Invalid_Text1 : constant String := "   GET /page HTTP/1.1";

   --     --  Invalid: HTTP too far from GET
   --     Invalid_Text2 : constant String := "GET" & [1 .. 50 => 'X'] & "HTTP/1.1";

   --     --  Invalid: Host too close to HTTP
   --     Invalid_Text3 : constant String := "GET /page HTTP/1.1Host: example.com";

   --     Empty_Match : constant Match := (EP => null,
   --        Start_Position => No_Value, End_Position => No_Value);
   --  begin
   --     Put_Line ("=== Testing Position Modifiers ===");

   --     --  Test 1: Valid case - should find all patterns
   --     Find_Matches (A, Patterns, Matches, Valid_Text);

   --     Put_Line ("Valid case matches:");
   --     for I in Matches'Range loop
   --        if Matches (I).EP /= null then
   --           Put_Line ("  Pattern " & I'Image & ": "
   --                    & Matches (I).EP.Pattern.all
   --                    & " at " & Matches (I).Start_Position'Image
   --                    & " to " & Matches (I).End_Position'Image);
   --        end if;
   --     end loop;

   --     Assert (Matches (1).EP /= null,
   --        "Position test - GET found at correct offset");
   --     Assert (Matches (2).EP /= null,
   --        "Position test - HTTP found within constraints");
   --     Assert (Matches (3).EP /= null,
   --        "Position test - Host found at correct distance");

   --     --  Test 2: GET not at offset 0
   --     A.Reset;
   --     Matches := [others => Empty_Match];
   --     Find_Matches (A, Patterns, Matches, Invalid_Text1);

   --     Assert (Matches (1).EP = null,
   --        "Position test - GET rejected when not at offset 0");

   --     --  Test 3: HTTP too far from GET (violates within constraint)
   --     A.Reset;
   --     Matches := [others => Empty_Match];
   --     Find_Matches (A, Patterns, Matches, Invalid_Text2);

   --     Assert (Matches (2).EP = null,
   --        "Position test - HTTP rejected when too far from GET");

   --     --  Test 4: Host too close to HTTP (violates distance constraint)
   --     A.Reset;
   --     Matches := [others => Empty_Match];
   --     Find_Matches (A, Patterns, Matches, Invalid_Text3);

   --     Assert (Matches (3).EP = null,
   --        "Position test - Host rejected when too close to HTTP");
   --  end Test_Position_Modifiers_Basic;

   --  ----------------------------------------------------------------------------
   --  --  Test_Position_Modifiers_Streaming
   --  --  Test position modifiers work correctly across streaming boundaries
   --  ----------------------------------------------------------------------------
   --  procedure Test_Position_Modifiers_Streaming is
   --     Pattern1 : aliased constant String := "START";
   --     Pattern2 : aliased constant String := "MIDDLE";
   --     Pattern3 : aliased constant String := "END";

   --     Patterns : constant Pattern_Array :=
   --        [new Enhanced_Pattern'(
   --           Pattern => Pattern1'Unchecked_Access,
   --           Nocase  => Case_Sensitive,
   --           others => <>),
   --         new Enhanced_Pattern'(
   --           Pattern => Pattern2'Unchecked_Access,
   --           Nocase  => Case_Sensitive,
   --           Distance => 5,             --  At least 5 bytes after START
   --           Within   => 50,            --  Within 50 bytes of START
   --           others => <>),
   --         new Enhanced_Pattern'(
   --           Pattern => Pattern3'Unchecked_Access,
   --           Nocase  => Case_Sensitive,
   --           Distance => 3,             --  At least 3 bytes after MIDDLE
   --           others => <>)
   --        ];

   --     A : Automaton := Build_Automaton (Patterns);
   --     Matches : Match_Array (1 .. 10);

   --     --  Split across streaming chunks
   --     Chunk1 : constant String := "START data here ";
   --     Chunk2 : constant String := "more MIDDLE info ";
   --     Chunk3 : constant String := "and END result";

   --     Empty_Match : constant Match := (EP => null,
   --        Start_Position => No_Value, End_Position => No_Value);
   --  begin
   --     Put_Line ("=== Testing Position Modifiers with Streaming ===");

   --     --  Initialize matches array
   --     Matches := [others => Empty_Match];

   --     --  Process chunks sequentially
   --     Find_Matches (A, Patterns, Matches, Chunk1);
   --     Put_Line ("After chunk 1:");
   --     for I in Matches'Range loop
   --        if Matches (I).EP /= null then
   --           Put_Line ("  Found: " & Matches (I).EP.Pattern.all &
   --                    " at " & Matches (I).Start_Position'Image);
   --        end if;
   --     end loop;

   --     Find_Matches (A, Patterns, Matches, Chunk2);
   --     Put_Line ("After chunk 2:");
   --     for I in Matches'Range loop
   --        if Matches (I).EP /= null then
   --           Put_Line ("  Found: " & Matches (I).EP.Pattern.all &
   --                    " at " & Matches (I).Start_Position'Image);
   --        end if;
   --     end loop;

   --     Find_Matches (A, Patterns, Matches, Chunk3);
   --     Put_Line ("After chunk 3:");
   --     for I in Matches'Range loop
   --        if Matches (I).EP /= null then
   --           Put_Line ("  Found: " & Matches (I).EP.Pattern.all &
   --                    " at " & Matches (I).Start_Position'Image);
   --        end if;
   --     end loop;

   --     Assert (Matches (1).EP /= null,
   --        "Streaming test - START found");
   --     Assert (Matches (2).EP /= null,
   --        "Streaming test - MIDDLE found with correct distance");
   --     Assert (Matches (3).EP /= null,
   --        "Streaming test - END found with correct distance");
   --  end Test_Position_Modifiers_Streaming;

   --  ----------------------------------------------------------------------------
   --  --  Test_Position_Modifiers_SQL_Injection
   --  --  Real-world IDS scenario: SQL injection detection
   --  ----------------------------------------------------------------------------
   --  procedure Test_Position_Modifiers_SQL_Injection is
   --     Pattern1 : aliased constant String := "SELECT";
   --     Pattern2 : aliased constant String := "*";
   --     Pattern3 : aliased constant String := "FROM";
   --     Pattern4 : aliased constant String := "users";

   --     Patterns : constant Pattern_Array :=
   --        [new Enhanced_Pattern'(
   --           Pattern => Pattern1'Unchecked_Access,
   --           Nocase  => Case_Insensitive,
   --           Offset  => 0,              --  SELECT must be at start
   --           Depth   => 6,              --  Within first 6 bytes (SELECT)
   --           others => <>),
   --         new Enhanced_Pattern'(
   --           Pattern => Pattern2'Unchecked_Access,
   --           Nocase  => Case_Sensitive,
   --           Distance => 1,             --  At least 1 byte after SELECT
   --           Within   => 10,            --  Within 10 bytes of SELECT
   --           others => <>),
   --         new Enhanced_Pattern'(
   --           Pattern => Pattern3'Unchecked_Access,
   --           Nocase  => Case_Insensitive,
   --           Distance => 1,             --  At least 1 byte after *
   --           Within   => 20,            --  Within 20 bytes of *
   --           others => <>),
   --         new Enhanced_Pattern'(
   --           Pattern => Pattern4'Unchecked_Access,
   --           Nocase  => Case_Insensitive,
   --           Distance => 1,             --  At least 1 byte after FROM
   --           Within   => 30,            --  Within 30 bytes of FROM
   --           others => <>)
   --        ];

   --     A : Automaton := Build_Automaton (Patterns);
   --     Matches : Match_Array (1 .. 10);

   --     --  Valid SQL injection attempt
   --     Attack_Text : constant String :=
   --        "SELECT * FROM users WHERE id=1";

   --     --  Invalid: SELECT not at start
   --     Benign_Text1 : constant String :=
   --        "The SELECT statement can use * FROM users";

   --     --  Invalid: Wrong sequence
   --     Benign_Text2 : constant String :=
   --        "SELECT name FROM users WHERE * IN (1,2,3)";

   --     Empty_Match : constant Match := (EP => null,
   --        Start_Position => No_Value, End_Position => No_Value);
   --  begin
   --     Put_Line ("=== Testing SQL Injection Detection ===");

   --     --  Test 1: Should detect attack
   --     Matches := [others => Empty_Match];
   --     Find_Matches (A, Patterns, Matches, Attack_Text);

   --     Put_Line ("Attack text matches:");
   --     for I in Matches'Range loop
   --        if Matches (I).EP /= null then
   --           Put_Line ("  Pattern " & I'Image & ": " &
   --                     Matches (I).EP.Pattern.all);
   --        end if;
   --     end loop;

   --     Assert (Matches (1).EP /= null,
   --        "SQL injection - SELECT detected");
   --     Assert (Matches (2).EP /= null,
   --        "SQL injection - * detected in sequence");
   --     Assert (Matches (3).EP /= null,
   --        "SQL injection - FROM detected in sequence");
   --     Assert (Matches (4).EP /= null,
   --        "SQL injection - users detected in sequence");

   --     --  Test 2: Should not detect when SELECT not at start
   --     A.Reset;
   --     Matches := [others => Empty_Match];
   --     Find_Matches (A, Patterns, Matches, Benign_Text1);

   --     Assert (Matches (1).EP = null,
   --        "SQL injection - SELECT rejected when not at start");

   --     --  Test 3: Should break sequence when patterns out of order
   --     A.Reset;
   --     Matches := [others => Empty_Match];
   --     Find_Matches (A, Patterns, Matches, Benign_Text2);

   --     --  Should find SELECT, but not * and FROM in the right position
   --     Assert (Matches (1).EP /= null,
   --        "SQL injection - SELECT found");
   --     Assert (Matches (2).EP = null,
   --        "SQL injection - * rejected due to wrong sequence");
   --     Assert (Matches (3).EP = null,
   --        "SQL injection - FROM rejected due to broken sequence");
   --  end Test_Position_Modifiers_SQL_Injection;

   --  ----------------------------------------------------------------------------
   --  --  Test_Position_Modifiers_Edge_Cases
   --  --  Test edge cases and boundary conditions
   --  ----------------------------------------------------------------------------
   --  procedure Test_Position_Modifiers_Edge_Cases is
   --     Pattern1 : aliased constant String := "A";
   --     Pattern2 : aliased constant String := "B";

   --     Patterns : constant Pattern_Array :=
   --        [new Enhanced_Pattern'(
   --           Pattern => Pattern1'Unchecked_Access,
   --           Nocase  => Case_Sensitive,
   --           others => <>),
   --         new Enhanced_Pattern'(
   --           Pattern => Pattern2'Unchecked_Access,
   --           Nocase  => Case_Sensitive,
   --           Distance => 0,             --  Exactly at previous match end
   --           others => <>)
   --        ];

   --     A : Automaton := Build_Automaton (Patterns);
   --     Matches : Match_Array (1 .. 10);

   --     --  Edge case: patterns adjacent
   --     Adjacent_Text : constant String := "AB";

   --     --  Edge case: exact distance boundary
   --     Boundary_Text : constant String := "A B";  -- B is 1 byte after A ends

   --     Empty_Match : constant Match := (EP => null,
   --        Start_Position => No_Value, End_Position => No_Value);
   --  begin
   --     Put_Line ("=== Testing Position Modifier Edge Cases ===");

   --     --  Test 1: Adjacent patterns with distance 0
   --     Matches := [others => Empty_Match];
   --     Find_Matches (A, Patterns, Matches, Adjacent_Text);

   --     Assert (Matches (1).EP /= null, "Edge case - A found");
   --     Assert (Matches (2).EP /= null, "Edge case - B found at distance 0");

   --     --  Test 2: Exact boundary case
   --     A.Reset;
   --     Matches := [others => Empty_Match];
   --     Find_Matches (A, Patterns, Matches, Boundary_Text);

   --     Assert (Matches (1).EP /= null, "Edge case - A found in boundary test");
   --     Assert (Matches (2).EP /= null, "Edge case - B found at exact boundary");
   --  end Test_Position_Modifiers_Edge_Cases;

begin

   Test_Integer_Options;
   Test_State_Counting;
   Basic_Test_1;
   --  Basic_Test_1_Nocase;
   --  Basic_Test_2;
   --  Basic_Test_3;
   --  Test_Mixed_Case_Patterns;
   --  Test_Performance_vs_Naive;
   --  Test_High_Pattern_Count;
   --  Test_Security_Evasion_Resistance;
   --  Test_Memory_Safety;
   --  Test_Position_Modifiers_Basic;
   --  Test_Position_Modifiers_Streaming;
   --  Test_Position_Modifiers_SQL_Injection;
   --  Test_Position_Modifiers_Edge_Cases;

   Put_Line ("Pass:" & Success_Count'Image & " / Total:" & Total_Tests'Image);
   if Success_Count = Total_Tests then
      Put_Line ("All tests passed successfully!");
   else
      Put_Line ("Some tests failed. Check the output for details.");
   end if;

end Tests;
