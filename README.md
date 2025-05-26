# Aho-Corasick Algorithm Library

This is a SPARK/Ada Alire crate for the Aho-Corasick string matching algorithm.

The algorithm allows for fast, simultaneous matching of multiple patterns in a
text, with linear-time complexity relative to the combined length of the
patterns and the text.

## Features
- Mixed-case pattern matching
- Formally-verified using SPARK (Bronze mode only)
- Stream processing support (process text in chunks while maintaining state)

## Notes
- The library uses Ada 2022 features and requires a version of GNAT that
   supports it.
- SPARKLib is used for its verified Doubly-Linked List implementation.

## Limitations
- The library has pre-defined maximum lengths for patterns, the number
  of patterns, and the number of "overlapping" patterns, which is to say
  patterns that can match at the same position in the text. These can be
  adjusted in the `src/aho_corasick.ads` file, but the defaults are
  generous and should be suitable for most use cases.

## Usage

```ada
   procedure Example is
      --  Note: in real code you would want to free these allocated objects
      Needles : constant Aho_Corasick.Pattern_Array (1 .. 2) :=
        [new Enhanced_Pattern'(Pattern => new String'("abc"), others => <>),
         new Enhanced_Pattern'(Pattern => new String'("def"), others => <>)];

      T : Aho_Corasick.Automaton := Aho_Corasick.Build_Automaton (Needles);
      Matches  : Aho_Corasick.Match_Array (Needles'Range);
      Haystack : constant String := "abcdefghi";
   begin
      Aho_Corasick.Find_Matches (T, Needles, Matches, Haystack);

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
```
