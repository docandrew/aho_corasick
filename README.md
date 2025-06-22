# Aho-Corasick Algorithm Library

This is a SPARK/Ada Alire crate for the Aho-Corasick string matching algorithm.

The algorithm allows for fast, simultaneous matching of multiple patterns in a
text, with linear-time complexity relative to the combined length of the
patterns and the text.

## Features
- Mixed-case pattern matching
- Formally-verified using SPARK (Silver mode / no run-time errors)
- Stream processing support (process text in chunks while maintaining state)
- Snort/Suricata-like position restrictions (depth/offset/distance/within) for
   limiting matches at certain positions in the text or relative to other
   matches

## Notes
- The library uses Ada 2022 features and requires a version of GNAT that
   supports it.
- SPARKLib is used for its verified Doubly-Linked List implementation.

## Limitations
- The library has pre-defined maximum lengths for patterns and the number
  of patterns. These are arbitrary and used for proving properties about the
  code. They can be adjusted in the `src/aho_corasick.ads` file, but the
  defaults are generous and should be suitable for most use cases.

## Usage

```ada
   procedure Example is
       --  Note: in real code you would want to free these allocated objects
       Needles : constant Aho_Corasick.Pattern_Array (1 .. 2) :=
         [new Enhanced_Pattern'(Pattern => new String'("abc"), others => <>),
          new Enhanced_Pattern'(Pattern => new String'("def"), others => <>)];
       Matches  : Aho_Corasick.Match_Array (Needles'Range);

       package Matcher is new Aho_Corasick.Automatons (Needles);
       use Matcher;

      T : Automaton := Build_Automaton (Needles);
      Haystack : constant String := "abcdefghi";
   begin
      Find_Matches (T, Needles, Matches, Haystack);

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

See the tests package for more examples of features such as case-sensitivity,
stream processing, and position restrictions.
