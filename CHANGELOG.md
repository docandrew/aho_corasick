## v0.3.0

**BREAKING CHANGES:**

- The `Find_Matches` procedure now requires a `Stream_State` parameter to
  support multiple text streams. This change allows the same automaton to be
  used with multiple text inputs simultaneously without needing copies of the
  automaton matrix.

- The `Reset` procedure has been updated to reset the `Stream_State` instead of
  the `Automaton`.

## v0.2.0

Initial public release.
