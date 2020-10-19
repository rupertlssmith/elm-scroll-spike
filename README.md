# elm-scroll-spike

Scroll bars with custom logic in Elm.

Relevant discussion:
https://discourse.elm-lang.org/t/custom-scroll-bar-logic/6403

Only lines that are visible are rendered, plus 1 page above and below so that
the page up/down controls do not flicker.

Size calculations are needed, and these are re-done when the window is resized.

`Html.Lazy` and `Html.Keyed` are used to minimise the DOM work as much as
possible.

Scrolling always happens in integer increments of the line height, so the text
does not jump around.
