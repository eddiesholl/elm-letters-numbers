# elm-letters-numbers

A solver for the number puzzles on the show 'Letters and Numbers'.

# Running the UI

- Clone the repo
- `npm install`
- `npm run serve`
- Visit the app at `http://localhost:8080`

# Solution and algorithm

I use a recursive algorithm to work through the list of input numbers, to build all possible combinations of input numbers and operators. Start with the first input, and descend into the remaining inputs to generate all possible expressions. Then 'apply' the first input with the expressions generated on the right.

When combining the left and right terms of an expression, all operators are 'applied'. Some operators (`-`) are applied `left op right` and `right op left`.

Given the inputs 1, 2 and 3:

-> 1 and [2 + 3, 2 - 3, 3 - 2, 2 * 3]

- 1 + (2 + 3), 1 + (2 - 3), 1 + (3 - 2), 1 + (2 * 3)
- 1 * (2 + 3), 1 * (2 - 3), 1 * (3 - 2), 1 * (2 * 3)
- 1 - (2 + 3), 1 - (2 - 3), 1 - (3 - 2), 1 - (2 * 3)
- (2 + 3) - 1, (2 - 3) - 1, (3 - 2) - 1, (2 * 3) - 1

The current approach has a few limitations. It will generate some identical combinations. It could also be improved by calculating the result along the way, and bail out early if the target gets reached exactly.
