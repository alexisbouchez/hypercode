import type { Lesson } from "../../types";

export const fold: Lesson = {
  id: "fold",
  title: "Fold",
  chapterId: "functional",
  content: `## Folding Lists

\`foldl\` (fold left) reduces a list to a single value by applying a function repeatedly:

\`\`\`lean
#eval [1, 2, 3, 4, 5].foldl (fun acc x => acc + x) 0
-- 15
\`\`\`

Reading this: start with accumulator \`0\`, then for each element \`x\`, compute the new accumulator \`acc + x\`.

\`foldl\` is the basis for many computations:

\`\`\`lean
-- Sum
def mySum (xs : List Nat) : Nat := xs.foldl (fun acc x => acc + x) 0

-- Product
def myProduct (xs : List Nat) : Nat := xs.foldl (fun acc x => acc * x) 1

-- Maximum (given non-empty list)
def myMax (xs : List Nat) : Nat := xs.foldl (fun acc x => if x > acc then x else acc) 0
\`\`\`

## Your Turn

Define \`countEvens\` that counts how many even numbers are in a list using \`foldl\`.

Hint: The accumulator starts at \`0\`, and for each element you add \`1\` if it's even, \`0\` otherwise.
`,
  starterCode: `def countEvens (xs : List Nat) : Nat :=
  xs.foldl (fun acc x => if x % 2 == 0 then acc + 1 else acc) 0

#eval countEvens [1, 2, 3, 4, 5, 6]
#eval countEvens [1, 3, 5, 7]
#eval countEvens [2, 4, 6, 8]
`,
  solution: `def countEvens (xs : List Nat) : Nat :=
  xs.foldl (fun acc x => if x % 2 == 0 then acc + 1 else acc) 0

#eval countEvens [1, 2, 3, 4, 5, 6]
#eval countEvens [1, 3, 5, 7]
#eval countEvens [2, 4, 6, 8]
`,
  tests: [
    {
      name: "countEvens [1..6] = 3",
      code: "{{FUNC}}\n#eval countEvens [1, 2, 3, 4, 5, 6]",
      expected: "3\n",
    },
    {
      name: "countEvens all odd = 0",
      code: "{{FUNC}}\n#eval countEvens [1, 3, 5, 7]",
      expected: "0\n",
    },
    {
      name: "countEvens all even = 4",
      code: "{{FUNC}}\n#eval countEvens [2, 4, 6, 8]",
      expected: "4\n",
    },
  ],
};
