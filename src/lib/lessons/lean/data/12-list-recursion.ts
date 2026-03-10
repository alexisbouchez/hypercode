import type { Lesson } from "../../types";

export const listRecursion: Lesson = {
  id: "list-recursion",
  title: "Recursion on Lists",
  chapterId: "lists",
  content: `## Recursive Functions on Lists

Lists have two forms — empty (\`[]\`) and non-empty (\`head :: tail\`). You can match on these to write recursive functions:

\`\`\`lean
def mySum : List Nat → Nat
  | [] => 0
  | x :: xs => x + mySum xs

#eval mySum [1, 2, 3, 4, 5]    -- 15
\`\`\`

Reading this:
- If the list is empty (\`[]\`), the sum is \`0\`
- Otherwise, the list is \`x :: xs\` — head \`x\` and tail \`xs\` — so the sum is \`x + mySum xs\`

Similarly for counting elements:

\`\`\`lean
def myLength : List Nat → Nat
  | [] => 0
  | _ :: xs => 1 + myLength xs
\`\`\`

## Structural Recursion and the Induction Principle

When you write a recursive function on a list by matching \`[]\` and \`x :: xs\`, you are performing **structural recursion** — each recursive call operates on a strictly smaller piece of the input. This pattern mirrors the **induction principle** for lists: to prove a property holds for all lists, prove it for the empty list (base case) and prove that if it holds for \`xs\` then it holds for \`x :: xs\` (inductive step).

In Lean's theorem-proving mode, the \`induction\` tactic automates exactly this pattern. For example, to prove that \`myLength (xs ++ ys) = myLength xs + myLength ys\`, you would use \`induction xs\` and Lean generates two goals matching the two constructors of \`List\`. Proof **tactics** like \`simp\`, \`rfl\`, and \`induction\` are the tools that make Lean a practical theorem prover.

## Your Turn

Define \`myProduct\` that multiplies all elements in a list.
- Empty list → \`1\` (the multiplicative identity)
- Non-empty → first element times the product of the rest
`,
  starterCode: `def myProduct : List Nat → Nat
  | [] => 1
  | x :: xs => x * myProduct xs

#eval myProduct [1, 2, 3, 4, 5]
#eval myProduct [2, 3, 4]
#eval myProduct []
`,
  solution: `def myProduct : List Nat → Nat
  | [] => 1
  | x :: xs => x * myProduct xs

#eval myProduct [1, 2, 3, 4, 5]
#eval myProduct [2, 3, 4]
#eval myProduct []
`,
  tests: [
    {
      name: "myProduct [1,2,3,4,5] = 120",
      code: "{{FUNC}}\n#eval myProduct [1, 2, 3, 4, 5]",
      expected: "120\n",
    },
    {
      name: "myProduct [2,3,4] = 24",
      code: "{{FUNC}}\n#eval myProduct [2, 3, 4]",
      expected: "24\n",
    },
    {
      name: "myProduct [] = 1",
      code: "{{FUNC}}\n#eval myProduct []",
      expected: "1\n",
    },
  ],
};
