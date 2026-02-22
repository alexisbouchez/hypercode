import type { Lesson } from "../../types";

export const lambdas: Lesson = {
  id: "lambdas",
  title: "Lambda Functions",
  chapterId: "functional",
  content: `## Anonymous Functions (Lambdas)

In Lean, you can create functions without naming them using \`fun\`:

\`\`\`lean
fun x => x * 2
\`\`\`

This creates a function that takes \`x\` and returns \`x * 2\`. You can assign it to a name:

\`\`\`lean
def double := fun (x : Nat) => x * 2

#eval double 7    -- 14
\`\`\`

Lambdas with multiple parameters:

\`\`\`lean
def add := fun (x y : Nat) => x + y

#eval add 3 4    -- 7
\`\`\`

Lambdas are most useful when you need to pass a function as an argument (which we will see in the next lessons).

## Your Turn

Define \`square\` as a lambda that squares a number, and \`negate\` as a lambda that subtracts a number from 10.
`,
  starterCode: `def square := fun (x : Nat) => x * x
def addTen := fun (x : Nat) => x + 10

#eval square 7
#eval square 12
#eval addTen 5
#eval addTen 32
`,
  solution: `def square := fun (x : Nat) => x * x
def addTen := fun (x : Nat) => x + 10

#eval square 7
#eval square 12
#eval addTen 5
#eval addTen 32
`,
  tests: [
    {
      name: "square 7 = 49",
      code: "{{FUNC}}\n#eval square 7",
      expected: "49\n",
    },
    {
      name: "square 12 = 144",
      code: "{{FUNC}}\n#eval square 12",
      expected: "144\n",
    },
    {
      name: "addTen 5 = 15",
      code: "{{FUNC}}\n#eval addTen 5",
      expected: "15\n",
    },
    {
      name: "addTen 32 = 42",
      code: "{{FUNC}}\n#eval addTen 32",
      expected: "42\n",
    },
  ],
};
