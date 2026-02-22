import type { Lesson } from "../../types";

export const helloWorld: Lesson = {
  id: "hello-world",
  title: "Hello, World!",
  chapterId: "foundations",
  content: `## Your First Lean Program

Lean 4 is a functional programming language and interactive theorem prover. Before we get to proofs, let's start with the basics — printing output.

In Lean, \`IO.println\` prints a string followed by a newline. To run it, wrap it in \`#eval\`:

\`\`\`lean
#eval IO.println "Hello, World!"
\`\`\`

The \`#eval\` directive tells Lean to evaluate an expression and show the result. When the expression is an \`IO\` action like \`IO.println\`, it executes the action.

## Your Turn

Print the message \`Hello, Lean!\` to the output.
`,
  starterCode: `-- Print "Hello, Lean!" using IO.println
#eval IO.println ""
`,
  solution: `#eval IO.println "Hello, Lean!"
`,
  tests: [
    {
      name: 'prints "Hello, Lean!"',
      expected: "Hello, Lean!\n",
    },
  ],
};
