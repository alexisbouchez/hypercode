import type { Lesson } from "../../types";

export const helloWorld: Lesson = {
  id: "hello-world",
  title: "Hello, World!",
  chapterId: "foundations",
  content: `## Your First Lean Program

Lean 4 is both a functional programming language and an interactive **theorem prover**. What makes Lean unique among programming languages is its type system based on **dependent type theory** — types can depend on values, which lets you express precise specifications and mathematical proofs directly in code. Before we explore those advanced features, let's start with the basics — printing output.

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
