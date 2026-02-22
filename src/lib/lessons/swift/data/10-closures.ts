import type { Lesson } from "../../types";

export const closures: Lesson = {
  id: "closures",
  title: "Closures",
  chapterId: "functional",
  content: `## Closures

Closures are self-contained blocks of functionality that can be passed around. In Swift, functions are first-class values — they can be stored in variables and passed as arguments:

\`\`\`swift
let double = { (x: Int) -> Int in x * 2 }
print(double(5))  // 10
\`\`\`

### Closures as Function Parameters

\`\`\`swift
func applyTwice(_ f: (Int) -> Int, _ x: Int) -> Int {
    return f(f(x))
}

print(applyTwice({ n in n + 3 }, 10))  // 16
\`\`\`

### Trailing Closure Syntax

When a closure is the last argument, you can place it outside the parentheses:

\`\`\`swift
applyTwice(10) { n in n + 3 }  // 16
\`\`\`

### Your Task

Write a function \`applyTwice\` that takes a function \`f: (Int) -> Int\` and an integer \`x\`, and returns the result of applying \`f\` to \`x\` twice (i.e., \`f(f(x))\`).`,

  starterCode: `func applyTwice(_ f: (Int) -> Int, _ x: Int) -> Int {
    return f(f(x))
}

print(applyTwice({ n in n * 2 }, 3))
print(applyTwice({ n in n + 10 }, 5))
`,

  solution: `func applyTwice(_ f: (Int) -> Int, _ x: Int) -> Int {
    return f(f(x))
}

print(applyTwice({ n in n * 2 }, 3))
print(applyTwice({ n in n + 10 }, 5))
`,

  tests: [
    {
      name: "double 3 twice = 12",
      expected: "12\n",
      code: `{{FUNC}}
print(applyTwice({ n in n * 2 }, 3))
`,
    },
    {
      name: "add 10 twice to 5 = 25",
      expected: "25\n",
      code: `{{FUNC}}
print(applyTwice({ n in n + 10 }, 5))
`,
    },
  ],
};
