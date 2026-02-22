import type { Lesson } from "../../types";

export const loops: Lesson = {
  id: "loops",
  title: "Loops",
  chapterId: "basics",
  content: `## Loops

### For-In Loops

Swift uses \`for x in range\` syntax. Ranges are created with \`...\` (inclusive) or \`..<\` (exclusive upper bound):

\`\`\`swift
// 1 through 5 (inclusive)
for i in 1...5 {
    print(i)
}

// 0 through 4 (exclusive upper bound)
for i in 0..<5 {
    print(i)
}
\`\`\`

### While Loops

\`\`\`swift
var n = 1
while n < 100 {
    n *= 2
}
print(n) // 128
\`\`\`

### Your Task

Write a function \`sumTo\` that takes an \`Int\` \`n\` and returns the sum of all integers from 1 to \`n\` (inclusive), using a \`for\` loop.`,

  starterCode: `func sumTo(_ n: Int) -> Int {
    var total = 0
    for i in 1...n {
        total += i
    }
    return total
}

print(sumTo(5))
print(sumTo(10))
`,

  solution: `func sumTo(_ n: Int) -> Int {
    var total = 0
    for i in 1...n {
        total += i
    }
    return total
}

print(sumTo(5))
print(sumTo(10))
`,

  tests: [
    {
      name: "sum 1 to 5",
      expected: "15\n",
      code: `{{FUNC}}
print(sumTo(5))
`,
    },
    {
      name: "sum 1 to 10",
      expected: "55\n",
      code: `{{FUNC}}
print(sumTo(10))
`,
    },
  ],
};
