import type { Lesson } from "../../types";

export const higherOrder: Lesson = {
  id: "higher-order",
  title: "Higher-Order Functions",
  chapterId: "functional",
  content: `## Higher-Order Functions

Swift arrays have powerful built-in higher-order functions:

### map — transform each element

\`\`\`swift
let nums = [1, 2, 3, 4]
let doubled = nums.map { $0 * 2 }  // [2, 4, 6, 8]
\`\`\`

### filter — keep elements matching a predicate

\`\`\`swift
let evens = nums.filter { $0 % 2 == 0 }  // [2, 4]
\`\`\`

### reduce — combine all elements into one value

\`\`\`swift
let total = nums.reduce(0) { $0 + $1 }  // 10
\`\`\`

### Chaining

\`\`\`swift
let result = nums.filter { $0 % 2 == 0 }.map { $0 * $0 }  // [4, 16]
\`\`\`

The shorthand \`$0\`, \`$1\`, etc. refer to the first and second closure arguments.

### Your Task

Write a function \`sumOfSquaresOfEvens\` that takes an array of integers and returns the sum of the squares of all even numbers. Use \`filter\`, \`map\`, and \`reduce\`.`,

  starterCode: `func sumOfSquaresOfEvens(_ nums: [Int]) -> Int {
    return nums.filter { $0 % 2 == 0 }.map { $0 * $0 }.reduce(0) { $0 + $1 }
}

print(sumOfSquaresOfEvens([1, 2, 3, 4, 5]))
`,

  solution: `func sumOfSquaresOfEvens(_ nums: [Int]) -> Int {
    return nums.filter { $0 % 2 == 0 }.map { $0 * $0 }.reduce(0) { $0 + $1 }
}

print(sumOfSquaresOfEvens([1, 2, 3, 4, 5]))
`,

  tests: [
    {
      name: "[1,2,3,4,5] → 20",
      expected: "20\n",
      code: `{{FUNC}}
print(sumOfSquaresOfEvens([1, 2, 3, 4, 5]))
`,
    },
    {
      name: "[2,4,6] → 56",
      expected: "56\n",
      code: `{{FUNC}}
print(sumOfSquaresOfEvens([2, 4, 6]))
`,
    },
    {
      name: "[1,3,5] → 0",
      expected: "0\n",
      code: `{{FUNC}}
print(sumOfSquaresOfEvens([1, 3, 5]))
`,
    },
  ],
};
