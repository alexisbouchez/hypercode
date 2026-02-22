import type { Lesson } from "../../types";

export const arrays: Lesson = {
  id: "arrays",
  title: "Arrays",
  chapterId: "collections",
  content: `## Arrays

Swift arrays are ordered collections of values of the same type:

\`\`\`swift
var nums = [1, 2, 3, 4, 5]
var names = ["Alice", "Bob", "Charlie"]
\`\`\`

### Common Operations

\`\`\`swift
nums.append(6)          // add to end
print(nums.count)       // number of elements
print(nums[0])          // access by index (first = 0)
nums[0] = 10            // modify element
\`\`\`

### Iterating

\`\`\`swift
for num in nums {
    print(num)
}
\`\`\`

### Your Task

Write a function \`sum\` that takes an array of integers and returns their sum.`,

  starterCode: `func sum(_ nums: [Int]) -> Int {
    var total = 0
    for n in nums {
        total += n
    }
    return total
}

print(sum([1, 2, 3, 4, 5]))
`,

  solution: `func sum(_ nums: [Int]) -> Int {
    var total = 0
    for n in nums {
        total += n
    }
    return total
}

print(sum([1, 2, 3, 4, 5]))
`,

  tests: [
    {
      name: "sum [1,2,3,4,5]",
      expected: "15\n",
      code: `{{FUNC}}
print(sum([1, 2, 3, 4, 5]))
`,
    },
    {
      name: "sum [10, 20, 30]",
      expected: "60\n",
      code: `{{FUNC}}
print(sum([10, 20, 30]))
`,
    },
    {
      name: "sum empty array",
      expected: "0\n",
      code: `{{FUNC}}
print(sum([]))
`,
    },
  ],
};
