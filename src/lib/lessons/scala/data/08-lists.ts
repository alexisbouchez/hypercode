import type { Lesson } from "../../types";

export const lists: Lesson = {
  id: "lists",
  title: "Lists",
  chapterId: "collections",
  content: `## Lists

Scala lists are immutable sequences. Create them with \`List(...)\`:

\`\`\`scala
val nums = List(1, 2, 3, 4, 5)
println(nums.length)  // 5
println(nums.head)    // 1
println(nums.tail)    // List(2, 3, 4, 5)
\`\`\`

### Prepend with \`::\`

\`\`\`scala
val more = 0 :: nums
println(more)  // List(0, 1, 2, 3, 4, 5)
\`\`\`

### foldLeft

\`foldLeft\` accumulates a result over the list:

\`\`\`scala
val sum = List(1, 2, 3, 4, 5).foldLeft(0)((acc, x) => acc + x)
println(sum)  // 15
\`\`\`

### Your Task

Write a function \`sumList\` that takes a \`List[Int]\` and returns the sum of all elements using \`foldLeft\`.`,

  starterCode: `def sumList(nums: List[Int]): Int = nums.foldLeft(0)((acc, x) => acc + x)

println(sumList(List(1, 2, 3, 4, 5)))
println(sumList(List(10, 20, 30)))
`,

  solution: `def sumList(nums: List[Int]): Int = nums.foldLeft(0)((acc, x) => acc + x)

println(sumList(List(1, 2, 3, 4, 5)))
println(sumList(List(10, 20, 30)))
`,

  tests: [
    {
      name: "sum 1..5 = 15",
      expected: "15\n",
      code: `{{FUNC}}
println(sumList(List(1, 2, 3, 4, 5)))
`,
    },
    {
      name: "sum 10+20+30 = 60",
      expected: "60\n",
      code: `{{FUNC}}
println(sumList(List(10, 20, 30)))
`,
    },
    {
      name: "empty list = 0",
      expected: "0\n",
      code: `{{FUNC}}
println(sumList(List()))
`,
    },
  ],
};
