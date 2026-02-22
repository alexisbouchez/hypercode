import type { Lesson } from "../../types";

export const mapFilter: Lesson = {
  id: "map-filter",
  title: "Map & Filter",
  chapterId: "collections",
  content: `## Map & Filter

\`map\` transforms each element; \`filter\` keeps only matching elements:

\`\`\`scala
val nums = List(1, 2, 3, 4, 5, 6)
val evens = nums.filter(x => x % 2 == 0)
println(evens)  // List(2, 4, 6)

val doubled = nums.map(x => x * 2)
println(doubled)  // List(2, 4, 6, 8, 10, 12)
\`\`\`

### Chaining

\`\`\`scala
val result = nums.filter(x => x % 2 == 0).map(x => x * x)
println(result)  // List(4, 16, 36)
\`\`\`

### Your Task

Write a function \`doubleEvens\` that takes a \`List[Int]\` and returns a new list containing only the even numbers, each doubled.`,

  starterCode: `def doubleEvens(nums: List[Int]): List[Int] =
  nums.filter(x => x % 2 == 0).map(x => x * 2)

println(doubleEvens(List(1, 2, 3, 4, 5, 6)))
println(doubleEvens(List(7, 8, 9, 10)))
`,

  solution: `def doubleEvens(nums: List[Int]): List[Int] =
  nums.filter(x => x % 2 == 0).map(x => x * 2)

println(doubleEvens(List(1, 2, 3, 4, 5, 6)))
println(doubleEvens(List(7, 8, 9, 10)))
`,

  tests: [
    {
      name: "1..6 → evens doubled",
      expected: "List(4, 8, 12)\n",
      code: `{{FUNC}}
println(doubleEvens(List(1, 2, 3, 4, 5, 6)))
`,
    },
    {
      name: "7..10 → evens doubled",
      expected: "List(16, 20)\n",
      code: `{{FUNC}}
println(doubleEvens(List(7, 8, 9, 10)))
`,
    },
  ],
};
