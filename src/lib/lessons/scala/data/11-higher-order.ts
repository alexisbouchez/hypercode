import type { Lesson } from "../../types";

export const higherOrder: Lesson = {
  id: "higher-order",
  title: "Higher-Order Functions",
  chapterId: "collections",
  content: `## Higher-Order Functions

Functions can take other functions as parameters:

\`\`\`scala
def applyTwice(f: Int => Int, x: Int): Int = f(f(x))

println(applyTwice(x => x * 2, 3))  // 12
\`\`\`

### Returning Functions

\`\`\`scala
def multiplier(factor: Int): Int => Int = x => x * factor

val triple = multiplier(3)
println(triple(5))  // 15
\`\`\`

### reduce

\`reduce\` is like \`foldLeft\` but uses the first element as the starting value:

\`\`\`scala
val product = List(1, 2, 3, 4).foldLeft(1)((acc, x) => acc * x)
println(product)  // 24
\`\`\`

### Your Task

Write a function \`product\` that takes a \`List[Int]\` and returns the product of all elements using \`foldLeft\`. An empty list should return \`1\`.`,

  starterCode: `def product(nums: List[Int]): Int = nums.foldLeft(1)((acc, x) => acc * x)

println(product(List(1, 2, 3, 4)))
println(product(List(2, 5, 10)))
`,

  solution: `def product(nums: List[Int]): Int = nums.foldLeft(1)((acc, x) => acc * x)

println(product(List(1, 2, 3, 4)))
println(product(List(2, 5, 10)))
`,

  tests: [
    {
      name: "product(1,2,3,4) = 24",
      expected: "24\n",
      code: `{{FUNC}}
println(product(List(1, 2, 3, 4)))
`,
    },
    {
      name: "product(2,5,10) = 100",
      expected: "100\n",
      code: `{{FUNC}}
println(product(List(2, 5, 10)))
`,
    },
    {
      name: "product(empty) = 1",
      expected: "1\n",
      code: `{{FUNC}}
println(product(List()))
`,
    },
  ],
};
