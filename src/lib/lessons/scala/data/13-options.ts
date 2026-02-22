import type { Lesson } from "../../types";

export const options: Lesson = {
  id: "options",
  title: "Options",
  chapterId: "oop",
  content: `## Options

Scala's \`Option[T]\` represents a value that may or may not exist — it's either \`Some(value)\` or \`None\`:

\`\`\`scala
def safeDivide(a: Int, b: Int): Option[Int] =
  if (b == 0) None else Some(a / b)

println(safeDivide(10, 2))   // Some(5)
println(safeDivide(10, 0))   // None
\`\`\`

### getOrElse

Provide a default value with \`getOrElse\`:

\`\`\`scala
val result = safeDivide(10, 0).getOrElse(-1)
println(result)  // -1
\`\`\`

### Your Task

Write a function \`safeHead\` that takes a \`List[Int]\` and returns \`Some(first)\` if the list is non-empty, or \`None\` if it's empty.`,

  starterCode: `def safeHead(nums: List[Int]): Option[Int] =
  if (nums.isEmpty) None else Some(nums.head)

println(safeHead(List(10, 20, 30)).getOrElse(-1))
println(safeHead(List()).getOrElse(-1))
`,

  solution: `def safeHead(nums: List[Int]): Option[Int] =
  if (nums.isEmpty) None else Some(nums.head)

println(safeHead(List(10, 20, 30)).getOrElse(-1))
println(safeHead(List()).getOrElse(-1))
`,

  tests: [
    {
      name: "non-empty list returns first",
      expected: "10\n",
      code: `{{FUNC}}
println(safeHead(List(10, 20, 30)).getOrElse(-1))
`,
    },
    {
      name: "empty list returns default",
      expected: "-1\n",
      code: `{{FUNC}}
println(safeHead(List()).getOrElse(-1))
`,
    },
  ],
};
