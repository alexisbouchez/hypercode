import type { Lesson } from "../../types";

export const tuplesMaps: Lesson = {
  id: "tuples-maps",
  title: "Tuples & Maps",
  chapterId: "collections",
  content: `## Tuples

Tuples are fixed-size ordered collections, created with \`{}\`:

\`\`\`elixir
point = {3, 4}
IO.puts(point)   # (3, 4)
\`\`\`

Destructure them with pattern matching:

\`\`\`elixir
{x, y} = {3, 4}
IO.puts(x)   # 3
\`\`\`

## Maps

Maps store key-value pairs with \`%{}\`:

\`\`\`elixir
person = %{name: "Alice", age: 30}
IO.puts(person.name)   # Alice
IO.puts(person.age)    # 30
\`\`\`

## Your Turn

1. Create \`point = {3, 4}\` and print it → \`(3, 4)\`
2. Destructure \`{x, y} = point\` and print \`x\` → \`Alice... wait
3. Create \`person = %{name: "Alice", age: 30}\`
4. Print \`person.name\` → \`Alice\`
5. Print \`person.age\` → \`30\`
`,
  starterCode: `point = {3, 4}
IO.puts(point)

person = %{name: "Alice", age: 30}
IO.puts(person.name)
IO.puts(person.age)
`,
  solution: `point = {3, 4}
IO.puts(point)

person = %{name: "Alice", age: 30}
IO.puts(person.name)
IO.puts(person.age)
`,
  tests: [
    {
      name: "tuples and maps",
      expected: "(3, 4)\nAlice\n30\n",
    },
  ],
};
