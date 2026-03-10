import type { Lesson } from "../../types";

export const tablesAsArrays: Lesson = {
  id: "tables-as-arrays",
  title: "Tables as Arrays",
  chapterId: "tables",
  content: `## Tables as Arrays

Tables are the only data structure in Lua. When used with integer keys starting at 1, they act as arrays:

\`\`\`lua
local fruits = {"apple", "banana", "cherry"}

print(#fruits)  -- 3 (length)
\`\`\`

**Important:** Lua arrays are 1-indexed, not 0-indexed!

### Modifying Tables

\`\`\`lua
table.insert(fruits, "date")     -- appends "date"
table.remove(fruits, 1)          -- removes first element
\`\`\`

### Concatenation

\`table.concat\` joins array elements:

\`\`\`lua
local parts = {"Hello", "World"}
print(table.concat(parts, " "))  -- "Hello World"
\`\`\`

### Your Task

Create an array of numbers, add an element, and print the count and concatenated result.`,

  starterCode: `local numbers = {10, 20, 30}
table.insert(numbers, 40)

print(#numbers)
print(table.concat(numbers, ", "))
`,

  solution: `local numbers = {10, 20, 30}
table.insert(numbers, 40)

print(#numbers)
print(table.concat(numbers, ", "))
`,

  tests: [
    {
      name: "prints count and elements",
      expected: "4\n10, 20, 30, 40\n",
    },
  ],
};
