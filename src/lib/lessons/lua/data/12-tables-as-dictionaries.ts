import type { Lesson } from "../../types";

export const tablesAsDictionaries: Lesson = {
  id: "tables-as-dictionaries",
  title: "Tables as Dictionaries",
  chapterId: "tables",
  content: `## Tables as Dictionaries

Tables can also be used as key-value maps (dictionaries):

\`\`\`lua
local person = {
  name = "Alice",
  age = 30,
  city = "Paris"
}

print(person.name)  -- Alice
print(person.age)   -- 30
\`\`\`

### Adding & Modifying Fields

\`\`\`lua
person.email = "alice@example.com"
person.age = 31
\`\`\`

### Bracket Notation

Use brackets for dynamic keys or keys with special characters:

\`\`\`lua
person["full name"] = "Alice Smith"
local key = "city"
print(person[key])  -- Paris
\`\`\`

### Your Task

Create a table representing a book with title, author, and year, then print each field.`,

  starterCode: `local book = {}
book.title = "The Moon"
book.author = "Jane"
book.year = 2020

print(book.title)
print(book.author)
print(book.year)
`,

  solution: `local book = {}
book.title = "The Moon"
book.author = "Jane"
book.year = 2020

print(book.title)
print(book.author)
print(book.year)
`,

  tests: [
    {
      name: "prints book fields",
      expected: "The Moon\nJane\n2020\n",
    },
  ],
};
