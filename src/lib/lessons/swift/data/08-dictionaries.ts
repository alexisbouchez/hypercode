import type { Lesson } from "../../types";

export const dictionaries: Lesson = {
  id: "dictionaries",
  title: "Dictionaries",
  chapterId: "collections",
  content: `## Dictionaries

Dictionaries store key-value pairs:

\`\`\`swift
var scores = ["Alice": 95, "Bob": 82, "Charlie": 78]
\`\`\`

### Accessing Values

Dictionary lookups return an optional because the key might not exist:

\`\`\`swift
let aliceScore = scores["Alice"] ?? 0   // 95
let daveScore = scores["Dave"] ?? 0     // 0 (default)
\`\`\`

### Adding and Updating

\`\`\`swift
scores["Dave"] = 91     // add new entry
scores["Alice"] = 100   // update existing
\`\`\`

### Iterating

\`\`\`swift
for (name, score) in scores {
    print("\\(name): \\(score)")
}
\`\`\`

### Your Task

Write a function \`countFrequency\` that takes an array of strings and a target string, and returns how many times the target appears in the array.`,

  starterCode: `func countFrequency(_ words: [String], _ target: String) -> Int {
    var count = 0
    for word in words {
        if word == target {
            count += 1
        }
    }
    return count
}

print(countFrequency(["apple", "banana", "apple", "cherry", "apple"], "apple"))
`,

  solution: `func countFrequency(_ words: [String], _ target: String) -> Int {
    var count = 0
    for word in words {
        if word == target {
            count += 1
        }
    }
    return count
}

print(countFrequency(["apple", "banana", "apple", "cherry", "apple"], "apple"))
`,

  tests: [
    {
      name: "apple appears 3 times",
      expected: "3\n",
      code: `{{FUNC}}
print(countFrequency(["apple", "banana", "apple", "cherry", "apple"], "apple"))
`,
    },
    {
      name: "banana appears 1 time",
      expected: "1\n",
      code: `{{FUNC}}
print(countFrequency(["apple", "banana", "apple", "cherry", "apple"], "banana"))
`,
    },
    {
      name: "grape appears 0 times",
      expected: "0\n",
      code: `{{FUNC}}
print(countFrequency(["apple", "banana", "apple", "cherry", "apple"], "grape"))
`,
    },
  ],
};
