import type { Lesson } from "../types";

export const maps: Lesson = {
  id: "maps",
  title: "Maps",
  chapterId: "data-structures",
  content: `## Key-Value Storage

Maps are Go's built-in hash table. They store key-value pairs and provide constant-time lookups.

### Creating Maps

\`\`\`go
// Map literal
ages := map[string]int{
    "Alice": 30,
    "Bob":   25,
}

// Make an empty map
scores := make(map[string]int)
\`\`\`

The type \`map[string]int\` reads as "a map from strings to ints." Keys can be any comparable type (strings, ints, booleans, structs without slice/map fields). Values can be anything.

### Operations

\`\`\`go
m := make(map[string]int)

// Insert or update
m["alice"] = 95

// Lookup
score := m["alice"] // 95

// Delete
delete(m, "alice")

// Length
fmt.Println(len(m))
\`\`\`

### The Comma-Ok Idiom

When you look up a key that does not exist, Go returns the zero value for the value type. To distinguish between "key not found" and "key exists with zero value", use the two-value form:

\`\`\`go
value, ok := m["key"]
if ok {
    fmt.Println("found:", value)
} else {
    fmt.Println("not found")
}
\`\`\`

This is called the "comma-ok" idiom. The second value is a boolean that indicates whether the key was present.

### Iterating

Use \`for range\` to iterate over a map. The iteration order is **not guaranteed** --- it is intentionally randomized by the runtime:

\`\`\`go
for key, value := range m {
    fmt.Printf("%s: %d\\n", key, value)
}
\`\`\`

### Your Task

Write a function \`wordCount\` that takes a string and returns a \`map[string]int\` where each key is a word and each value is how many times that word appears.

Use \`strings.Fields\` to split the string into words (it splits on whitespace).`,

  starterCode: `package main

import (
\t"fmt"
\t"strings"
)

func wordCount(s string) map[string]int {
\t// Your code here
\treturn nil
}

func main() {
\tresult := wordCount("the cat sat on the mat the cat")
\tfmt.Println(result["the"])
\tfmt.Println(result["cat"])
\tfmt.Println(result["sat"])
\tfmt.Println(result["on"])
\tfmt.Println(result["mat"])
}
`,

  solution: `package main

import (
\t"fmt"
\t"strings"
)

func wordCount(s string) map[string]int {
\tcounts := make(map[string]int)
\tfor _, word := range strings.Fields(s) {
\t\tcounts[word]++
\t}
\treturn counts
}

func main() {
\tresult := wordCount("the cat sat on the mat the cat")
\tfmt.Println(result["the"])
\tfmt.Println(result["cat"])
\tfmt.Println(result["sat"])
\tfmt.Println(result["on"])
\tfmt.Println(result["mat"])
}
`,

  tests: [
    {
      name: '"the" appears 3 times',
      code: `package main

import (
\t"fmt"
\t"strings"
)

{{FUNC}}

func main() {
\t_ = strings.Fields
\tresult := wordCount("the cat sat on the mat the cat")
\tfmt.Println(result["the"])
}`,
      expected: "3\n",
    },
    {
      name: '"cat" appears 2 times',
      code: `package main

import (
\t"fmt"
\t"strings"
)

{{FUNC}}

func main() {
\t_ = strings.Fields
\tresult := wordCount("the cat sat on the mat the cat")
\tfmt.Println(result["cat"])
}`,
      expected: "2\n",
    },
    {
      name: '"sat" appears 1 time',
      code: `package main

import (
\t"fmt"
\t"strings"
)

{{FUNC}}

func main() {
\t_ = strings.Fields
\tresult := wordCount("the cat sat on the mat the cat")
\tfmt.Println(result["sat"])
}`,
      expected: "1\n",
    },
    {
      name: "empty string returns empty map",
      code: `package main

import (
\t"fmt"
\t"strings"
)

{{FUNC}}

func main() {
\t_ = strings.Fields
\tresult := wordCount("")
\tfmt.Println(len(result))
}`,
      expected: "0\n",
    },
  ],
};
