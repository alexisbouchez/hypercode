import type { Lesson } from "../types";

export const strings_: Lesson = {
  id: "strings",
  title: "Strings",
  chapterId: "data-structures",
  content: `## More Than Meets the Eye

Go strings are immutable sequences of bytes. This seems simple until you encounter non-ASCII text and discover the difference between bytes and characters.

### Bytes vs Runes

A \`string\` in Go is a read-only slice of bytes. The \`len\` function returns the number of **bytes**, not the number of characters:

\`\`\`go
fmt.Println(len("hello"))  // 5
fmt.Println(len("héllo"))  // 6 (é is 2 bytes in UTF-8)
\`\`\`

Go uses the term **rune** for what most languages call a character. A rune is an alias for \`int32\` and represents a single Unicode code point.

To get the actual character count, use \`utf8.RuneCountInString\`:

\`\`\`go
import "unicode/utf8"

fmt.Println(utf8.RuneCountInString("héllo")) // 5
\`\`\`

### Iterating Over Strings

A \`for range\` loop over a string yields runes, not bytes:

\`\`\`go
for i, r := range "héllo" {
    fmt.Printf("%d: %c\\n", i, r)
}
// 0: h
// 1: é    (byte index 1, but é is 2 bytes wide)
// 3: l    (byte index jumps to 3)
// 4: l
// 5: o
\`\`\`

The index is the byte position, but \`r\` is a full rune. This is the safe way to process strings character by character.

### The strings Package

The \`strings\` package provides essential string operations:

\`\`\`go
import "strings"

strings.Contains("seafood", "foo")     // true
strings.HasPrefix("seafood", "sea")    // true
strings.HasSuffix("seafood", "food")   // true

strings.ToUpper("hello")               // "HELLO"
strings.ToLower("HELLO")               // "hello"

strings.Replace("oink oink", "oink", "moo", -1) // "moo moo"

strings.Split("a,b,c", ",")            // ["a", "b", "c"]
strings.Join([]string{"a", "b"}, "-")  // "a-b"

strings.Fields("  foo  bar  baz  ")    // ["foo", "bar", "baz"]
\`\`\`

\`strings.Fields\` splits on any whitespace and ignores leading/trailing spaces. It is often more useful than \`strings.Split\`.

### Conversions

You can convert between strings, byte slices, and rune slices:

\`\`\`go
s := "hello"
b := []byte(s)   // string to bytes
r := []rune(s)   // string to runes
s2 := string(b)  // bytes back to string
s3 := string(r)  // runes back to string
\`\`\`

For number conversions, use the \`strconv\` package:

\`\`\`go
import "strconv"

s := strconv.Itoa(42)       // int to string: "42"
n, err := strconv.Atoi("42") // string to int: 42, nil
\`\`\`

### Your Task

Write a function \`acronym(s string) string\` that takes a phrase and returns its acronym. Take the first letter of each word and uppercase it.

For example, \`"Portable Network Graphics"\` becomes \`"PNG"\`.

Use \`strings.Fields\` to split into words, and \`strings.ToUpper\` to uppercase.`,

  starterCode: `package main

import (
\t"fmt"
\t"strings"
)

func acronym(s string) string {
\t// Your code here
\treturn ""
}

func main() {
\tfmt.Println(acronym("Portable Network Graphics"))
\tfmt.Println(acronym("as soon as possible"))
}
`,

  solution: `package main

import (
\t"fmt"
\t"strings"
)

func acronym(s string) string {
\twords := strings.Fields(s)
\tresult := ""
\tfor _, w := range words {
\t\tfor _, r := range w {
\t\t\tresult += strings.ToUpper(string(r))
\t\t\tbreak
\t\t}
\t}
\treturn result
}

func main() {
\tfmt.Println(acronym("Portable Network Graphics"))
\tfmt.Println(acronym("as soon as possible"))
}
`,

  tests: [
    {
      name: 'acronym("Portable Network Graphics") = "PNG"',
      code: `package main

import (
\t"fmt"
\t"strings"
)

{{FUNC}}

func main() {
\tfmt.Println(acronym("Portable Network Graphics"))
}`,
      expected: "PNG\n",
    },
    {
      name: 'acronym("as soon as possible") = "ASAP"',
      code: `package main

import (
\t"fmt"
\t"strings"
)

{{FUNC}}

func main() {
\tfmt.Println(acronym("as soon as possible"))
}`,
      expected: "ASAP\n",
    },
    {
      name: 'acronym("Go is awesome") = "GIA"',
      code: `package main

import (
\t"fmt"
\t"strings"
)

{{FUNC}}

func main() {
\tfmt.Println(acronym("Go is awesome"))
}`,
      expected: "GIA\n",
    },
    {
      name: "single word",
      code: `package main

import (
\t"fmt"
\t"strings"
)

{{FUNC}}

func main() {
\tfmt.Println(acronym("Hello"))
}`,
      expected: "H\n",
    },
  ],
};
