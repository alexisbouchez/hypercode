import type { Lesson } from "../types";

export const select_: Lesson = {
  id: "select",
  title: "Select",
  chapterId: "concurrency",
  content: `## Multiplexing Channels

The \`select\` statement lets a goroutine wait on multiple channel operations simultaneously. It is like a \`switch\` statement, but each case is a channel send or receive.

\`\`\`go
select {
case msg := <-ch1:
    fmt.Println("from ch1:", msg)
case msg := <-ch2:
    fmt.Println("from ch2:", msg)
}
\`\`\`

If multiple channels are ready, \`select\` picks one at random. If none are ready, it blocks until one becomes ready.

> "On screen!" The bridge crew monitors tactical, science, and communications all at once --- just like \`select\` listens on multiple channels simultaneously, ready to act on whichever one has news first.

### Non-Blocking Operations

Add a \`default\` case to make channel operations non-blocking:

\`\`\`go
select {
case msg := <-ch:
    fmt.Println("received:", msg)
default:
    fmt.Println("no message available")
}
\`\`\`

Without \`default\`, the \`select\` blocks. With \`default\`, it executes the default case immediately if no channel is ready.

### Pattern: Merging Channels

A common use of \`select\` is merging multiple channels into one. This lets a consumer read from a single channel regardless of how many producers exist:

\`\`\`go
func merge(ch1, ch2 <-chan string) <-chan string {
    out := make(chan string)
    go func() {
        defer close(out)
        for ch1 != nil || ch2 != nil {
            select {
            case v, ok := <-ch1:
                if !ok { ch1 = nil; continue }
                out <- v
            case v, ok := <-ch2:
                if !ok { ch2 = nil; continue }
                out <- v
            }
        }
    }()
    return out
}
\`\`\`

Setting a channel to \`nil\` after it closes prevents \`select\` from receiving zero values from it, because receives on a nil channel block forever.

### Your Task

Write a function \`merge(ch1, ch2 <-chan int) <-chan int\` that merges two integer channels into one. The output channel should receive all values from both input channels and close when both inputs are exhausted.

Key steps:
1. Create an output channel and launch a goroutine
2. Loop while at least one input channel is still open
3. Use \`select\` to receive from whichever channel is ready
4. When a channel is closed (\`ok\` is false), set it to \`nil\` so \`select\` ignores it
5. Close the output channel when both inputs are exhausted`,

  starterCode: `package main

import "fmt"

func merge(ch1, ch2 <-chan int) <-chan int {
\tout := make(chan int)
\tgo func() {
\t\tdefer close(out)
\t\t// Loop while ch1 or ch2 is still open.
\t\t// Use select to receive from whichever is ready.
\t\t// When ok is false, set the channel to nil and continue.
\t}()
\treturn out
}

func main() {
\ta := make(chan int)
\tb := make(chan int)

\tgo func() {
\t\tfor _, v := range []int{1, 3, 5} {
\t\t\ta <- v
\t\t}
\t\tclose(a)
\t}()

\tgo func() {
\t\tfor _, v := range []int{2, 4, 6} {
\t\t\tb <- v
\t\t}
\t\tclose(b)
\t}()

\tfor v := range merge(a, b) {
\t\tfmt.Println(v)
\t}
}
`,

  solution: `package main

import "fmt"

func merge(ch1, ch2 <-chan int) <-chan int {
\tout := make(chan int)
\tgo func() {
\t\tdefer close(out)
\t\tfor ch1 != nil || ch2 != nil {
\t\t\tselect {
\t\t\tcase v, ok := <-ch1:
\t\t\t\tif !ok {
\t\t\t\t\tch1 = nil
\t\t\t\t\tcontinue
\t\t\t\t}
\t\t\t\tout <- v
\t\t\tcase v, ok := <-ch2:
\t\t\t\tif !ok {
\t\t\t\t\tch2 = nil
\t\t\t\t\tcontinue
\t\t\t\t}
\t\t\t\tout <- v
\t\t\t}
\t\t}
\t}()
\treturn out
}

func main() {
\ta := make(chan int)
\tb := make(chan int)

\tgo func() {
\t\tfor _, v := range []int{1, 3, 5} {
\t\t\ta <- v
\t\t}
\t\tclose(a)
\t}()

\tgo func() {
\t\tfor _, v := range []int{2, 4, 6} {
\t\t\tb <- v
\t\t}
\t\tclose(b)
\t}()

\tfor v := range merge(a, b) {
\t\tfmt.Println(v)
\t}
}
`,

  tests: [
    {
      name: "merge two channels with sequential sends",
      code: `package main

import (
\t"fmt"
\t"sort"
)

{{FUNC}}

func main() {
\ta := make(chan int)
\tb := make(chan int)

\tgo func() {
\t\ta <- 10
\t\ta <- 20
\t\tclose(a)
\t}()

\tgo func() {
\t\tb <- 30
\t\tb <- 40
\t\tclose(b)
\t}()

\tvar results []int
\tfor v := range merge(a, b) {
\t\tresults = append(results, v)
\t}
\tsort.Ints(results)
\tfor _, v := range results {
\t\tfmt.Println(v)
\t}
}`,
      expected: "10\n20\n30\n40\n",
    },
    {
      name: "merge with one empty channel",
      code: `package main

import "fmt"

{{FUNC}}

func main() {
\ta := make(chan int)
\tb := make(chan int)

\tgo func() {
\t\ta <- 1
\t\ta <- 2
\t\tclose(a)
\t}()

\tgo func() {
\t\tclose(b)
\t}()

\tfor v := range merge(a, b) {
\t\tfmt.Println(v)
\t}
}`,
      expected: "1\n2\n",
    },
    {
      name: "merge with both channels empty",
      code: `package main

import "fmt"

{{FUNC}}

func main() {
\ta := make(chan int)
\tb := make(chan int)

\tgo func() { close(a) }()
\tgo func() { close(b) }()

\tcount := 0
\tfor range merge(a, b) {
\t\tcount++
\t}
\tfmt.Println(count)
}`,
      expected: "0\n",
    },
  ],
};
