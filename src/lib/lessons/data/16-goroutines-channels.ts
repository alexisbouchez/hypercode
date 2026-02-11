import type { Lesson } from "../types";

export const concurrency: Lesson = {
  id: "concurrency",
  title: "Goroutines and Channels",
  chapterId: "concurrency",
  content: `## Concurrency in Go

Concurrency is one of Go's defining features. Go makes it easy to run functions concurrently and communicate between them safely.

### Goroutines

A goroutine is a lightweight thread managed by the Go runtime. You start one by putting the \`go\` keyword before a function call:

\`\`\`go
go doSomething()
\`\`\`

That is it. The function runs concurrently with the rest of your program. Goroutines are extremely cheap: you can launch thousands without concern.

\`\`\`go
func printNumbers() {
    for i := 1; i <= 5; i++ {
        fmt.Println(i)
    }
}

func main() {
    go printNumbers() // runs concurrently
    fmt.Println("started")
    time.Sleep(time.Second) // wait for goroutine
}
\`\`\`

### Channels

Channels are Go's way of communicating between goroutines. A channel is a typed conduit through which you send and receive values:

\`\`\`go
ch := make(chan int)  // create a channel of ints

go func() {
    ch <- 42  // send a value into the channel
}()

val := <-ch  // receive a value from the channel
fmt.Println(val) // 42
\`\`\`

The \`<-\` operator is used for both sending and receiving. Sends and receives block until the other side is ready, which naturally synchronizes goroutines.

### Channel Direction

Function signatures can restrict a channel to send-only or receive-only:

\`\`\`go
func producer(ch chan<- int) {  // can only send to ch
    ch <- 42
}

func consumer(ch <-chan int) {  // can only receive from ch
    val := <-ch
    fmt.Println(val)
}
\`\`\`

This makes your intent clear and catches mistakes at compile time.

### Closing Channels and Range

A sender can close a channel to signal that no more values will be sent. The receiver can detect this:

\`\`\`go
ch := make(chan int)

go func() {
    for i := 0; i < 5; i++ {
        ch <- i
    }
    close(ch)
}()

for val := range ch {
    fmt.Println(val)
}
\`\`\`

The \`range\` loop receives values from the channel until it is closed. This is the cleanest way to consume all values from a channel.

### Buffered Channels

By default, channels are *unbuffered*: a send blocks until another goroutine receives. A **buffered channel** has a capacity, allowing sends to proceed without a receiver until the buffer is full:

\`\`\`go
ch := make(chan int, 3)  // buffer holds up to 3 values

ch <- 1  // does not block
ch <- 2  // does not block
ch <- 3  // does not block
// ch <- 4 would block here (buffer full)

fmt.Println(<-ch) // 1
\`\`\`

Buffered channels are useful when the sender and receiver run at different speeds, or when you know the exact number of values that will be sent.

### Synchronization with Channels

When you need to wait for a goroutine to finish, you can use a channel as a signal:

\`\`\`go
done := make(chan bool)

go func() {
    fmt.Println("working...")
    done <- true  // signal completion
}()

<-done  // wait for the goroutine to finish
\`\`\`

In production Go code, \`sync.WaitGroup\` is the standard tool for waiting on multiple goroutines. It is not available in this playground, but you will encounter it in virtually every real Go codebase:

\`\`\`go
// Real Go code (not available in this playground):
var wg sync.WaitGroup
for i := 0; i < 3; i++ {
    wg.Add(1)
    go func(n int) {
        defer wg.Done()
        fmt.Println(n)
    }(i)
}
wg.Wait()
\`\`\`

### Your Task

Write a function \`squares(n int) <-chan int\` that returns a receive-only channel. It should start a goroutine that sends the squares of 1 through \`n\` into the channel, then closes it.`,

  starterCode: `package main

import "fmt"

func squares(n int) <-chan int {
\t// Your code here
\treturn nil
}

func main() {
\tfor val := range squares(5) {
\t\tfmt.Println(val)
\t}
}
`,

  solution: `package main

import "fmt"

func squares(n int) <-chan int {
\tch := make(chan int)
\tgo func() {
\t\tfor i := 1; i <= n; i++ {
\t\t\tch <- i * i
\t\t}
\t\tclose(ch)
\t}()
\treturn ch
}

func main() {
\tfor val := range squares(5) {
\t\tfmt.Println(val)
\t}
}
`,

  tests: [
    {
      name: "squares(5) returns 1, 4, 9, 16, 25",
      code: `package main

import "fmt"

{{FUNC}}

func main() {
\tfor val := range squares(5) {
\t\tfmt.Println(val)
\t}
}`,
      expected: "1\n4\n9\n16\n25\n",
    },
    {
      name: "squares(3) returns 1, 4, 9",
      code: `package main

import "fmt"

{{FUNC}}

func main() {
\tfor val := range squares(3) {
\t\tfmt.Println(val)
\t}
}`,
      expected: "1\n4\n9\n",
    },
    {
      name: "squares(1) returns 1",
      code: `package main

import "fmt"

{{FUNC}}

func main() {
\tfor val := range squares(1) {
\t\tfmt.Println(val)
\t}
}`,
      expected: "1\n",
    },
  ],
};
