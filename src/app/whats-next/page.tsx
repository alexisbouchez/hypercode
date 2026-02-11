import type { Metadata } from "next";
import { ContentShell } from "@/components/content-shell";

export const metadata: Metadata = {
  title: "What's Next? - Hypercode",
  description:
    "You have completed all twelve lessons. Here are next steps, project ideas, and references to keep learning Go.",
};

const content = `
## Congratulations

You have completed all twelve lessons. You now have a solid foundation in Go's core language features: packages, variables, control flow, functions, slices, maps, structs, interfaces, error handling, and generics.

That is a real accomplishment. You know enough to read and write Go code, understand Go codebases, and start building your own projects.

## Concurrency

The biggest topic we did not cover is concurrency. Go's goroutines and channels are one of its most powerful features.

A **goroutine** is a lightweight thread of execution. You start one by putting the \`go\` keyword before a function call. Go can run millions of goroutines concurrently.

A **channel** is a typed conduit through which you send and receive values between goroutines. Channels synchronize execution and eliminate the need for explicit locks in many cases.

Concurrency is what makes Go the language of choice for network services, web servers, and distributed systems. It is the natural next topic to learn.

## The Standard Library

Go ships with a comprehensive standard library. Key packages to explore:

- \`net/http\` -- build web servers and HTTP clients
- \`encoding/json\` -- encode and decode JSON
- \`os\` and \`io\` -- file system and I/O operations
- \`testing\` -- write and run tests
- \`context\` -- manage cancellation and timeouts
- \`sync\` -- mutexes, wait groups, and other synchronization primitives
- \`database/sql\` -- database access interface

## Build Something

The best way to learn is to build. Some project ideas:

- **A command-line tool** -- a file organizer, task tracker, or URL shortener
- **A REST API** with \`net/http\` -- user authentication, CRUD operations, JSON responses
- **A concurrent web scraper** -- fetch multiple pages in parallel using goroutines
- **A chat server** using WebSockets -- real-time communication between clients

## References

Here are the best resources for continuing your Go journey:

- [The Go Programming Language Specification](https://go.dev/ref/spec) -- the official language spec, precise and surprisingly readable.
- [Effective Go](https://go.dev/doc/effective_go) -- idiomatic Go patterns and conventions, written by the Go team.
- [Go by Example](https://gobyexample.com) -- annotated code examples covering a wide range of Go features.
- [The Go Blog](https://go.dev/blog) -- official blog with in-depth articles on language features and design decisions.
- [The Go Playground](https://go.dev/play) -- run and share Go code in the browser.
- *The Go Programming Language* by Alan Donovan and Brian Kernighan (Addison-Wesley, 2015) -- the most comprehensive Go book, written by two of the best technical authors.
- [Go Standard Library Documentation](https://pkg.go.dev/std) -- detailed documentation for every package in the standard library.
`;

export default function WhatsNextPage() {
  return (
    <ContentShell
      title="What's Next?"
      content={content}
      activePage="whats-next"
      prevHref="/lessons/generic-types"
    />
  );
}
