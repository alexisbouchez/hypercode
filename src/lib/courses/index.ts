import type { Course } from "./types";
import { chapters as goChapters, lessons as goLessons } from "@/lib/lessons";
import { zigChapters, zigLessons } from "@/lib/lessons/zig";

const goIntroductionContent = `
## Why Go?

Go is a statically typed, compiled language designed for simplicity and efficiency. It compiles fast, runs fast, and is easy to read. Here is what makes it stand out:

- **Fast compilation** -- Go compiles entire projects in seconds, even large codebases.
- **Simple syntax** -- The language has only 25 keywords. If you know any C-style language, you can read Go code immediately.
- **Built-in concurrency** -- Goroutines and channels make concurrent programming straightforward.
- **Rich standard library** -- HTTP servers, JSON encoding, cryptography, testing -- all built in, no third-party dependencies required.
- **Single binary deployment** -- Go compiles to a single static binary. No runtime, no VM, no dependencies to install on the target machine.

## The Story

Go was created at Google in 2007 by Robert Griesemer, Rob Pike, and Ken Thompson. The three designers were frustrated with the complexity of existing systems languages, particularly C++: slow builds, complicated dependency management, and a language specification that had grown unwieldy over decades.

Their goal was a language that combined the performance and safety of a compiled language with the ease of use of a dynamic one. Go was open-sourced in November 2009 and reached its first stable release, Go 1.0, in March 2012 with a strong backward-compatibility promise.

Rob Pike had co-created the Plan 9 operating system and the UTF-8 encoding. Ken Thompson had co-created Unix and the C programming language. Robert Griesemer had worked on the V8 JavaScript engine and the Java HotSpot VM.

## Who Uses Go

Go has become the backbone of modern cloud infrastructure. Some of the most influential open-source projects are written in Go:

- **Docker** -- the containerization platform that changed how software is deployed.
- **Kubernetes** -- the container orchestration system that powers cloud-native applications.
- **Terraform** -- the infrastructure-as-code tool used to manage cloud resources.

Major companies use Go extensively: Google, Uber, Dropbox, Twitch, Cloudflare, and many others. It is especially popular for building microservices, CLI tools, DevOps tooling, and network services.

## What You Will Learn

This course contains **17 lessons** organized into **8 chapters**:

1. **Foundations** -- How Go programs are structured: packages, imports, and the \`main\` function. Variables, types, and constants.
2. **Control Flow** -- Conditionals with \`if\`/\`else\` and \`switch\`. Loops with \`for\`.
3. **Functions** -- Declaring functions, multiple return values, named returns, and deferred execution.
4. **Data Structures** -- Slices (dynamic arrays), maps (hash tables), and string operations.
5. **Custom Types** -- Pointers, structs, and interfaces.
6. **Error Handling** -- Go's explicit approach to errors using values instead of exceptions.
7. **Generics** -- Type parameters for functions and data structures.
8. **Concurrency** -- Goroutines, channels, and the select statement.

Each lesson explains a concept, demonstrates it with code examples, and gives you an exercise to practice.

Let's get started.
`;

const goWhatsNextContent = `
## Congratulations

You have completed all the lessons. You now have a solid foundation in Go's core language features: packages, variables, control flow, functions, slices, maps, structs, interfaces, error handling, generics, and concurrency.

That is a real accomplishment. You know enough to read and write Go code, understand Go codebases, and start building your own projects.

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
- *The Go Programming Language* by Alan Donovan and Brian Kernighan (Addison-Wesley, 2015) -- the most comprehensive Go book.
- [Go Standard Library Documentation](https://pkg.go.dev/std) -- detailed documentation for every package in the standard library.
`;

const zigIntroductionContent = `
## Why Zig?

Zig is a systems programming language designed for robustness, performance, and maintainability. It aims to be a better C -- with modern ergonomics but without hidden control flow, hidden allocations, or undefined behavior.

- **No hidden control flow** -- No operator overloading, no hidden function calls. What you see is what the machine executes.
- **Compile-time execution** -- Zig's \`comptime\` lets you run arbitrary code at compile time, replacing the need for macros and code generation.
- **Manual memory management done right** -- Allocators are explicit and passed as parameters. No global allocator, no garbage collector.
- **C interop** -- Zig can import C headers and call C functions directly, with no bindings or FFI boilerplate.
- **Safety without runtime cost** -- Bounds checking, null checking, and undefined behavior detection in debug builds, stripped in release builds.

## The Story

Zig was created by Andrew Kelley, who started working on it in 2015 after becoming frustrated with C and C++ while working on performance-critical systems. The first public release came in 2017.

Andrew's goal was a language that could be used everywhere C is used -- operating systems, embedded systems, high-performance computing -- but with a focus on making the programmer's intent clear and catching bugs at compile time rather than at runtime.

The Zig Software Foundation was established to support the development of the language and its ecosystem.

## Who Uses Zig

Zig is gaining traction in systems programming and infrastructure:

- **Bun** -- the fast JavaScript runtime and toolkit is written in Zig.
- **TigerBeetle** -- a high-performance financial transactions database.
- **Mach Engine** -- a game engine built on Zig.
- **Uber** -- uses Zig for network proxy infrastructure.

Zig is also used as a drop-in C/C++ cross-compiler, since its toolchain includes a complete C compiler with cross-compilation support out of the box.

## What You Will Learn

This course contains **12 lessons** organized into **6 chapters**:

1. **Foundations** -- How Zig programs are structured, variables, types, and printing output.
2. **Control Flow** -- Conditionals with \`if\`/\`else\` and \`switch\`. Loops with \`while\` and \`for\`.
3. **Functions** -- Function declarations, error unions, and error handling.
4. **Data Structures** -- Arrays, slices, and structs with methods.
5. **Memory** -- Pointers, allocators, and manual memory management.
6. **Advanced** -- Compile-time execution, optionals, and tagged unions.

Each lesson explains a concept, demonstrates it with code examples, and gives you an exercise to practice.

Let's get started.
`;

const zigWhatsNextContent = `
## Congratulations

You have completed all twelve lessons. You now have a solid foundation in Zig's core features: variables, control flow, functions, error handling, arrays, structs, pointers, allocators, comptime, and optionals.

That is a real accomplishment. You understand the key ideas that make Zig unique among systems languages.

## What to Explore Next

Here are topics to dive deeper into:

- **Async/Await** -- Zig has built-in support for async functions and event loops.
- **SIMD and Vectors** -- Zig exposes SIMD operations through its vector types.
- **C Interop** -- Import and use C libraries directly in Zig code with \`@cImport\`.
- **Build System** -- Zig's \`build.zig\` replaces Makefiles and CMake with Zig code.
- **Testing** -- Zig has built-in test blocks that run with \`zig test\`.

## Build Something

The best way to learn is to build. Some project ideas:

- **A command-line tool** -- a file processor, argument parser, or system utility
- **A simple HTTP server** -- using Zig's standard library networking
- **A data structure library** -- implement a hash map, linked list, or B-tree
- **A game** -- using a Zig game framework or raw SDL bindings

## References

- [Zig Language Reference](https://ziglang.org/documentation/master/) -- the official language documentation.
- [Zig Standard Library Documentation](https://ziglang.org/documentation/master/std/) -- API docs for the standard library.
- [Ziglings](https://codeberg.org/ziglings/exercises/) -- small exercises to learn Zig.
- [Zig News](https://zig.news/) -- community articles and tutorials.
- [Zig GitHub](https://github.com/ziglang/zig) -- the source code and issue tracker.
`;

export const courses: Course[] = [
  {
    id: "go",
    title: "Go",
    description: "Learn the Go programming language from scratch. Build fast, reliable software with Go's simple syntax, built-in concurrency, and powerful standard library.",
    language: "go",
    chapters: goChapters,
    lessons: goLessons,
    pdfPath: "/go-book.pdf",
    runtimeLabel: "Go runtime",
    introductionContent: goIntroductionContent,
    whatsNextContent: goWhatsNextContent,
  },
  {
    id: "zig",
    title: "Zig",
    description: "Learn the Zig programming language from scratch. Master manual memory management, comptime metaprogramming, and zero-cost abstractions.",
    language: "zig",
    chapters: zigChapters,
    lessons: zigLessons,
    runtimeLabel: "Zig runtime",
    introductionContent: zigIntroductionContent,
    whatsNextContent: zigWhatsNextContent,
  },
];

export function getCourse(id: string): Course | undefined {
  return courses.find((c) => c.id === id);
}

export function getCourseOrThrow(id: string): Course {
  const course = getCourse(id);
  if (!course) throw new Error(`Course not found: ${id}`);
  return course;
}
