import type { Course } from "./types";
import { chapters as goChapters, lessons as goLessons } from "@/lib/lessons";
import { zigChapters, zigLessons } from "@/lib/lessons/zig";
import { sqlChapters, sqlLessons } from "@/lib/lessons/sql";
import { arm64Chapters, arm64Lessons } from "@/lib/lessons/arm64";
import { cChapters, cLessons } from "@/lib/lessons/c";

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

const sqlIntroductionContent = `
## Why PostgreSQL?

PostgreSQL is the world's most advanced open-source relational database. It is reliable, feature-rich, and standards-compliant. Here is what makes it stand out:

- **Battle-tested** -- Used by Apple, Instagram, Spotify, and thousands of other companies in production.
- **Feature-rich** -- JSON, full-text search, arrays, window functions, CTEs, and more -- all built in.
- **Standards-compliant** -- Follows the SQL standard closely, so what you learn here applies broadly.
- **Free and open source** -- No licensing fees, backed by a strong community since 1996.
- **Extensible** -- Custom types, operators, index methods, and procedural languages.

## The Story

PostgreSQL traces its origins to the POSTGRES project at UC Berkeley, started by Professor Michael Stonebraker in 1986. The project was a research successor to the Ingres database. In 1996, the project was renamed PostgreSQL to reflect its SQL support, and the open-source community took over development.

Today, PostgreSQL is developed by a global community of contributors. Major releases come out annually, each adding significant features while maintaining backward compatibility.

## Who Uses PostgreSQL

PostgreSQL powers some of the world's largest applications:

- **Instagram** -- stores hundreds of millions of user records.
- **Spotify** -- manages music catalog and user data.
- **Apple** -- uses PostgreSQL for various internal services.
- **Reddit, Twitch, The Guardian** -- all rely on PostgreSQL in production.

It is the default choice for startups and enterprises alike, and the most popular database on platforms like Supabase, Neon, and Railway.

## What You Will Learn

This course contains **22 lessons** organized into **8 chapters**:

1. **SQL Basics** -- SELECT queries, filtering with WHERE, sorting, and pagination.
2. **Tables** -- Data types, CREATE TABLE, constraints, keys, and ALTER TABLE.
3. **CRUD Operations** -- INSERT, UPDATE, DELETE, upsert, and bulk operations.
4. **Joins** -- INNER JOIN, LEFT/RIGHT/FULL OUTER JOIN, self joins, and cross joins.
5. **Aggregations** -- COUNT, SUM, AVG, GROUP BY, HAVING, ROLLUP, and CUBE.
6. **Subqueries & CTEs** -- Nested queries, correlated subqueries, and Common Table Expressions.
7. **Advanced SQL** -- Window functions, JSON operations, arrays, and full-text search.
8. **Performance** -- Indexes, EXPLAIN ANALYZE, query optimization, and database design.

Each lesson explains a concept, demonstrates it with SQL examples, and gives you an exercise to practice against a live PostgreSQL database running in your browser.

Let's get started.
`;

const sqlWhatsNextContent = `
## Congratulations

You have completed all 22 lessons. You now have a solid foundation in SQL and PostgreSQL: querying, filtering, joining, aggregating, subqueries, CTEs, window functions, JSON, indexes, and database design.

That is a real accomplishment. You can read and write SQL confidently, design database schemas, and optimize queries.

## What to Explore Next

Here are topics to dive deeper into:

- **Transactions** -- ACID properties, BEGIN/COMMIT/ROLLBACK, isolation levels, and deadlocks.
- **Stored Procedures and Functions** -- PL/pgSQL for server-side logic.
- **Triggers** -- Automatically execute functions when data changes.
- **Views and Materialized Views** -- Save complex queries as virtual or cached tables.
- **Partitioning** -- Split large tables for better performance.
- **Replication** -- Set up read replicas and high availability.

## Build Something

The best way to learn is to build. Some project ideas:

- **A REST API with a database backend** -- CRUD endpoints backed by PostgreSQL.
- **A data dashboard** -- aggregate and visualize data from multiple tables.
- **A multi-tenant application** -- row-level security and schema isolation.
- **A search engine** -- full-text search with ranking and highlighting.

## References

- [PostgreSQL Official Documentation](https://www.postgresql.org/docs/current/) -- comprehensive and well-written.
- [PostgreSQL Tutorial](https://www.postgresqltutorial.com/) -- practical examples for common tasks.
- [Use The Index, Luke](https://use-the-index-luke.com/) -- deep dive into SQL indexing and performance.
- [pgexercises.com](https://pgexercises.com/) -- interactive SQL exercises.
- *Designing Data-Intensive Applications* by Martin Kleppmann (O'Reilly, 2017) -- the best book on database internals and distributed systems.
`;

const arm64IntroductionContent = `
## Why ARM64 Assembly?

ARM64 (AArch64) is the dominant processor architecture of the modern era. It powers every iPhone, every Android phone, Apple's M-series Macs, AWS Graviton servers, and billions of embedded devices. Understanding ARM64 assembly gives you direct insight into how these processors execute code.

- **The architecture of the future** -- ARM's power efficiency is moving it from mobile into servers, desktops, and supercomputers.
- **Clean, modern design** -- ARM64 is a modern RISC architecture with a regular, orthogonal instruction set that is easier to learn than x86.
- **Fixed-width instructions** -- Every instruction is exactly 32 bits. No variable-length decoding complexity.
- **31 general-purpose registers** -- Generous register file means less memory traffic.
- **Real-world relevance** -- Knowing ARM64 assembly helps you understand compiler output, debug low-level issues, and write performance-critical code.

## The Story

ARM started in 1983 at Acorn Computers in Cambridge, England. Sophie Wilson and Steve Furber designed the original ARM1 processor. The name originally stood for "Acorn RISC Machine," later "Advanced RISC Machine."

ARM's big insight was that a simpler processor could be faster and more power-efficient. This bet paid off spectacularly: ARM processors now ship in over 250 billion chips, more than any other processor family in history.

ARM64 (AArch64) was introduced in 2011 with the ARMv8-A architecture, bringing 64-bit computing to the ARM world. It was a clean break from the 32-bit ARM instruction set, designed from scratch with modern computing needs in mind.

## What You Will Learn

This course contains **16 lessons** organized into **6 chapters**:

1. **Foundations** -- Registers, MOV, arithmetic, and your first program using Linux syscalls.
2. **Memory** -- Load/store architecture, addressing modes, and the stack.
3. **Control Flow** -- Condition flags, CMP, conditional branches, and loops.
4. **Functions** -- BL/RET, the calling convention, and recursive functions.
5. **Bitwise Operations** -- AND, OR, XOR, shifts, and bit manipulation patterns.
6. **Putting It Together** -- String operations and sorting algorithms.

Each lesson explains a concept, demonstrates it with code, and gives you an exercise. Your code runs directly in the browser using a custom ARM64 interpreter -- no native toolchain needed.

Let's get started.
`;

const arm64WhatsNextContent = `
## Congratulations

You have completed all 16 lessons. You now understand ARM64's register model, memory operations, control flow, functions, bitwise operations, and have implemented real algorithms in assembly.

That is a real accomplishment. ARM64 assembly is not easy, and you have built a solid foundation.

## What to Explore Next

- **System calls** -- Linux has hundreds of syscalls beyond write and exit: open, read, mmap, socket, and more.
- **SIMD/NEON** -- ARM64's vector instructions for parallel data processing.
- **Floating point** -- ARM64 has 32 dedicated 128-bit vector/FP registers (V0-V31).
- **Atomic operations** -- LDXR/STXR for lock-free concurrent programming.
- **Exception handling** -- How ARM64 handles interrupts, page faults, and system calls at the hardware level.
- **Performance optimization** -- Instruction scheduling, cache effects, and branch prediction.

## Build Something

- **A simple shell** -- Read commands, fork processes, execute programs.
- **A bootloader** -- Write bare-metal ARM64 code that runs without an OS.
- **An emulator** -- Build an emulator for a simpler architecture (like CHIP-8) in ARM64.
- **Optimize a hot loop** -- Take a C function, look at the compiler output, and hand-optimize it.

## References

- [ARM Architecture Reference Manual](https://developer.arm.com/documentation/ddi0487/latest/) -- the definitive reference for ARM64.
- [ARM64 Instruction Set Quick Reference](https://developer.arm.com/documentation/100076/0100/) -- concise instruction reference.
- [Azeria Labs ARM Assembly Basics](https://azeria-labs.com/writing-arm-assembly-language/) -- excellent tutorials on ARM assembly.
- [ARM Assembly Internals and Reverse Engineering](https://www.wiley.com/en-us/ARM+Assembly+Internals+and+Reverse+Engineering-p-9781119745303) by Maria Markstedter -- comprehensive book.
- [Computer Organization and Design: ARM Edition](https://www.elsevier.com/books/computer-organization-and-design-arm-edition/patterson/978-0-12-801733-3) by Patterson and Hennessy.
`;

const cIntroductionContent = `
## Why C?

C is one of the most important programming languages in history. Created by Dennis Ritchie at Bell Labs in 1972, it is the language behind operating systems, embedded systems, databases, compilers, and countless other foundational software.

- **Close to the metal** -- C gives you direct access to memory and hardware. No garbage collector, no runtime overhead.
- **Portable** -- C runs on virtually every platform, from microcontrollers to supercomputers.
- **The lingua franca** -- Most other languages (Python, Ruby, Node.js) have runtimes written in C. Understanding C means understanding how software really works.
- **Simple but powerful** -- C has a small core: variables, functions, pointers, structs, and arrays. These simple building blocks compose into complex systems.

## The Story

Dennis Ritchie developed C at Bell Labs between 1969 and 1973, alongside the Unix operating system. C evolved from an earlier language called B (created by Ken Thompson), which itself descended from BCPL. The first edition of *The C Programming Language* by Brian Kernighan and Dennis Ritchie (known as K&R) was published in 1978 and became one of the most influential programming books ever written.

C was standardized by ANSI in 1989 (C89/C90) and has been updated several times since: C99, C11, C17, and C23.

## The ARM64 Assembly View

This course has a unique feature: when you run your C code, you can see the **ARM64 assembly** it compiles to. Click the **Assembly** tab after running to see the actual machine instructions generated by the TCC compiler.

This gives you a window into how your high-level C code translates to low-level processor instructions -- how variables become registers, how function calls become branch instructions, and how loops become conditional jumps.

## What You Will Learn

This course contains **14 lessons** organized into **6 chapters**:

1. **C Basics** -- Your first C program, variables, types, and arithmetic.
2. **Control Flow** -- Conditionals with \`if\`/\`else\` and loops with \`for\`/\`while\`.
3. **Functions** -- Declaring functions, parameters, return values, and recursion.
4. **Arrays and Strings** -- Arrays, C-style strings, and multi-dimensional arrays.
5. **Pointers** -- Address-of, dereference, pointer arithmetic, and arrays as pointers.
6. **Structs** -- Defining structs, member access, and struct pointers with the arrow operator.

Each lesson explains a concept, demonstrates it with code examples, and gives you an exercise to practice. Your code is compiled to ARM64 by TCC running in your browser, then executed by our ARM64 interpreter.

Let's get started.
`;

const cWhatsNextContent = `
## Congratulations

You have completed all 14 lessons. You now have a solid foundation in C: variables, types, control flow, functions, arrays, strings, pointers, and structs.

That is a real accomplishment. C is not easy, and understanding it gives you a deep appreciation for how software works at the system level.

## What to Explore Next

Here are topics to dive deeper into:

- **Dynamic memory allocation** -- \`malloc\`, \`free\`, \`calloc\`, \`realloc\` for heap-allocated memory.
- **File I/O** -- \`fopen\`, \`fread\`, \`fwrite\`, \`fprintf\` for reading and writing files.
- **Preprocessor** -- \`#define\`, \`#ifdef\`, \`#include\` for compile-time configuration.
- **Linked lists** -- Build dynamic data structures with structs and pointers.
- **Header files** -- Organize larger programs across multiple files.
- **Makefiles** -- Build systems for C projects.

## Build Something

The best way to learn is to build. Some project ideas:

- **A command-line calculator** -- parse and evaluate arithmetic expressions
- **A linked list library** -- insert, delete, search, and sort operations
- **A simple shell** -- read commands, fork processes, and execute programs
- **A memory allocator** -- implement your own \`malloc\` and \`free\`

## References

- *The C Programming Language* by Brian Kernighan and Dennis Ritchie (Prentice Hall, 1988) -- the definitive C book, still relevant today.
- [C Reference](https://en.cppreference.com/w/c) -- comprehensive online reference for the C standard library.
- [Beej's Guide to C Programming](https://beej.us/guide/bgc/) -- free online guide, well-written and practical.
- [Modern C](https://gustedt.gitlabpages.inria.fr/modern-c/) by Jens Gustedt -- a modern take on C programming.
- [CS:APP](https://csapp.cs.cmu.edu/) -- *Computer Systems: A Programmer's Perspective* covers how C maps to machine code.
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
  {
    id: "postgresql",
    title: "PostgreSQL",
    description: "Learn SQL and PostgreSQL from scratch. Master queries, joins, aggregations, window functions, and database design with a live database in your browser.",
    language: "sql",
    chapters: sqlChapters,
    lessons: sqlLessons,
    runtimeLabel: "SQL runtime",
    introductionContent: sqlIntroductionContent,
    whatsNextContent: sqlWhatsNextContent,
  },
  {
    id: "arm64",
    title: "ARM64 Assembly",
    description: "Learn ARM64 assembly language from scratch. Understand how processors execute code with registers, memory, branches, and syscalls.",
    language: "arm64",
    chapters: arm64Chapters,
    lessons: arm64Lessons,
    runtimeLabel: "ARM64 runtime",
    introductionContent: arm64IntroductionContent,
    whatsNextContent: arm64WhatsNextContent,
  },
  {
    id: "c",
    title: "C",
    description: "Learn the C programming language from scratch. See the ARM64 assembly your code compiles to, compiled and executed entirely in your browser.",
    language: "c",
    chapters: cChapters,
    lessons: cLessons,
    runtimeLabel: "TCC compiler",
    introductionContent: cIntroductionContent,
    whatsNextContent: cWhatsNextContent,
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
