import type { Course } from "./types";
import { chapters as goChapters, lessons as goLessons } from "@/lib/lessons";
import { zigChapters, zigLessons } from "@/lib/lessons/zig";
import { sqlChapters, sqlLessons } from "@/lib/lessons/sql";
import { arm64Chapters, arm64Lessons } from "@/lib/lessons/arm64";
import { cChapters, cLessons } from "@/lib/lessons/c";
import { gleamChapters, gleamLessons } from "@/lib/lessons/gleam";
import { rChapters, rLessons } from "@/lib/lessons/r";
import { holycChapters, holycLessons } from "@/lib/lessons/holyc";

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

This course contains **18 lessons** organized into **7 chapters**:

1. **C Basics** -- Your first C program, variables, types, arithmetic, and type casting.
2. **Control Flow** -- Conditionals with \`if\`/\`else\`, loops with \`for\`/\`while\`, and \`switch\` statements.
3. **Enums and Bitwise** -- Enum declarations, named constants, and bitwise operators for flags and bit manipulation.
4. **Functions** -- Declaring functions, parameters, return values, and recursion.
5. **Arrays and Strings** -- Arrays, C-style strings, and multi-dimensional arrays.
6. **Pointers** -- Address-of, dereference, pointer arithmetic, and arrays as pointers.
7. **Structs** -- Defining structs, member access, and struct pointers with the arrow operator.

Each lesson explains a concept, demonstrates it with code examples, and gives you an exercise to practice. Your code is compiled to ARM64 by TCC running in your browser, then executed by our ARM64 interpreter.

Let's get started.
`;

const cWhatsNextContent = `
## Congratulations

You have completed all 18 lessons. You now have a solid foundation in C: variables, types, control flow, switch statements, enums, bitwise operators, functions, arrays, strings, pointers, and structs.

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

const gleamIntroductionContent = `
## Why Gleam?

Gleam is a friendly functional programming language with a powerful type system that catches mistakes before your code runs. It compiles to both Erlang and JavaScript, letting you build reliable software for any platform.

- **Type safe** -- Gleam's type system catches errors at compile time. No null pointer exceptions, no undefined is not a function. If it compiles, it works.
- **Functional** -- Immutable data, pattern matching, and the pipe operator make code easy to read and reason about.
- **Erlang VM** -- Gleam runs on the BEAM, the battle-tested virtual machine behind WhatsApp, Discord, and other systems serving millions of users.
- **JavaScript target** -- Gleam also compiles to JavaScript, so you can share code between server and client.
- **Friendly** -- Clear error messages, simple syntax, and a welcoming community. Gleam is designed to be a joy to use.

## The Story

Gleam was created by Louis Pilfold, who started working on it in 2018. Louis wanted a language that combined the reliability of the Erlang ecosystem with the developer experience of modern typed languages.

The Erlang VM is legendary for building fault-tolerant, concurrent systems -- it powers phone networks, messaging apps, and databases that need to stay up 24/7. But Erlang's dynamic typing and unusual syntax can be barriers for newcomers. Gleam bridges this gap with a familiar syntax and a strong type system, while giving you full access to the BEAM ecosystem.

Gleam reached version 1.0 in March 2024, marking its stability commitment.

## Who Uses Gleam

Gleam is a young but rapidly growing language:

- **Gleam itself** -- the Gleam compiler is written in Rust, and its standard library and package manager are written in Gleam.
- **Web applications** -- frameworks like Lustre (frontend) and Wisp (backend) let you build full-stack Gleam apps.
- **The BEAM ecosystem** -- Gleam interoperates seamlessly with Erlang and Elixir libraries.

## What You Will Learn

This course contains **16 lessons** organized into **7 chapters**:

1. **Foundations** -- How Gleam programs are structured: imports, functions, and printing output.
2. **Data Types** -- Strings, numbers, and the operations you can perform on them.
3. **Control Flow** -- Case expressions and pattern matching.
4. **Collections** -- Lists and tuples for grouping data.
5. **Custom Types** -- Sum types, records, and generics.
6. **Error Handling** -- The Result type and the use expression.
7. **Functional Patterns** -- The pipe operator and higher-order functions.

Each lesson explains a concept, demonstrates it with code examples, and gives you an exercise to practice.

Let's get started.
`;

const gleamWhatsNextContent = `
## Congratulations

You have completed all 16 lessons. You now have a solid foundation in Gleam: functions, types, pattern matching, custom types, error handling, pipes, and higher-order functions.

That is a real accomplishment. You understand the key ideas that make Gleam a powerful and enjoyable language.

## What to Explore Next

Here are topics to dive deeper into:

- **The BEAM** -- Learn about processes, message passing, and OTP for building fault-tolerant concurrent systems.
- **Lustre** -- A frontend framework for building interactive web apps in Gleam.
- **Wisp** -- A web framework for building backend services in Gleam.
- **Gleam OTP** -- Typed wrappers around Erlang's OTP behaviors.
- **JavaScript interop** -- Call JavaScript libraries from Gleam code.

## Build Something

The best way to learn is to build. Some project ideas:

- **A command-line tool** -- parse arguments, read files, and produce output
- **A web API** with Wisp -- routes, JSON responses, and database queries
- **A frontend app** with Lustre -- components, state management, and effects
- **A Discord bot** -- using Gleam on the BEAM for real-time message handling

## References

- [Gleam Language Tour](https://tour.gleam.run/) -- interactive guide to every Gleam feature.
- [Gleam Documentation](https://gleam.run/documentation/) -- official language documentation.
- [Gleam Standard Library](https://hexdocs.pm/gleam_stdlib/) -- API docs for the standard library.
- [Gleam Packages](https://packages.gleam.run/) -- the Gleam package index.
- [Awesome Gleam](https://github.com/gleam-lang/awesome-gleam) -- community-curated list of Gleam resources.
- [Gleam Discord](https://discord.gg/Fm8Pwmy) -- the official community chat.
`;

const rIntroductionContent = `
## Why R?

R is the language of statistics and data science. It was designed from the ground up for data analysis, visualization, and statistical computing. Here is what makes it stand out:

- **Built for data** -- Vectors, data frames, and matrices are first-class citizens. Data manipulation is natural and expressive.
- **Unmatched statistics** -- From t-tests to Bayesian inference, R has every statistical method built in or available through packages.
- **Visualization** -- R's plotting capabilities, especially ggplot2, produce publication-quality graphics with minimal code.
- **CRAN** -- The Comprehensive R Archive Network hosts over 20,000 packages covering every domain of data analysis.
- **Interactive analysis** -- R excels at exploratory data analysis with its REPL and notebook-style workflows.

## The Story

R was created by Ross Ihaka and Robert Gentleman at the University of Auckland, New Zealand, in 1993. The name "R" is a play on the creators' first names and a nod to S, the language that inspired it.

S was developed at Bell Labs by John Chambers in the 1970s as a language for statistical computing. R reimplemented S as free, open-source software and quickly surpassed it. The first stable release, R 1.0.0, came in February 2000.

Today, R is maintained by the R Core Team and has one of the most active open-source communities in data science.

## Who Uses R

R is the standard tool in many fields:

- **Academia** -- the dominant language for statistical research and publications.
- **Pharmaceutical industry** -- used for clinical trial analysis and FDA submissions.
- **Finance** -- risk modeling, time series analysis, and quantitative trading.
- **Tech companies** -- Google, Facebook, Microsoft, and Twitter use R for data analysis.

The tidyverse ecosystem, created by Hadley Wickham, has made R accessible to a much broader audience with packages like dplyr, ggplot2, and tidyr.

## What You Will Learn

This course contains **16 lessons** organized into **7 chapters**:

1. **Foundations** -- Printing output, variables, types, and arithmetic.
2. **Vectors** -- Creating vectors, vectorized operations, indexing, and filtering.
3. **Control Flow** -- Conditionals and loops.
4. **Functions** -- Defining functions, default arguments, closures, and higher-order functions.
5. **Data Structures** -- Lists, matrices, and data frames.
6. **Data Manipulation** -- Apply functions and data frame operations.
7. **Strings** -- String manipulation with paste, gsub, and sprintf.

Each lesson explains a concept, demonstrates it with code examples, and gives you an exercise to practice.

Let's get started.
`;

const rWhatsNextContent = `
## Congratulations

You have completed all 16 lessons. You now have a solid foundation in R: variables, vectors, control flow, functions, lists, matrices, data frames, apply functions, and string operations.

That is a real accomplishment. You understand the core building blocks that make R powerful for data analysis.

## What to Explore Next

Here are topics to dive deeper into:

- **ggplot2** -- The most popular R visualization package. Learn the grammar of graphics.
- **dplyr** -- Data manipulation with filter, select, mutate, summarize, and group_by.
- **tidyr** -- Reshape data with pivot_longer and pivot_wider.
- **R Markdown** -- Create reproducible reports combining code, output, and narrative.
- **Shiny** -- Build interactive web applications entirely in R.
- **Statistical modeling** -- Linear models, GLMs, and machine learning with caret or tidymodels.

## Build Something

The best way to learn is to build. Some project ideas:

- **An exploratory data analysis** -- download a dataset from Kaggle and analyze it with dplyr and ggplot2.
- **A statistical report** -- run hypothesis tests and build regression models in R Markdown.
- **A Shiny dashboard** -- create an interactive visualization app.
- **A data pipeline** -- clean, transform, and analyze a messy real-world dataset.

## References

- [R for Data Science](https://r4ds.hadley.nz/) by Hadley Wickham -- the best introduction to modern R.
- [Advanced R](https://adv-r.hadley.nz/) by Hadley Wickham -- deep dive into R's internals and programming patterns.
- [The R Manuals](https://cran.r-project.org/manuals.html) -- official documentation from the R Core Team.
- [CRAN Task Views](https://cran.r-project.org/web/views/) -- curated lists of packages by topic.
- [RStudio Cheatsheets](https://posit.co/resources/cheatsheets/) -- quick reference cards for popular packages.
- [R-bloggers](https://www.r-bloggers.com/) -- community blog aggregator for R content.
`;

const holycIntroductionContent = `
## Why HolyC?

HolyC is the programming language Terry A. Davis created for TempleOS — a complete operating system he built alone over more than a decade. It is a C dialect with unique features designed for simplicity, directness, and a close relationship between programmer and machine.

- **No preprocessor** -- HolyC has no \`#define\`. Metaprogramming is done with \`#exe\` blocks that run real code at compile time.
- **No \`continue\`** -- Intentionally omitted to encourage explicit control flow with \`goto\`.
- **Capital built-ins** -- \`Print\`, \`TRUE\`, \`FALSE\` and other built-ins are capitalized to distinguish them from user code.
- **Default arguments** -- Functions can have default parameter values at any position, unlike C.
- **Range switch cases** -- \`case 1...5:\` matches a range of values in a single case.
- **JIT model** -- HolyC compiles and runs immediately. There is no separate compile step and no \`main\` function required.
- **Classes** -- \`class\` replaces C's \`typedef struct\`, with cleaner syntax.

## The Story

Terry Davis began working on TempleOS around 2003 after a series of hospitalizations. He believed God had given him a specification for a "Third Temple" — a personal, direct interface between humans and the divine — implemented as an operating system.

TempleOS is 100,000 lines of HolyC, written entirely by one person. It includes its own compiler, filesystem (\`RedSea\`), 2D/3D graphics, music, and a Bible-verse-generating oracle. The entire system runs in ring 0 — there is no user/kernel separation, no memory protection, no networking. Every part of the system is equally accessible to every program.

Terry Davis died in 2018. TempleOS remains a singular achievement: a complete, working operating system, programming language, and development environment created by one human being.

## The Aiwnios Runtime

This course runs HolyC in your browser using **Aiwnios**, a reimplementation of the TempleOS HolyC compiler that runs on Linux (and, via WebAssembly, in browsers). Aiwnios supports HolyC's core syntax and built-ins, making it possible to learn the language without running TempleOS itself.

## What You Will Learn

This course contains **16 lessons** organized into **6 chapters**:

1. **The Temple** -- Your first HolyC program, printing output, and comments.
2. **Types & Variables** -- Integer types, floating point, booleans, and \`auto\`.
3. **Control Flow** -- \`if\`/\`else\`, switch with ranges, and loops without \`continue\`.
4. **Functions** -- Declaring functions, default arguments, and output parameters.
5. **Classes & Structures** -- Defining classes, member access, and inheritance patterns.
6. **Advanced HolyC** -- Arrays, pointers, and compile-time expressions with \`#exe\`.

Each lesson explains a concept, demonstrates it with code, and gives you an exercise to practice.

Let's get started.
`;

const holycWhatsNextContent = `
## Congratulations

You have completed all 16 lessons. You now understand HolyC's types, control flow, functions, classes, pointers, and compile-time metaprogramming.

That is a real accomplishment. HolyC is an unusual language with a singular history, and you have learned its core ideas.

## What to Explore Next

- **TempleOS itself** -- Download and run TempleOS in a virtual machine. Experience HolyC in its native environment: no mouse, no networking, 640×480 16-color display.
- **Aiwnios** -- Explore the Aiwnios source code on GitHub. It is a complete HolyC compiler and runtime written in C.
- **The TempleOS source** -- Read Terry's original HolyC code. It is available in the TempleOS distribution and on GitHub mirrors.
- **\`#exe\` metaprogramming** -- Explore how \`#exe\` blocks can generate data structures, precompute tables, and embed constants at compile time.
- **TempleOS graphics** -- The original TempleOS has a sprite editor, 3D flight simulator, and other programs written in HolyC.

## Build Something

- **A calculator** -- parse and evaluate arithmetic expressions in HolyC
- **A sorting visualizer** -- implement bubble sort or quicksort and print each step
- **A number theory program** -- primes, factorization, or the Collatz sequence
- **A text adventure** -- simple game logic using HolyC's \`switch\` and functions

## References

- [TempleOS on GitHub](https://github.com/cia-foundation/TempleOS) -- a mirror of the TempleOS source code.
- [Aiwnios on GitHub](https://github.com/Aiwnios/Aiwnios) -- the HolyC compiler used in this course.
- [TempleOS Wikipedia](https://en.wikipedia.org/wiki/TempleOS) -- background on the OS and its creator.
- [The TempleOS Online Documentation](https://templeos.org) -- Terry's original documentation, preserved online.
- [HolyC Overview](https://en.wikipedia.org/wiki/HolyC) -- a concise summary of the language's features.
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
  {
    id: "gleam",
    title: "Gleam",
    description: "Learn the Gleam programming language from scratch. Build type-safe, functional programs with pattern matching, the pipe operator, and the Result type.",
    language: "gleam",
    chapters: gleamChapters,
    lessons: gleamLessons,
    runtimeLabel: "Gleam runtime",
    introductionContent: gleamIntroductionContent,
    whatsNextContent: gleamWhatsNextContent,
  },
  {
    id: "r",
    title: "R",
    description: "Learn the R programming language from scratch. Master vectors, data frames, functions, and data manipulation with R's powerful built-in tools.",
    language: "r",
    chapters: rChapters,
    lessons: rLessons,
    runtimeLabel: "R runtime",
    introductionContent: rIntroductionContent,
    whatsNextContent: rWhatsNextContent,
  },
  {
    id: "holyc",
    title: "HolyC",
    description: "Learn HolyC, the programming language Terry Davis created for TempleOS. Explore range switches, default arguments, compile-time #exe blocks, and a direct relationship with the machine.",
    language: "holyc",
    chapters: holycChapters,
    lessons: holycLessons,
    runtimeLabel: "Aiwnios HolyC",
    introductionContent: holycIntroductionContent,
    whatsNextContent: holycWhatsNextContent,
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
