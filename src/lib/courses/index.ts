import type { Course } from "./types";
import { chapters as goChapters, lessons as goLessons } from "@/lib/lessons";
import { zigChapters, zigLessons } from "@/lib/lessons/zig";
import { sqlChapters, sqlLessons } from "@/lib/lessons/sql";
import { arm64Chapters, arm64Lessons } from "@/lib/lessons/arm64";
import { cChapters, cLessons } from "@/lib/lessons/c";
import { gleamChapters, gleamLessons } from "@/lib/lessons/gleam";
import { rChapters, rLessons } from "@/lib/lessons/r";
import { holycChapters, holycLessons } from "@/lib/lessons/holyc";
import { linuxChapters, linuxLessons } from "@/lib/lessons/linux";
import { coreutilsChapters, coreutilsLessons } from "@/lib/lessons/coreutils";
import { kernelChapters, kernelLessons } from "@/lib/lessons/kernel";
import { jsChapters, jsLessons } from "@/lib/lessons/javascript";
import { tsChapters, tsLessons } from "@/lib/lessons/typescript";
import { algorithmsChapters, algorithmsLessons } from "@/lib/lessons/algorithms";
import { distributedSystemsChapters, distributedSystemsLessons } from "@/lib/lessons/distributed-systems";
import { rubyChapters, rubyLessons } from "@/lib/lessons/ruby";
import { treesChapters, treesLessons } from "@/lib/lessons/trees";
import { threejsChapters, threejsLessons } from "@/lib/lessons/threejs";
import { linkedListsChapters, linkedListsLessons } from "@/lib/lessons/linked-lists";
import { pythonChapters, pythonLessons } from "@/lib/lessons/python";
import { haskellChapters, haskellLessons } from "@/lib/lessons/haskell";
import { graphsChapters, graphsLessons } from "@/lib/lessons/graphs";
import { linearAlgebraChapters, linearAlgebraLessons } from "@/lib/lessons/linear-algebra";
import { diffeqChapters, diffeqLessons } from "@/lib/lessons/diffeq";
import { calculusChapters, calculusLessons } from "@/lib/lessons/calculus";
import { statisticsChapters, statisticsLessons } from "@/lib/lessons/statistics";
import { calculus2Chapters, calculus2Lessons } from "@/lib/lessons/calculus2";
import { calculus3Chapters, calculus3Lessons } from "@/lib/lessons/calculus3";
import { circuitsChapters, circuitsLessons } from "@/lib/lessons/circuits";
import { sqliteChapters, sqliteLessons } from "@/lib/lessons/sqlite";
import { redisChapters, redisLessons } from "@/lib/lessons/redis";
import { cppChapters, cppLessons } from "@/lib/lessons/cpp";
import { musicChapters, musicLessons } from "@/lib/lessons/music";
import { classicalMechanicsChapters, classicalMechanicsLessons } from "@/lib/lessons/classical-mechanics";
import { wavesChapters, wavesLessons } from "@/lib/lessons/waves";
import { quantumChapters, quantumLessons } from "@/lib/lessons/quantum";
import { genomicsChapters, genomicsLessons } from "@/lib/lessons/genomics";
import { microgptChapters, microgptLessons } from "@/lib/lessons/microgpt";
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

const redisIntroductionContent = `
## Why Redis?

Redis is the world's most popular in-memory data structure store. It is used as a cache, message broker, and database. Sub-millisecond response times, hundreds of thousands of operations per second.

- **Blazing fast** — all data lives in RAM; reads and writes in microseconds
- **Versatile** — strings, lists, sets, hashes, sorted sets, streams, and more
- **Persistent** — optional disk persistence; data survives restarts
- **Atomic** — single commands are atomic; no race conditions
- **Pub/Sub** — built-in messaging between clients
- **TTL** — keys expire automatically; perfect for sessions and caches

## The Story

Redis was created by Salvatore Sanfilippo (antirez) in 2009 while working on a startup in Italy. He was frustrated with MySQL's performance for a real-time web analytics tool. He wrote the first version of Redis overnight and open-sourced it.

Redis stands for **Re**mote **Di**ctionary **S**erver. Redis Labs (now Redis Inc.) funds its development. Redis 7.0 introduced radical improvements to performance and memory efficiency.

## Who Uses Redis

Redis is used in production by virtually every major technology company:

- **Twitter** — caches timelines and user data
- **GitHub** — queues background jobs
- **Stack Overflow** — caches question and answer data
- **Pinterest** — stores social graphs
- **Snapchat** — stores message metadata
- **Uber** — geospatial real-time driver tracking

## What You Will Learn

This course contains **15 lessons** organized into **5 chapters**:

1. **Strings** — SET/GET, string operations, TTL/expiry, atomic counters.
2. **Collections** — Lists, Sets, Hashes, and Sorted Sets.
3. **Key Operations** — KEYS, TYPE, RENAME, and cursor-based SCAN.
4. **Patterns** — Transactions, caching patterns, and leaderboards.
5. **Advanced** — HyperLogLog and data modeling.

Each lesson explains a concept with examples and gives you an exercise to practice against a live Redis emulator in your browser.

Let's get started.
`;

const redisWhatsNextContent = `
## Congratulations

You have completed all 15 lessons. You now understand Redis's core data structures, key management, transactions, caching patterns, and HyperLogLog.

## What to Explore Next

- **Pub/Sub** — SUBSCRIBE/PUBLISH for real-time messaging between clients.
- **Streams** — XADD/XREAD for durable, consumer-group message logs (Kafka-like).
- **Lua Scripting** — EVAL for complex atomic server-side operations.
- **Redis Modules** — RedisSearch (full-text), RedisJSON, RedisGraph, RedisTimeSeries.
- **Redis Sentinel** — automatic failover and high availability.
- **Redis Cluster** — horizontal scaling across multiple nodes.

## Use Redis in Your Projects

- **Node.js** — \`ioredis\` or the official \`@redis/client\`
- **Python** — \`redis-py\` (\`pip install redis\`)
- **Go** — \`github.com/redis/go-redis\`
- **Bun** — use \`ioredis\` or \`@redis/client\`

## References

- [Redis Documentation](https://redis.io/docs/) — comprehensive command reference.
- [Redis University](https://university.redis.com/) — free courses from Redis Inc.
- [Try Redis](https://try.redis.io/) — interactive Redis in the browser.
`;

const sqliteIntroductionContent = `
## Why SQLite?

SQLite is the most widely deployed database engine in the world. Unlike PostgreSQL or MySQL, SQLite is a library embedded directly in your application — no server, no configuration, no installation.

- **Everywhere** -- iOS, Android, Firefox, Python stdlib, Bun, every major browser. An estimated one trillion SQLite databases are in active use.
- **Zero setup** -- A single \`.db\` file. Open it, query it, done.
- **Standard SQL** -- Most SQL you learn here — SELECT, JOIN, GROUP BY, window functions, CTEs — works identically in PostgreSQL and MySQL.
- **Serverless** -- Perfect for local apps, mobile, embedded devices, testing, and prototyping.
- **Fast** -- For read-heavy workloads on a single machine, SQLite often outperforms client-server databases.

## The Story

SQLite was created by D. Richard Hipp in 2000, originally for the US Navy to manage guided missile destroyer software. Hipp designed it to work without a server — the entire database is a single file that the application reads and writes directly.

In 2000, a database without a server was an unusual idea. Today it is a design pattern that powers billions of applications. SQLite is public domain — no license, no restrictions.

## What You Will Learn

This course contains **15 lessons** organized into **5 chapters**:

1. **Getting Started** -- SELECT, WHERE, ORDER BY, LIMIT, and SQLite's flexible type system.
2. **Schema** -- CREATE TABLE, INTEGER PRIMARY KEY, constraints, and ALTER TABLE.
3. **CRUD Operations** -- INSERT, UPDATE, DELETE, and conflict resolution.
4. **Querying** -- JOINs, aggregations, GROUP BY, and Common Table Expressions.
5. **SQLite Features** -- Window functions and JSON functions built into SQLite.

Each lesson explains a concept with examples and gives you an exercise to practice against a live SQLite database in your browser.

Let's get started.
`;

const sqliteWhatsNextContent = `
## Congratulations

You have completed all 15 lessons. You now understand SQLite's fundamentals: querying, filtering, sorting, schema management, CRUD operations, joins, aggregations, CTEs, window functions, and JSON.

## What to Explore Next

- **Full-text search** -- SQLite's FTS5 extension for fast text search.
- **Virtual tables** -- Custom table implementations (CSV files, R-tree spatial indexes).
- **WAL mode** -- Write-Ahead Logging for better concurrent read performance.
- **Transactions** -- BEGIN/COMMIT/ROLLBACK and SAVEPOINT for atomicity.
- **Indexes** -- CREATE INDEX, covering indexes, and EXPLAIN QUERY PLAN.

## Use SQLite in Your Projects

- **Bun** -- \`import { Database } from "bun:sqlite"\` — the fastest SQLite API in any runtime.
- **Python** -- \`import sqlite3\` — in the standard library, zero install.
- **Node.js** -- \`better-sqlite3\` or \`@libsql/client\` for async remote SQLite (Turso).
- **Go** -- \`github.com/mattn/go-sqlite3\` or the pure-Go \`modernc.org/sqlite\`.

## References

- [SQLite Official Documentation](https://www.sqlite.org/docs.html) — thorough and well-organized.
- [SQLite Tutorial](https://www.sqlitetutorial.net/) — practical examples.
- [Bun SQLite docs](https://bun.sh/docs/api/sqlite) — fastest SQLite API in JS/TS.
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

const cppIntroductionContent = `
## Why C++?

C++ is one of the most powerful and widely deployed programming languages in history. Bjarne Stroustrup created it at Bell Labs starting in 1979 as "C with Classes," and the name \`C++\` (the increment operator applied to C) came in 1983.

- **Systems-level power** -- C++ gives you manual memory control, zero-overhead abstractions, and direct hardware access — the same capabilities as C.
- **Object-oriented** -- Classes, inheritance, and polymorphism let you model complex systems with clear structure.
- **Generic programming** -- Templates let you write code once that works for any type, powering the entire Standard Library.
- **The Standard Library** -- \`vector\`, \`string\`, \`map\`, \`algorithm\`, \`thread\` — a comprehensive toolkit built in.
- **Zero overhead** -- Abstractions in C++ cost nothing at runtime. If you don't use it, you don't pay for it.

## The Story

C++ grew from the frustration that C, while powerful, lacked the tools to manage the complexity of large software projects. Stroustrup added classes (from Simula), virtual functions, operator overloading, and eventually templates — turning C into a multi-paradigm language.

Major standardization milestones:
- **C++98** — first standard, templates, STL
- **C++11** — auto, lambda, range-based for, move semantics, smart pointers
- **C++14/17** — refinements and new library features
- **C++20** — concepts, ranges, coroutines, modules

## Who Uses C++

C++ powers some of the most demanding software in the world:

- **Game engines** — Unreal Engine, id Tech, Frostbite are written in C++
- **Browsers** — Chrome (V8, Blink), Firefox (Gecko) use C++ for performance-critical paths
- **Databases** — MySQL, MongoDB, ClickHouse are C++
- **Scientific computing** — LLVM, TensorFlow, OpenCV
- **Finance** — High-frequency trading systems where microseconds matter

## What You Will Learn

This course contains **15 lessons** organized into **6 chapters**:

1. **C++ Basics** -- Hello C++, variables and types, string operations.
2. **Control Flow** -- Conditionals and loops including the range-based for.
3. **Functions** -- Overloading, default arguments, and references.
4. **Classes** -- Classes, constructors, and encapsulation.
5. **Inheritance** -- Inheritance and virtual functions (polymorphism).
6. **Templates & STL** -- Function templates and \`std::vector\`.

Let's get started.
`;

const cppWhatsNextContent = `
## Congratulations

You have completed all 15 lessons. You now understand C++'s core features: output with cout, variables, strings, control flow, function overloading, default arguments, references, classes, constructors, encapsulation, inheritance, virtual functions, function templates, and vectors.

That is a strong foundation. C++ is a vast language, but these concepts unlock everything else.

## What to Explore Next

- **Smart pointers** -- \`unique_ptr\`, \`shared_ptr\`, and \`weak_ptr\` replace raw \`new\`/\`delete\`.
- **Lambda expressions** -- anonymous functions for callbacks and algorithms: \`[](int x) { return x * 2; }\`
- **The STL algorithms** -- \`std::sort\`, \`std::find\`, \`std::transform\`, \`std::accumulate\` work on any container.
- **Move semantics** -- \`std::move\`, rvalue references, and move constructors for zero-copy transfers.
- **Exceptions** -- \`try\`, \`catch\`, \`throw\` for error handling.
- **Class templates** -- Generic classes like \`vector<T>\`, \`map<K,V>\`, and your own.
- **Operator overloading** -- Make your classes work with \`+\`, \`==\`, \`<<\`, and other operators.

## Build Something

- **A stack or queue** -- implement using \`vector\` or a linked list
- **A matrix class** -- with addition, multiplication, and transpose
- **A simple expression parser** -- tokenize and evaluate arithmetic strings
- **A polymorphic shape library** -- area, perimeter, and drawing for many shape types

## References

- [cppreference.com](https://en.cppreference.com/w/cpp) -- the definitive C++ reference.
- *A Tour of C++* by Bjarne Stroustrup -- concise, authoritative overview of modern C++.
- *Effective Modern C++* by Scott Meyers -- essential patterns for C++11/14.
- [LearnCpp.com](https://www.learncpp.com/) -- free, thorough tutorial for beginners.
- [C++ Core Guidelines](https://isocpp.github.io/CppCoreGuidelines/CppCoreGuidelines) -- best practices maintained by Stroustrup and Sutter.
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

const coreutilsIntroductionContent = `
## Build the Tools You Use Every Day

Every command you ran in the Linux course — \`cat\`, \`grep\`, \`wc\`, \`head\`, \`tail\`, \`tr\`, \`uniq\` — is a C program. These programs are part of the **GNU coreutils** package, and they have been powering Unix systems for decades.

This course asks one question: **can you build them yourself?**

Each lesson gives you a coreutils command and asks you to implement its core logic as a C function. Not the full command with every flag and edge case — just the essential algorithm.

- \`cat\` → print a string character by character
- \`wc -l\` → count newlines
- \`grep\` → find lines containing a substring
- \`tail\` → print the last N lines
- \`tac\` → print lines in reverse order

Simple descriptions. Interesting implementations.

## Why This Matters

Writing these tools from scratch forces you to think about problems that are usually hidden:

- **How does \`wc -w\` know where one word ends and another begins?** A state machine.
- **How does \`tail\` know where the last N lines start without reading the file twice?** Scanning backwards.
- **How does \`grep\` check every position in a line?** A nested loop over all starting positions.

These are real algorithms running on millions of machines every day. You will understand them after implementing them.

## The Connection

This course is designed to follow the **Linux** and **C** courses on this platform:

- The Linux course taught you to *use* these commands from the shell.
- The C course taught you the language they are written in.
- This course connects both: rewrite the commands in C.

## What You Will Learn

This course contains **13 lessons** organized into **4 chapters**:

1. **Output** -- \`echo\`, \`cat\`, \`rev\`. Print strings, character by character, and reversed.
2. **Counting** -- \`wc -c\`, \`wc -l\`, \`wc -w\`. Count characters, lines, and words using pointer arithmetic and state machines.
3. **Filtering** -- \`head\`, \`tail\`, \`grep\`. Select lines from the start, end, or by pattern.
4. **Transformation** -- \`toupper\`, \`tr\`, \`uniq\`, \`tac\`. Modify and reorder text.

Each lesson explains the command, walks through the algorithm, and gives you a C function to implement and test.

Let's build coreutils.
`;

const coreutilsWhatsNextContent = `
## Congratulations

You have reimplemented 13 coreutils in C: \`echo\`, \`cat\`, \`rev\`, \`wc -c/l/w\`, \`head\`, \`tail\`, \`grep\`, \`toupper\`, \`tr\`, \`uniq\`, and \`tac\`.

That is real systems programming. These are the same algorithms that run on every Linux server in the world.

## What to Explore Next

You have built the core. Here are ways to go deeper:

- **Add the missing flags** -- \`head -n N\` with \`atoi\` to parse the argument; \`grep -i\` for case-insensitive matching; \`uniq -c\` to count occurrences.
- **Read from stdin** -- Real coreutils read from \`stdin\` (or files given as arguments). Add \`fgets\` or \`getchar\` loops to handle real input.
- **Handle multiple files** -- \`cat file1 file2\` opens and concatenates multiple files using \`fopen\`, \`fread\`, \`fclose\`.
- **Add error handling** -- What happens when a file does not exist? \`fprintf(stderr, ...)\` and non-zero exit codes.
- **Implement sort** -- Sorting lines requires either dynamic memory (\`malloc\`) or a fixed buffer. A classic exercise.

## Build Something Real

The ultimate exercise: **write a complete coreutil that works on your system**.

\`\`\`c
// mycat.c
#include <stdio.h>

int main(int argc, char *argv[]) {
    if (argc < 2) {
        // read from stdin
        int c;
        while ((c = getchar()) != EOF) putchar(c);
        return 0;
    }
    for (int i = 1; i < argc; i++) {
        FILE *f = fopen(argv[i], "r");
        if (!f) { fprintf(stderr, "mycat: %s: No such file\\n", argv[i]); return 1; }
        int c;
        while ((c = fgetc(f)) != EOF) putchar(c);
        fclose(f);
    }
    return 0;
}
\`\`\`

Compile with \`gcc mycat.c -o mycat\` and test it on real files. This is software you actually built, running on real hardware.

## References

- [GNU coreutils source code](https://github.com/coreutils/coreutils) -- Read the real implementations. \`src/cat.c\` is ~700 lines. \`src/wc.c\` handles UTF-8. See how far your implementations got.
- [The C Programming Language](https://www.cs.princeton.edu/~bwk/cbook.html) by Kernighan & Ritchie -- The original book. Chapter 7 on I/O is directly relevant.
- [Beej's Guide to C](https://beej.us/guide/bgc/) -- Free, comprehensive, practical.
- \`man 1 coreutils\` -- On any Linux system, \`man cat\`, \`man grep\`, etc. document every flag.
`;

const kernelIntroductionContent = `
## How Linux Works Under the Hood

You have used Linux commands. You have written C programs. Now go one level deeper: **how does the Linux kernel itself work?**

This course teaches Linux internals through C simulations. Every lesson implements a real kernel data structure or algorithm — not toy code, but the actual patterns from \`kernel/sched.c\`, \`mm/slab.c\`, \`fs/inode.c\`, and \`kernel/signal.c\`.

Since we run in the browser, we simulate rather than invoke real syscalls. But the data structures, algorithms, and logic are faithful to the real kernel.

## What the Kernel Does

The Linux kernel is the software layer between your programs and the hardware. Its four main jobs:

- **Process management** — create, schedule, and terminate processes
- **Memory management** — map virtual addresses to physical RAM, handle page faults
- **File system** — abstract disks into files and directories
- **IPC and signals** — let processes communicate and coordinate

Each chapter of this course covers one of these subsystems.

## What You Will Learn

This course contains **15 lessons** organized into **5 chapters**:

1. **Processes** — The Process Control Block (\`task_struct\`), state transitions, \`fork()\`, and round-robin scheduling.
2. **Memory** — Virtual address layout, page table walks, and the slab allocator.
3. **File System** — Inodes, file descriptor tables, and the File Allocation Table (FAT).
4. **IPC** — Ring buffers (the kernel pipe implementation), semaphores, and signal dispatch.
5. **System Calls** — The syscall dispatch table and \`sys_write\`.

Each lesson explains the concept, shows where it lives in the real kernel source, and gives you a C function to implement and test.

Let's read the kernel.
`;

const kernelWhatsNextContent = `
## Congratulations

You have implemented the core data structures of a real operating system:

- **Processes**: PCB, state machine, fork, round-robin scheduler
- **Memory**: virtual address parsing, page tables, slab allocator
- **File System**: inodes, file descriptor tables, FAT chains
- **IPC**: ring buffers, semaphores, signal dispatch tables
- **Syscalls**: syscall table, sys_write

These are not abstractions — they are the actual patterns used in \`linux/sched.h\`, \`mm/slab.c\`, \`fs/inode.h\`, and \`kernel/signal.c\`.

## Go Deeper

- **Read the kernel source** — [elixir.bootlin.com](https://elixir.bootlin.com/linux/latest/source) lets you browse the Linux source online with cross-references. Start with \`include/linux/sched.h\` (task_struct) and \`mm/slab.c\`.
- **Write a kernel module** — A kernel module is a C file you compile and load into a running kernel. The classic first module: a character device that returns "hello" when read.
- **Explore xv6** — MIT's teaching operating system, written in C for RISC-V. It implements everything you learned here in ~8,000 lines: [github.com/mit-pdos/xv6-riscv](https://github.com/mit-pdos/xv6-riscv).
- **Read Operating Systems: Three Easy Pieces** — Free online OS textbook covering virtualization, concurrency, and persistence in depth: [ostep.org](https://ostep.org).
- **Try the Linux kernel labs** — [linux-kernel-labs.github.io](https://linux-kernel-labs.github.io) has hands-on exercises for kernel development.

## References

- *Linux Kernel Development* by Robert Love — The best single-volume guide to the kernel internals.
- *Understanding the Linux Kernel* by Bovet & Cesati — Deep dive into kernel mechanisms.
- [The Linux Kernel documentation](https://www.kernel.org/doc/html/latest/) — Official docs, including subsystem-specific guides.
`;

const linuxIntroductionContent = `
## Why Linux?

Linux is the operating system that runs the world. It powers web servers, cloud infrastructure, Android phones, supercomputers, and embedded systems. If you write software professionally, you will work with Linux every day.

- **Free and open source** -- The kernel source code is public, auditable, and modifiable. You own your system.
- **Dominant in servers** -- Over 96% of the world's web servers run Linux. AWS, Google Cloud, and Azure are built on Linux.
- **The developer's OS** -- Package managers, shell scripting, SSH, containers, and deployment pipelines all assume Linux fluency.
- **Stable and secure** -- Linux powers critical infrastructure because it is reliable, configurable, and has a strong security model.

## The Story

Linus Torvalds was a 21-year-old Finnish student in 1991 when he announced on a Usenet newsgroup: "I'm doing a (free) operating system (just a hobby, won't be big and professional like GNU)." That project became the Linux kernel — today one of the largest collaborative software projects in history, with contributions from thousands of developers and companies including Google, Intel, Red Hat, and Microsoft.

The GNU Project, started by Richard Stallman in 1983, had already created most of the user-space tools (shell, compiler, coreutils). Combined with Torvalds' kernel, the result was a complete free operating system: GNU/Linux.

## The Shell

At the heart of Linux is the **shell** — a text interface where you type commands. The default shell on most systems is **Bash** (Bourne Again SHell). The shell is not just a way to run programs; it is a full scripting language for automating tasks.

This course runs your shell commands in an in-browser Linux simulator. You get a real shell experience — a virtual filesystem, common commands, pipes, variables, and scripting — without needing to install anything.

## What You Will Learn

This course contains **17 lessons** organized into **4 chapters**:

1. **The Shell** -- \`echo\`, \`pwd\`, \`ls\`, \`cd\`, and \`cat\`. The fundamental commands for navigating and reading files.
2. **Working with Files** -- \`mkdir\`, \`touch\`, \`cp\`, \`mv\`, and \`rm\`. Creating, copying, moving, and deleting files and directories.
3. **Text Processing** -- \`head\`, \`grep\`, pipes, and \`wc\`. Reading parts of files, searching, and combining commands with the pipe operator.
4. **Shell Scripting** -- Variables, \`for\` loops, and \`if\`/\`else\` conditionals. Writing scripts that automate work.

Each lesson explains a concept, demonstrates it with examples, and gives you an exercise to practice in the browser.

Let's get started.
`;

const linuxWhatsNextContent = `
## Congratulations

You have completed all 17 lessons. You now know how to navigate the filesystem, manage files, process text, and write shell scripts with variables, loops, and conditionals.

That is a real foundation. Linux fluency opens up a world of capabilities: automating repetitive tasks, managing servers, building deployment pipelines, and understanding how software systems work.

## What to Explore Next

Here are topics to dive deeper into:

- **Redirections** -- \`>\` and \`>>\` write stdout to files; \`<\` reads stdin from files; \`2>\` redirects stderr.
- **File permissions** -- \`chmod\`, \`chown\`, and understanding \`rwxr-xr-x\` in detail.
- **Processes** -- \`ps\`, \`top\`, \`kill\`, background jobs with \`&\`, and \`fg\`/\`bg\`.
- **Regular expressions** -- \`grep -E\`, \`sed\`, and \`awk\` for powerful text transformation.
- **SSH** -- Connecting to remote machines securely.
- **cron** -- Scheduling scripts to run at specific times.
- **Package management** -- \`apt\`, \`yum\`, or \`pacman\` depending on your distribution.
- **Environment variables** -- \`export\`, \`.bashrc\`, and \`.profile\`.

## Build Something

The best way to solidify Linux skills is to use them:

- **A backup script** -- automatically copy important files to a backup directory with timestamps
- **A log analyzer** -- parse a log file with \`grep\`, \`awk\`, and \`sort\` to find the most common errors
- **A project scaffolder** -- a script that creates a standard directory structure for new projects
- **A system monitor** -- display disk usage, memory, and CPU load in a readable format

## References

- [The Linux Command Line](https://linuxcommand.org/tlcl.php) by William Shotts -- free online book, comprehensive and well-written.
- [Bash Reference Manual](https://www.gnu.org/software/bash/manual/bash.html) -- the official Bash documentation.
- [TLDR Pages](https://tldr.sh/) -- community-maintained simplified man pages with practical examples.
- [Explain Shell](https://explainshell.com/) -- paste any shell command and get a visual breakdown of what each part does.
- [The Art of Command Line](https://github.com/jlevy/the-art-of-command-line) -- a curated guide of command-line tips and tricks.
- [Linux Journey](https://linuxjourney.com/) -- interactive Linux learning with exercises.
`;

const jsIntroductionContent = `
## Why JavaScript?

JavaScript is the language of the web. It runs in every browser, powers millions of websites and apps, and is consistently one of the most popular programming languages in the world.

- **Everywhere** -- Every browser runs JavaScript natively. No install, no compile step, no runtime to set up. Open the console and start coding.
- **Full-stack** -- With Node.js, JavaScript runs on servers too. One language for frontend, backend, scripting, and tooling.
- **Huge ecosystem** -- npm hosts over 2 million packages. Whatever you need to build, there is probably a library for it.
- **Fast enough** -- Modern JavaScript engines (V8, SpiderMonkey) use JIT compilation. JavaScript is fast enough for games, real-time apps, and data processing.
- **Expressive** -- First-class functions, closures, destructuring, and the rest of the modern ES6+ syntax make JavaScript surprisingly elegant.

## The Story

JavaScript was created by Brendan Eich in just 10 days in May 1995 while working at Netscape. Originally called Mocha, then LiveScript, it was renamed JavaScript as a marketing move to capitalize on Java's popularity — despite having almost nothing to do with Java.

The language was standardized in 1997 as ECMAScript (ES1). For years, browser incompatibilities made JavaScript painful to write. The release of ES6 (ES2015) transformed the language: \`let\`, \`const\`, arrow functions, classes, modules, Promises, and much more arrived all at once.

Today, JavaScript evolves yearly, with new features added through TC39, the standards committee. Engines like Google's V8 (used in Chrome and Node.js) make it one of the fastest dynamic languages in existence.

## Who Uses JavaScript

Practically everyone in web development:

- **React, Vue, Angular** -- the dominant frontend frameworks are all JavaScript.
- **Node.js** -- JavaScript on the server, powering Netflix, LinkedIn, and Uber.
- **Electron** -- VS Code, Slack, and Figma are built with JavaScript.
- **React Native** -- mobile apps from a single JavaScript codebase.

## What You Will Learn

This course contains **15 lessons** organized into **5 chapters**:

1. **Basics** -- \`console.log\`, variables with \`let\` and \`const\`, and string operations.
2. **Control Flow** -- Conditionals with \`if\`/\`else\`, \`for\` loops, and \`while\` loops.
3. **Functions** -- Declaring functions, arrow functions, and closures.
4. **Arrays** -- Creating and modifying arrays, then \`map\`, \`filter\`, and \`reduce\`.
5. **Objects** -- Key-value pairs, property access, and destructuring.

Each lesson explains a concept, shows examples, and gives you an exercise to write and run in your browser.

Let's get started.
`;

const jsWhatsNextContent = `
## Congratulations

You have completed all 15 lessons. You now know JavaScript's core language: variables, types, control flow, functions, closures, arrays, higher-order functions, objects, and destructuring.

That is a real foundation. You can read and write JavaScript, understand most code you encounter, and start building your own projects.

## What to Explore Next

Here are the natural next steps:

- **The DOM** -- \`document.querySelector\`, event listeners, and updating the page dynamically. This is how JavaScript makes web pages interactive.
- **Fetch API** -- Make HTTP requests with \`fetch()\` to load data from APIs. Return JSON and display it on a page.
- **Promises and async/await** -- Handle asynchronous operations cleanly. Essential for working with APIs and databases.
- **ES6+ Features** -- Spread (\`...\`), rest parameters, optional chaining (\`?.\`), nullish coalescing (\`??\`), and template literals.
- **Modules** -- \`import\` and \`export\` to organize code across files.
- **TypeScript** -- Add static types to JavaScript for better tooling and fewer bugs at scale.

## Build Something

The best way to learn is to build. Some project ideas:

- **A todo app** -- add, complete, and delete tasks. Store them in localStorage.
- **A weather app** -- fetch data from a public weather API and display it.
- **A quiz game** -- questions and answers, score tracking, and a timer.
- **A markdown previewer** -- type markdown on the left, see HTML on the right.

## References

- [MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript) -- the definitive JavaScript reference. Every built-in function, every API, with examples.
- [javascript.info](https://javascript.info/) -- the best free JavaScript tutorial. Comprehensive, modern, well-explained.
- [Eloquent JavaScript](https://eloquentjavascript.net/) by Marijn Haverbeke -- a free book that goes deep into the language.
- [You Don't Know JS](https://github.com/getify/You-Dont-Know-JS) by Kyle Simpson -- six books explaining JavaScript's quirks and internals in depth.
- [The Modern JavaScript Tutorial](https://javascript.info/) -- practical, modern, and thorough.
`;

const tsIntroductionContent = `
## Why TypeScript?

TypeScript is JavaScript with static types. It is a superset of JavaScript — any valid JavaScript is valid TypeScript. The type system catches errors at compile time, before your code ever runs.

- **Catch bugs early** -- Type errors are caught when you write code, not when users hit them. A mistyped property name, a wrong argument type, or a missing return value are all caught before deployment.
- **Better tooling** -- TypeScript enables powerful autocomplete, inline documentation, and safe refactoring in any editor. The type system knows what properties an object has.
- **Scales with your codebase** -- As projects grow, types make code easier to understand and modify safely. Large teams rely on TypeScript to work on shared codebases without stepping on each other.
- **Gradual adoption** -- You can add TypeScript to an existing JavaScript project one file at a time. The \`strict: false\` option lets you start loose and tighten up incrementally.
- **The industry standard** -- React, Angular, Vue, Node.js, and virtually every major JavaScript framework are written in or have first-class TypeScript support.

## The Story

TypeScript was created at Microsoft by Anders Hejlsberg — the original designer of C# and Delphi. It was announced in October 2012 after being developed internally for two years.

Microsoft's motivation was practical: they were building large applications in JavaScript and finding it increasingly hard to maintain at scale. TypeScript was their answer — a typed layer that compiles away entirely, leaving standard JavaScript.

The project went open source from day one on GitHub. It gained widespread adoption in the JavaScript ecosystem, especially after Angular 2 (2016) made TypeScript its primary language. Today, TypeScript is consistently one of the most loved and most used programming languages.

## Who Uses TypeScript

The JavaScript ecosystem has largely adopted TypeScript:

- **Microsoft** -- VS Code, Teams, and Office Online are built with TypeScript.
- **Google** -- Angular is TypeScript-first.
- **Airbnb** -- migrated their entire frontend to TypeScript.
- **Slack, Asana, Figma, Stripe** -- all use TypeScript in production.

The DefinitelyTyped repository hosts type definitions for over 8,000 JavaScript libraries, making TypeScript work with the entire npm ecosystem.

## What You Will Learn

This course contains **15 lessons** organized into **6 chapters**:

1. **Basics** -- Type annotations for variables, arrays, and function parameters and return types.
2. **Interfaces** -- Defining object shapes with interfaces, type aliases, and optional properties.
3. **Type System** -- Union types, type narrowing with \`typeof\`, and literal types.
4. **Generics** -- Generic functions and generic constraints with \`extends\` and \`keyof\`.
5. **Classes** -- TypeScript classes with access modifiers, and enums.
6. **Advanced** -- Readonly properties and type guard functions.

Each lesson explains a concept, demonstrates it with code examples, and gives you an exercise to practice.

Let's get started.
`;

const tsWhatsNextContent = `
## Congratulations

You have completed all 15 lessons. You now have a solid foundation in TypeScript: type annotations, interfaces, union types, type narrowing, generics, classes, enums, readonly, and type guards.

That is a real accomplishment. You understand the core type system features that make TypeScript valuable at scale.

## What to Explore Next

Here are topics to dive deeper into:

- **Utility Types** -- \`Partial<T>\`, \`Required<T>\`, \`Pick<T, K>\`, \`Omit<T, K>\`, \`Record<K, V>\`, \`ReturnType<F>\` — TypeScript's built-in type transformations.
- **Mapped Types** -- Transform every property of a type: \`{ [K in keyof T]: ... }\`.
- **Conditional Types** -- \`T extends U ? X : Y\` for type-level branching.
- **Template Literal Types** -- String manipulation at the type level: \`\\\`get\${Capitalize<string>}\\\`\`.
- **Declaration Files** -- Write \`.d.ts\` files to add types to JavaScript libraries.
- **Strict Mode** -- Enable \`strict: true\` for stricter null checking and better safety.
- **Decorators** -- Metadata annotations used heavily in Angular and NestJS.

## Build Something

The best way to learn is to build. Some project ideas:

- **A typed REST API client** -- define types for requests and responses, with generics for different endpoints
- **A state machine** -- use discriminated unions to model application states safely
- **A type-safe event emitter** -- generics and mapped types to enforce correct event handler signatures
- **Migrate a JavaScript project** -- take an existing JS project and add TypeScript types incrementally

## References

- [TypeScript Handbook](https://www.typescriptlang.org/docs/handbook/intro.html) -- the official guide, comprehensive and well-written.
- [TypeScript Playground](https://www.typescriptlang.org/play) -- write and run TypeScript in the browser, see the compiled JavaScript.
- [Type Challenges](https://github.com/type-challenges/type-challenges) -- a collection of type system puzzles to sharpen your skills.
- [Total TypeScript](https://www.totaltypescript.com/) by Matt Pocock -- advanced TypeScript patterns explained clearly.
- [Effective TypeScript](https://effectivetypescript.com/) by Dan Vanderkam -- 62 specific ways to improve your TypeScript.
- *Programming TypeScript* by Boris Cherny (O'Reilly, 2019) -- a comprehensive book on TypeScript for JavaScript developers.
`;

const algorithmsIntroductionContent = `
## Why Study Algorithms?

Algorithms are the foundation of all software. Every program you write — whether it sorts a list, searches a database, or finds a route on a map — relies on well-understood algorithms. Learning them makes you a better programmer at every level.

- **Speed matters** -- A naive O(n²) sort on 1 million elements takes seconds. An O(n log n) sort takes milliseconds. The algorithm is the difference.
- **Interview ready** -- Algorithms are the core of technical interviews at every top tech company.
- **Problem solving** -- Algorithm design teaches systematic thinking: how to decompose problems, identify patterns, and reason about correctness.
- **Universal knowledge** -- These algorithms run on every language, every platform, every architecture.

## The Canon

The algorithms in this course are not academic curiosities. They are the ones that power real software:

- **Merge sort** is used in Python's \`sorted()\` and Java's \`Arrays.sort()\`.
- **Quick sort** underlies C's \`qsort\` and C++'s \`std::sort\`.
- **BFS** finds shortest paths in Google Maps and social network friend suggestions.
- **Dijkstra's algorithm** powers GPS navigation and network routing protocols.
- **Dynamic programming** is used in sequence alignment (bioinformatics), spell correction, and compiler optimization.

## What You Will Learn

This course contains **15 lessons** organized into **5 chapters**:

1. **Sorting** -- Bubble sort, selection sort, insertion sort, merge sort, and quick sort. Understand the trade-offs between O(n²) and O(n log n) algorithms.
2. **Searching** -- Linear search for unsorted data, binary search for sorted data. The power of O(log n).
3. **Data Structures** -- Stack (LIFO), Queue (FIFO), and Linked List. The building blocks for more complex algorithms.
4. **Graphs** -- Breadth-first search (BFS), depth-first search (DFS), and Dijkstra's shortest path algorithm.
5. **Dynamic Programming** -- Memoization, the Fibonacci sequence, and the Longest Common Subsequence problem.

Each lesson explains the algorithm, shows the code, and gives you an exercise to implement it.

Let's get started.
`;

const algorithmsWhatsNextContent = `
## Congratulations

You have completed all 15 lessons. You now understand sorting algorithms, binary search, stacks, queues, linked lists, graph traversal, and dynamic programming.

That is a real accomplishment. These algorithms appear in virtually every technical interview and underlie most software systems.

## What to Explore Next

You have covered the fundamentals. Here is where to go deeper:

- **Heaps and Priority Queues** -- Efficient O(log n) min/max operations. The foundation for Dijkstra with a priority queue and heap sort.
- **Hash Tables** -- O(1) average lookup, insertion, and deletion. The most used data structure in practice.
- **Binary Search Trees** -- O(log n) search, insert, delete for dynamic sorted data.
- **Balanced BSTs (AVL, Red-Black)** -- Guaranteed O(log n) even with adversarial inputs.
- **Tries** -- Efficient prefix searches for autocomplete and spell checking.
- **Union-Find** -- Efficient connected components and Kruskal's minimum spanning tree.
- **Topological Sort** -- Ordering dependencies in build systems and package managers.
- **Bellman-Ford** -- Shortest paths with negative weights.

## Practice

Theory is not enough — you need to practice on real problems:

- [LeetCode](https://leetcode.com/) -- the most popular competitive programming platform. Start with "Easy" problems.
- [NeetCode](https://neetcode.io/) -- curated problem lists with video explanations, organized by pattern.
- [AlgoExpert](https://www.algoexpert.io/) -- 160 handpicked questions with video solutions.
- [Project Euler](https://projecteuler.net/) -- mathematical and computational problems that reward algorithmic thinking.

## References

- *Introduction to Algorithms* (CLRS) by Cormen, Leiserson, Rivest, and Stein -- the definitive textbook. Dense but comprehensive.
- *The Algorithm Design Manual* by Steven Skiena -- practical and readable. Excellent war stories from real-world algorithm use.
- *Grokking Algorithms* by Aditya Bhargava -- illustrated introduction, great for visual learners.
- [Visualgo](https://visualgo.net/) -- visual animations of sorting, graph, and DP algorithms.
- [Big-O Cheat Sheet](https://www.bigocheatsheet.com/) -- quick reference for time and space complexity.
`;

const distributedSystemsIntroductionContent = `
## Why Distributed Systems?

When a single machine is not enough — too slow, too small, or too unreliable — you distribute the workload across multiple machines. This is how every major internet service operates: Google, Amazon, Netflix, and WhatsApp all run on distributed systems.

- **Scale** -- distribute load across hundreds or thousands of machines. No single machine can serve a billion users.
- **Fault tolerance** -- when one machine fails (and it will), the system keeps running. Replication and consensus protocols keep data safe.
- **Latency** -- serve users from servers close to them. Data replicated globally means fast reads everywhere.

But distribution introduces hard problems: machines fail, networks partition, clocks drift, and messages are lost or delayed. Every distributed system must make trade-offs between consistency, availability, and partition tolerance — the famous **CAP theorem**.

## What You Will Build

In this course, you will implement the core algorithms and data structures that power real distributed systems — in JavaScript:

- **Lamport clocks and vector clocks** — how to order events when there is no global clock.
- **Consistent hashing** — how DynamoDB and Cassandra distribute data across nodes without reshuffling everything when a node is added or removed.
- **Leader election** — how Kafka and Kubernetes elect a coordinator.
- **Quorum-based replication** — how to guarantee strong consistency with N replicas.
- **CRDTs** — conflict-free data structures that merge automatically in eventually consistent systems.
- **Bloom filters** — probabilistic membership tests used in Cassandra, Chrome, and Bitcoin.
- **Rate limiting** — the token bucket algorithm used by every production API.
- **Circuit breakers** — the pattern that prevents cascading failures in microservices.
- **LRU caches** — the eviction policy behind CPU caches, Redis, and database buffer pools.
- **Gossip protocols** — how Cassandra and DynamoDB propagate cluster membership.
- **Two-phase commit** — the atomic protocol for distributed transactions.

## What You Will Learn

This course contains **12 lessons** organized into **6 chapters**:

1. **Clocks & Ordering** -- Lamport clocks and vector clocks for logical time.
2. **Data Distribution** -- Consistent hashing for distributing data across nodes.
3. **Consensus** -- Leader election and two-phase commit for coordination.
4. **Replication** -- Quorum-based operations, G-Counter CRDTs, and gossip protocols.
5. **Probabilistic Structures** -- Bloom filters for space-efficient membership testing.
6. **Fault Tolerance** -- Rate limiting, circuit breakers, and LRU caching.

Each lesson explains the concept, shows the algorithm, and gives you an exercise to implement it in JavaScript.

Let's get started.
`;

const distributedSystemsWhatsNextContent = `
## Congratulations

You have completed all 12 lessons. You now understand Lamport clocks, vector clocks, consistent hashing, leader election, two-phase commit, quorums, CRDTs, gossip protocols, Bloom filters, rate limiting, circuit breakers, and LRU caches.

These are the algorithms and patterns that power Amazon, Google, Netflix, and every other major distributed system.

## What to Explore Next

You have covered the foundational algorithms. Here is where to go deeper:

- **Raft consensus** -- The readable alternative to Paxos. A leader is elected per term; all writes go through the leader. Implemented by etcd (Kubernetes), CockroachDB, and TiKV.
- **Paxos** -- The original consensus protocol by Lamport. Notoriously difficult to understand but foundational to Google Chubby and Spanner.
- **MVCC (Multi-Version Concurrency Control)** -- How databases like PostgreSQL and CockroachDB implement snapshot isolation without locking.
- **Saga pattern** -- Managing long-running distributed transactions without 2PC.
- **Consistent Hashing with Virtual Nodes** -- How Cassandra and DynamoDB use virtual nodes for better load balancing.
- **Replication logs** -- WAL shipping, statement-based replication, and row-based replication.

## Build Something

- **A key-value store** -- implement a simple distributed key-value store with consistent hashing and replication.
- **A rate-limited API server** -- build a Node.js server with per-client rate limiting backed by Redis.
- **A Raft implementation** -- implement the Raft leader election protocol across multiple processes.

## References

- *Designing Data-Intensive Applications* by Martin Kleppmann (O'Reilly, 2017) -- the best book on distributed systems for practitioners. Covers replication, partitioning, transactions, consistency, and consensus with clarity and depth.
- [The Raft Paper](https://raft.github.io/raft.pdf) -- "In Search of an Understandable Consensus Algorithm" by Ongaro and Ousterhout. Readable and complete.
- [Amazon DynamoDB Paper](https://www.allthingsdistributed.com/files/amazon-dynamo-sosp2007.pdf) -- the original Dynamo paper. Consistent hashing, quorums, and vector clocks in production.
- [Google Bigtable Paper](https://research.google/pubs/bigtable-a-distributed-storage-system-for-structured-data/) -- how Google stores petabytes of structured data.
- [Jepsen](https://jepsen.io/) -- Kyle Kingsbury's analyses of consistency and failure modes in distributed databases.
- [Martin Kleppmann's Blog](https://martin.kleppmann.com/) -- deep dives into distributed systems research.
`;

const rubyIntroductionContent = `
## Why Ruby?

Ruby is a language designed for programmer happiness. Matz (Yukihiro Matsumoto) created it in 1995 with a single guiding principle: make programming enjoyable. The result is a language that reads almost like English, rewards elegance, and gets out of your way.

- **Expressive** -- Ruby code is concise and readable. A task that takes 10 lines in Java often takes 2 in Ruby.
- **Everything is an object** -- In Ruby, even integers and booleans are objects with methods. \`5.times\`, \`"hello".upcase\`, \`[1,2,3].map\`.
- **Flexible** -- Ruby embraces the idea that there is more than one way to do things. Blocks, procs, lambdas, open classes — the language bends to you.
- **Rails** -- Ruby on Rails, the web framework created by DHH, revolutionized web development. GitHub, Shopify, Basecamp, and Airbnb were all built on Rails.
- **Great for beginners** -- Ruby's syntax is forgiving and the error messages are helpful. It is one of the best first languages.

## The Story

Yukihiro "Matz" Matsumoto began designing Ruby in 1993 and released the first version in 1995. He was inspired by Perl, Smalltalk, Eiffel, Ada, and Lisp — taking what he loved from each.

Ruby remained relatively obscure outside Japan until 2004, when David Heinemeier Hansson (DHH) released Ruby on Rails. Rails showed what Ruby was capable of: a web application framework that made building CRUD apps dramatically faster. The Rails community exploded, and Ruby exploded with it.

Today, Ruby is mature, stable, and widely used in web development, scripting, and DevOps tooling. The MRI (Matz's Ruby Interpreter) is the reference implementation, but JRuby and TruffleRuby bring Ruby to the JVM and GraalVM.

## Who Uses Ruby

Ruby powers some of the world's most recognizable products:

- **GitHub** -- the largest code hosting platform in the world, built on Rails.
- **Shopify** -- e-commerce platform serving over a million merchants, running on a massive Rails monolith.
- **Basecamp** -- the original Rails app, still running on Ruby.
- **Airbnb** -- started on Rails and still uses Ruby extensively.
- **Stripe** -- uses Ruby for parts of its payment infrastructure.

## What You Will Learn

This course contains **15 lessons** organized into **5 chapters**:

1. **Basics** -- \`puts\`, variables, strings with interpolation, and string methods.
2. **Numbers** -- Integer and Float arithmetic, comparison operators, and boolean logic.
3. **Collections** -- Arrays, Hashes, and Ranges — Ruby's three core collection types.
4. **Control Flow** -- Conditionals with \`if\`/\`unless\`, loops with \`while\`/\`until\`, and iterators: \`each\`, \`map\`, \`select\`, \`reduce\`.
5. **Methods & OOP** -- Defining methods with default parameters, blocks and \`yield\`, classes with instance variables, and modules as mixins.

Each lesson explains a concept, shows examples, and gives you an exercise to write and run in your browser.

Let's get started.
`;

const rubyWhatsNextContent = `
## Congratulations

You have completed all 15 lessons. You now know Ruby's core: variables, strings, numbers, arrays, hashes, ranges, control flow, iterators, methods, blocks, classes, and modules.

That is a real foundation. You can read Ruby code, write Ruby programs, and start exploring the vast Ruby ecosystem.

## What to Explore Next

Here are the natural next steps:

- **Ruby on Rails** -- The most popular Ruby framework. Build web applications with models, views, controllers, and a database in hours. Start with the [Rails Guides](https://guides.rubyonrails.org/).
- **RubyGems** -- Ruby's package manager. Over 100,000 gems available. \`gem install\` and \`require\` to add any library.
- **Symbols and frozen strings** -- Ruby's performance optimization for immutable string-like values.
- **Procs and Lambdas** -- First-class functions in Ruby. Understand the difference between \`Proc.new\`, \`proc {}\`, and \`lambda {}\`.
- **Comparable and Enumerable** -- Two of Ruby's most useful modules. Include them in your classes to get dozens of free methods.
- **File I/O** -- \`File.read\`, \`File.write\`, and processing files line by line.
- **Regular expressions** -- Ruby has native regex support: \`"hello" =~ /e(l+)o/\`.
- **Testing** -- RSpec and Minitest, the two dominant testing frameworks. TDD is a core Ruby community value.

## Build Something

The best way to learn is to build:

- **A command-line tool** -- process CSV files, generate reports, or automate a repetitive task.
- **A web scraper** -- use the Nokogiri gem to parse HTML and extract data from websites.
- **A REST API** -- use Sinatra (a lightweight framework) to build a simple HTTP API.
- **A Rails app** -- follow the official Rails tutorial and build a blog or to-do app with a real database.

## References

- [The Ruby Programming Language](https://www.oreilly.com/library/view/the-ruby-programming/9780596516178/) by Matz and David Flanagan (O'Reilly) -- the definitive Ruby book.
- [Programming Ruby (the Pickaxe)](https://pragprog.com/titles/ruby5/programming-ruby-3-3-5th-edition/) by Dave Thomas -- the classic Ruby reference.
- [Ruby Documentation](https://ruby-doc.org/) -- official API docs for the standard library.
- [RubyGems](https://rubygems.org/) -- the Ruby package repository.
- [Ruby on Rails Guides](https://guides.rubyonrails.org/) -- comprehensive guides for the most popular Ruby framework.
- [The Odin Project](https://www.theodinproject.com/paths/full-stack-ruby-on-rails) -- free, comprehensive full-stack curriculum using Ruby and Rails.
`;

const treesIntroductionContent = `
## Why Trees?

Trees are the most important data structure beyond arrays and linked lists. They appear everywhere in software:

- **File systems** — directories are trees. Every \`ls\` you run traverses one.
- **The DOM** — every webpage is a tree of HTML elements.
- **Compilers** — source code is parsed into an Abstract Syntax Tree (AST) before execution.
- **Databases** — B-trees power the indexes that make SQL queries fast.
- **Git** — the commit graph is a tree (actually a DAG). Every \`git log\` traverses it.
- **DNS** — the domain name system is a tree. \`com\` → \`example\` → \`www\`.

Understanding trees is the gateway to understanding how real systems work at depth.

## Why C?

Implementing trees in C forces you to understand them at the lowest level:

- There is no garbage collector. You call \`malloc\` to create a node, and you own that memory.
- There is no tree library. You write the struct, the insert, the traversal — everything.
- The pointer manipulation is explicit. \`root->left = new_node(val)\` is not hidden behind an object system.

When you implement a BST in C, you understand what Python's \`dict\` and Java's \`TreeMap\` are doing under the hood.

## What You Will Learn

This course contains **12 lessons** organized into **4 chapters**:

1. **Binary Trees** — Define the Node struct, create nodes with malloc, and implement all three classic traversals: inorder, preorder, and postorder.
2. **Tree Properties** — Recursive algorithms to count nodes, measure height, and count leaves.
3. **Binary Search Tree** — Insert values, search for values, and find min/max using the BST property.
4. **BST Operations** — Validate that a tree is a correct BST using the min-max technique.

Each lesson explains the concept, walks through the algorithm, and gives you a function to implement and test.

Let's build trees.
`;

const treesWhatsNextContent = `
## Congratulations

You have implemented 12 tree algorithms in C: node creation, all three traversals, node count, height, leaf count, sum, BST insert, BST search, min/max, and BST validation.

That is real data structures knowledge — the kind that comes up in every technical interview and underpins every database and compiler ever written.

## What to Explore Next

You have covered the fundamentals. Here is where to go deeper:

- **BST Delete** — The hardest BST operation. When deleting a node with two children, replace it with its inorder successor (the smallest node in its right subtree).
- **Balanced BSTs (AVL Trees)** — A BST stays O(log n) only if it's balanced. AVL trees self-balance on insert and delete using rotations.
- **Red-Black Trees** — The self-balancing BST used by Linux's process scheduler, Java's \`TreeMap\`, and C++'s \`std::map\`.
- **B-Trees** — N-ary trees optimized for disk access. The structure behind every database index.
- **Tries** — Trees for string prefixes. Used in autocomplete, spell checkers, and IP routing tables.
- **Heaps** — A tree with a different invariant (parent ≤ children). The structure behind priority queues.

## Build Something

- **A sorted set** — use a BST to build a dynamic sorted container with insert, search, delete, and iteration.
- **An expression evaluator** — parse arithmetic expressions into a tree and evaluate them recursively.
- **A file system simulator** — implement a tree of directories and files with path traversal.
- **A spell checker** — build a trie from a dictionary and check words character by character.

## References

- *Introduction to Algorithms* (CLRS) by Cormen, Leiserson, Rivest, and Stein — Chapter 12 covers BSTs; Chapters 13-14 cover Red-Black Trees and augmented data structures.
- *The Algorithm Design Manual* by Steven Skiena — practical discussion of tree variants and when to use each.
- [Visualgo - BST](https://visualgo.net/en/bst) — animated visualization of BST insert, delete, and search.
- [CS50x Data Structures](https://cs50.harvard.edu/x/) — Harvard's free course covers trees with excellent C examples.
`;

const linkedListsIntroductionContent = `
## Why Linked Lists?

Linked lists are the foundational heap-allocated data structure. Before you can understand trees, graphs, hash tables, or memory allocators, you need to understand the linked list — because they all share the same core idea: nodes connected by pointers.

They appear everywhere in systems programming:

- **Operating systems** — the Linux kernel uses doubly-linked lists extensively (process lists, wait queues, LRU caches).
- **Memory allocators** — free blocks are tracked as a linked list.
- **Compilers** — symbol tables and intermediate representations use linked structures.
- **Undo/redo** — editor history is a linked list of states.

## Why C?

Linked lists without manual memory management are just exercises. In C, you call \`malloc\` to allocate each node, you own the pointer, and you call \`free\` when you are done. That is the full picture — no garbage collector hiding the cost.

Implementing a linked list in C teaches you:

- How heap allocation works at the function-call level.
- Why pointer manipulation requires care (NULL checks, prev/cur tracking).
- What languages like Python and Java are doing behind their list objects.

## What You Will Learn

This course contains **12 lessons** organized into **4 chapters**:

1. **The Node** — Define the \`Node\` struct, allocate nodes with \`malloc\`, traverse and print a list, and measure its length.
2. **Insertions & Deletions** — Insert at the front (O(1)), insert at the back (O(n)), and remove the front node.
3. **Search & Access** — Linear search, O(n) index access, and delete-by-value with the prev/cur two-pointer pattern.
4. **Classic Problems** — Three interview staples: nth-from-end (fast/slow pointers), in-place reversal, and merging two sorted lists.

Each lesson explains the algorithm, walks through a worked example, and gives you a function to implement.

Let's build lists.
`;

const linkedListsWhatsNextContent = `
## Congratulations

You have implemented 12 linked list algorithms in C: node creation, traversal, length, push front/back, pop front, search, nth node, delete by value, nth from end, reverse, and merge sorted.

These are exactly the problems that come up in systems programming interviews and in real kernel/allocator code. You know them at the pointer level now — not just abstractly.

## What to Explore Next

- **Doubly-Linked Lists** — Add a \`prev\` pointer to each node. O(1) deletion anywhere (given the node pointer). The Linux kernel's list implementation uses this.
- **Circular Lists** — The tail's \`next\` points back to the head. Useful for round-robin schedulers.
- **Skip Lists** — Probabilistic multi-level linked lists that achieve O(log n) search. Used in Redis and LevelDB.
- **XOR Lists** — Store \`prev XOR next\` in a single pointer field, halving memory use. A classic bit-manipulation trick.
- **Memory Allocators** — Implement a free list allocator: \`malloc\` returns from the front of the free list; \`free\` inserts back in order.

## Build Something

- **A stack** — use push_front/pop_front to implement LIFO semantics with O(1) push and pop.
- **A queue** — maintain head and tail pointers; O(1) enqueue at tail, O(1) dequeue at head.
- **A sorted insert** — insert values in sorted order into a linked list (the basis of insertion sort).
- **Cycle detection** — implement Floyd's tortoise-and-hare algorithm to detect if a list has a cycle.

## References

- *The C Programming Language* by Kernighan and Ritchie — Chapter 6 covers structs and the basics of pointer-linked data structures.
- *Introduction to Algorithms* (CLRS) — Chapter 10 covers linked lists, stacks, and queues at depth.
- [Linux kernel list.h](https://github.com/torvalds/linux/blob/master/include/linux/list.h) — the actual doubly-linked list implementation used throughout the Linux kernel. Read it after this course.
`;

const threejsIntroductionContent = `
## Why Three.js?

Three.js is the most popular JavaScript library for 3D graphics on the web. It wraps WebGL — the raw GPU API — into a clean, high-level scene graph that lets you create stunning 3D experiences without writing a single line of GLSL shader code.

- **WebGL made easy** -- WebGL requires hundreds of lines of boilerplate for a spinning cube. Three.js reduces it to 10.
- **Runs everywhere** -- Any modern browser supports it. No plugins, no installs, no native builds.
- **Huge ecosystem** -- Thousands of examples, plugins, and a massive community.
- **Real-time rendering** -- 60fps animations, particle systems, physics-ready geometry, and post-processing effects.
- **3D + DOM** -- Combine Three.js with React, Vue, or plain HTML. React Three Fiber makes it declarative.

## The Story

Three.js was created by Ricardo Cabello (known online as Mr.doob) and first released in 2010. It started as a port of a ActionScript 3D engine to JavaScript, but quickly became its own thing.

The library has grown from a weekend project to the de-facto standard for WebGL — used by Google, NASA, Apple, and thousands of creative developers worldwide. It is entirely open-source (MIT license) and has over 1,000 contributors on GitHub.

## What You Will Build

In this course you will:

- Understand the **scene graph**: scenes, cameras, renderers, meshes
- Work with **geometries** (Box, Sphere, Torus, Cylinder) and **materials** (Basic, Standard, Phong)
- Add **lights** (Ambient, Directional, Point, Spot) for realistic shading
- **Transform** objects with position, rotation, and scale
- Build **animation loops** with delta time for frame-rate independence
- Organize objects into **groups** and hierarchies
- Control the **camera** and field of view
- Add atmospheric **fog**
- Detect mouse interaction with **raycasting**
- Create **particle systems** with BufferGeometry
- Combine everything into a final **Solar System** scene

Every lesson renders a live 3D preview directly in your browser.
`;

const threejsWhatsNextContent = `
## What to Explore Next

### Three.js Ecosystem
- **React Three Fiber** -- Declarative Three.js for React. Compose 3D scenes with components and hooks. See [docs.pmnd.rs](https://docs.pmnd.rs/react-three-fiber).
- **Drei** -- Useful helpers and abstractions for R3F: OrbitControls, Environment, Text3D, and more.
- **Cannon.js / Rapier** -- Physics engines for realistic simulations (gravity, collisions, joints).
- **Postprocessing** -- Bloom, depth of field, chromatic aberration, and other full-screen effects.

### Shaders
- **GLSL** -- The shading language behind WebGL. Write custom vertex and fragment shaders with \`ShaderMaterial\`.
- **The Book of Shaders** -- [thebookofshaders.com](https://thebookofshaders.com/) — the best free resource for learning GLSL.

### Resources
- [Three.js Documentation](https://threejs.org/docs/) — comprehensive API reference with live examples.
- [Three.js Journey](https://threejs-journey.com/) — the most popular paid Three.js course by Bruno Simon.
- [Three.js Examples](https://threejs.org/examples/) — hundreds of official demos and experiments.
- [Discover Three.js](https://discoverthreejs.com/) — free online book covering fundamentals.
`;

const pythonIntroductionContent = `
## Why Python?

Python is the most popular programming language in the world. It powers artificial intelligence, data science, web development, automation, and scientific computing. Its philosophy — "there should be one obvious way to do it" — makes it readable and maintainable at scale.

- **Readable syntax** -- Python reads almost like English. No semicolons, no braces, indentation defines structure.
- **Dynamically typed** -- No type declarations. Faster to write, easier to prototype.
- **Batteries included** -- A massive standard library covers HTTP, JSON, CSV, regular expressions, testing, and much more.
- **Huge ecosystem** -- PyPI hosts over 500,000 packages: NumPy, Pandas, Django, Flask, FastAPI, TensorFlow, PyTorch.
- **Interactive** -- Python's REPL and Jupyter notebooks make exploration fast and visual.

## The Story

Python was created by Guido van Rossum, who started working on it in December 1989 as a hobby project during the Christmas holidays. He wanted a language that bridged the gap between C and shell scripting — powerful yet easy to use. Python 1.0 was released in 1994.

Python 2 and Python 3 coexisted for years, but Python 2 reached end-of-life in 2020. All modern Python is Python 3. The language is governed by the Python Software Foundation and maintained by a global community of contributors.

Python's name comes not from the snake, but from Monty Python's Flying Circus. The language has always had a sense of humor — the canonical package installer is called \`pip\`, and the Zen of Python (\`import this\`) includes aphorisms like "Readability counts."

## What You Will Learn

In this course you will master:

- **Foundations**: variables, strings, numbers, lists, and dictionaries
- **Control flow**: if/elif/else, for loops, while loops, break/continue
- **Functions**: arguments, defaults, \`*args\`, \`**kwargs\`, higher-order functions, lambdas
- **Comprehensions**: list, dict, and set comprehensions
- **Object-oriented programming**: classes, inheritance, dunder methods
- **Error handling**: try/except/finally, raising exceptions
- **Generators**: lazy evaluation, infinite sequences, \`yield\`
- **Standard library**: \`collections\`, \`itertools\`, \`functools\`
- **Recursion**: base cases, recursive cases, memoization
`;

const pythonWhatsNextContent = `
## What to Explore Next

### Web Development
- **FastAPI** -- Modern, fast Python web framework. Automatic API docs, type hints, async support. The fastest-growing Python web framework. [fastapi.tiangolo.com](https://fastapi.tiangolo.com/)
- **Django** -- The batteries-included framework. ORM, admin panel, auth, templates. Great for full-stack apps. [djangoproject.com](https://www.djangoproject.com/)
- **Flask** -- Lightweight WSGI framework. Start small, add only what you need.

### Data Science & AI
- **NumPy** -- Fast multi-dimensional arrays and linear algebra. The foundation of the scientific Python stack.
- **Pandas** -- DataFrames for data manipulation and analysis. Read CSV, SQL, Excel; filter, group, aggregate.
- **Matplotlib / Seaborn** -- Data visualization.
- **scikit-learn** -- Machine learning: classification, regression, clustering, pipelines.
- **PyTorch / TensorFlow** -- Deep learning frameworks for building neural networks.

### Advanced Python
- **async/await** -- Asyncio for concurrent I/O without threads. Powerful for web servers and network clients.
- **Type hints + mypy** -- Static type checking in Python. Catches bugs at development time.
- **Decorators and metaclasses** -- Advanced metaprogramming patterns.
- **Dataclasses** -- \`@dataclass\` reduces boilerplate for data-holding classes.

### Resources
- [The Python Tutorial](https://docs.python.org/3/tutorial/) — Official Python docs, comprehensive and well-written.
- [Real Python](https://realpython.com/) — Practical tutorials on all aspects of Python.
- [Python Cookbook](https://www.oreilly.com/library/view/python-cookbook-3rd/9781449357337/) by David Beazley — Advanced recipes and idioms.
- [Fluent Python](https://www.oreilly.com/library/view/fluent-python-2nd/9781492056348/) by Luciano Ramalho — Deep dive into Python's data model.
- [PyPI](https://pypi.org/) — The Python Package Index. Find packages for any task.
`;

const linearAlgebraIntroductionContent = `
## Why Linear Algebra?

Linear algebra is the mathematics of vectors and matrices — the language of machine learning, computer graphics, scientific computing, and data analysis. Every neural network, every 3D game, every recommendation system is built on it.

- **Machine Learning** — gradient descent, PCA, SVD, attention in transformers
- **Computer Graphics** — transformations, projections, shading
- **Data Science** — dimensionality reduction, regression, covariance
- **Physics Simulations** — systems of differential equations
- **Cryptography** — lattice-based cryptography is pure linear algebra

## Why NumPy and SymPy?

**NumPy** gives you fast numerical linear algebra:
- Vectors and matrices as \`np.array\`
- \`np.dot\`, \`np.linalg.solve\`, \`np.linalg.eig\` — all backed by LAPACK and BLAS
- Runs at C speed on arrays of millions of values

**SymPy** gives you exact symbolic algebra with beautiful rendering:
- Symbols, expressions, equations — no floating point error
- \`sympy.pprint()\` renders equations as Unicode art in your terminal:

\`\`\`
 ⎡1  2⎤           2
 ⎢    ⎥   (x + 1)
 ⎣3  4⎦
\`\`\`

Together, they cover both the computational and symbolic sides of linear algebra.

## What You Will Learn

This course contains **15 lessons** organized into **4 chapters**:

1. **Vectors** — Create NumPy vectors, perform element-wise operations, compute dot products, and normalize with the L2 norm.
2. **Matrices** — Build 2D matrices, apply transpose, multiply with the \`@\` operator, and compute determinants.
3. **Linear Systems** — Test invertibility, solve \`Ax = b\` with \`np.linalg.solve\`, find eigenvalues, and fit lines with least squares.
4. **Symbolic Math** — Use SymPy to factor polynomials, solve equations exactly, and render expressions as beautiful Unicode math.

Let's compute.
`;

const linearAlgebraWhatsNextContent = `
## Congratulations

You have implemented 15 linear algebra concepts in Python: vectors, element-wise operations, dot products, norms, matrices, transpose, multiplication, determinants, invertibility, solving systems, eigenvalues, least squares, symbolic factoring, symbolic solving, and symbolic matrix algebra.

This is the mathematical foundation for machine learning, graphics, and scientific computing.

## What to Explore Next

- **Singular Value Decomposition (SVD)** — \`np.linalg.svd\`. Every matrix has an SVD. It powers image compression (JPEG), recommendation systems (Netflix), and dimensionality reduction.
- **Principal Component Analysis (PCA)** — compute eigenvectors of the covariance matrix to find the directions of maximum variance. The foundation of dimensionality reduction.
- **NumPy Broadcasting** — operations between arrays of different shapes. Eliminates explicit loops for most tensor operations.
- **SciPy** — builds on NumPy with sparse matrices, FFT, optimization, and statistics.
- **Matplotlib** — visualize vectors, transformations, eigenspaces, and regression lines.

## Build Something

- **Image compression** — load a grayscale image as a matrix, compute its SVD, reconstruct with k singular values, and compare quality vs. size.
- **Linear regression** — implement gradient descent from scratch using matrix operations on the Boston housing dataset.
- **2D transformation visualizer** — apply rotation/scaling/shear matrices to a set of points and plot before/after.
- **PageRank** — model the web as an adjacency matrix and find the dominant eigenvector.

## References

- *Linear Algebra and Its Applications* by Gilbert Strang — the most readable linear algebra textbook, used at MIT.
- [3Blue1Brown: Essence of Linear Algebra](https://www.3blue1brown.com/topics/linear-algebra) — 16 videos with stunning visualizations of every concept from this course.
- [NumPy documentation](https://numpy.org/doc/stable/reference/routines.linalg.html) — complete reference for \`np.linalg\`.
- [SymPy documentation](https://docs.sympy.org/latest/tutorials/intro-tutorial/index.html) — SymPy tutorial and full API.
`;

const statisticsIntroductionContent = `
## Why Statistics?

Statistics is the science of learning from data. It gives you the tools to summarize data, quantify uncertainty, test hypotheses, and make predictions. Every field that works with data — from medicine to machine learning — depends on statistical reasoning.

- **Data science & ML** — train/test splits, evaluation metrics, A/B testing, overfitting detection. Statistics is the foundation beneath every model.
- **Scientific research** — clinical trials, psychology experiments, and physics measurements all rely on hypothesis testing and confidence intervals.
- **Business** — conversion rate optimization, product analytics, forecasting, and pricing all use statistical methods daily.
- **Engineering** — quality control, signal processing, reliability analysis.

## The Python Statistics Stack

This course uses three complementary libraries:

- **\`statistics\`** — Python's built-in module for basic descriptive stats (mean, median, mode, stdev). No dependencies needed.
- **NumPy** — fast array operations, percentiles, random sampling, and correlation. The backbone of scientific Python.
- **SciPy** — probability distributions, hypothesis tests (t-test, chi-square), standard error, and linear regression via \`scipy.stats\`.

## What You Will Learn

This course contains **15 lessons** organized into **4 chapters**:

1. **Descriptive Statistics** — Summarize datasets with mean, median, mode, variance, standard deviation, percentiles, and z-scores.
2. **Distributions** — Model random phenomena with the normal and binomial distributions. Understand sampling, the Central Limit Theorem, and standard error.
3. **Inference** — Draw conclusions from samples: one-sample and two-sample t-tests, confidence intervals, and chi-square goodness-of-fit.
4. **Regression** — Measure relationships: Pearson correlation, linear regression, and bootstrap resampling for distribution-free inference.

## In-Browser Runtime

All code runs live in your browser via **Pyodide** — CPython compiled to WebAssembly. NumPy and SciPy load automatically from your import statements.
`;

const statisticsWhatsNextContent = `
## Continue Your Statistics Journey

### Deeper Statistics

- **ANOVA** — compare means across three or more groups simultaneously.
- **Multiple regression** — model y as a linear combination of multiple predictors using \`statsmodels\`.
- **Bayesian inference** — quantify beliefs with prior distributions and update with data using PyMC or Stan.
- **Non-parametric tests** — Mann-Whitney U, Wilcoxon signed-rank for when normality can't be assumed.

### Visualization

- **Matplotlib** — histograms, box plots, scatter plots with regression lines, QQ-plots for normality.
- **Seaborn** — statistical visualization with built-in distribution plots, pair plots, and heatmaps.

## Build Something

- **A/B test simulator** — generate two conversion rate datasets, run a two-sample t-test, and estimate the sample size needed for statistical power.
- **Distribution explorer** — interactive sliders for μ and σ, showing how the normal distribution PDF and CDF change.
- **Bootstrap vs. t-test comparison** — verify that bootstrap CIs and t-test CIs agree for normally distributed data.

## References

- *Statistics* by Freedman, Pisani & Purves — the most intuitive introduction to statistical reasoning.
- [Think Stats](https://greenteapress.com/thinkstats2/) by Allen Downey — statistics with Python, free online.
- [SciPy stats documentation](https://docs.scipy.org/doc/scipy/reference/stats.html) — full reference for all distributions and tests.
- [StatQuest with Josh Starmer](https://www.youtube.com/@statquest) — YouTube channel with clear visual explanations of every concept in this course.
`;

const graphsIntroductionContent = `
## Why Graph Algorithms?

Graphs are one of the most powerful abstractions in computer science. They model relationships between things — web pages and links, cities and roads, users and friendships, tasks and dependencies. Nearly every complex problem at scale can be framed as a graph problem.

- **Social networks** — Facebook, LinkedIn, Twitter are graphs. Shortest path gives degrees of separation. PageRank ranks influence.
- **Maps & Navigation** — Google Maps uses Dijkstra and A* to find shortest routes.
- **Compilers** — Topological sort orders declarations. Cycle detection finds circular imports.
- **Networking** — Routing protocols (OSPF) use shortest-path algorithms.
- **Machine Learning** — Knowledge graphs, graph neural networks, dependency parsing.

## What You Will Learn

This course implements the core graph algorithms from scratch in Python:

- **Representations**: adjacency lists — the foundation of all graph algorithms
- **Traversals**: BFS (level-by-level) and DFS (deep-first) — the building blocks
- **Connectivity**: has_path, connected components, bipartite check
- **Cycle detection**: undirected (parent tracking) and directed (3-color DFS)
- **Topological sort**: Kahn's algorithm for ordering DAGs
- **Shortest paths**: Dijkstra (non-negative weights) and Bellman-Ford (negative weights)
- **Union-Find**: path compression and union by rank — nearly O(1) per operation
- **Minimum Spanning Tree**: Kruskal's algorithm using Union-Find
- **PageRank**: the algorithm behind Google Search

Every function is tested against concrete examples with known correct outputs.
`;

const graphsWhatsNextContent = `
## What to Explore Next

### More Graph Algorithms
- **A* Search** — Dijkstra with a heuristic. Used for pathfinding in games and maps. Requires an admissible heuristic.
- **Floyd-Warshall** — All-pairs shortest paths in O(V³). Great for dense graphs or when you need distances between all pairs.
- **Tarjan's SCC** — Strongly Connected Components in a single DFS pass. More efficient than Kosaraju's.
- **Network Flow** — Edmonds-Karp (BFS-based Ford-Fulkerson) for max flow / min cut problems.
- **Articulation Points & Bridges** — Find critical infrastructure nodes/edges in a network.

### Applications
- **Word Ladder** — BFS on a graph where words are nodes and edges connect words differing by one letter.
- **Course Schedule** — LeetCode 207/210: topological sort with cycle detection.
- **Number of Islands** — DFS/BFS on a 2D grid graph.
- **Alien Dictionary** — Topological sort from character ordering constraints.

### Tools & Libraries
- **NetworkX** — The Python graph library. Implements hundreds of algorithms, visualizes graphs, handles massive networks.
- **igraph** — Fast graph analysis for Python and R.
- **Gephi** — Graph visualization and exploration tool.
- **Neo4j** — Graph database for storing and querying graph-structured data with Cypher query language.

### Further Reading
- *Introduction to Algorithms* (CLRS) — Chapters 22-25 cover all graph algorithms in depth.
- *Algorithm Design Manual* by Steven Skiena — Practical graph algorithms with real-world applications.
- [CP-algorithms.com](https://cp-algorithms.com/) — Detailed explanations with implementations for competitive programming.
- [LeetCode Graph Problems](https://leetcode.com/tag/graph/) — 200+ practice problems from easy to hard.
`;

const haskellIntroductionContent = `
## Why Haskell?

Haskell is a purely functional, statically typed programming language with lazy evaluation. Learning Haskell changes how you think about programming — even if you never write it professionally.

- **Pure functions** -- No hidden state, no side effects. Every function takes inputs and returns outputs. The same input always produces the same output.
- **Strong type system** -- Haskell's types catch entire classes of bugs at compile time. The famous saying: "If it compiles, it often works."
- **Pattern matching** -- Destructure data with elegance. Define functions by cases, matching on shape and value simultaneously.
- **Lazy evaluation** -- Haskell only computes values when needed. You can define infinite lists and only use the parts you want.
- **Conciseness** -- Haskell programs are often a fraction of the length of equivalent imperative code.

## The Story

Haskell was born in 1987 when a committee of researchers decided that the proliferation of lazy functional languages needed a single, open standard. Named after the logician Haskell Curry, version 1.0 was released in 1990. GHC (Glasgow Haskell Compiler) is the de-facto standard implementation.

## What You Will Learn

This course contains **15 lessons** organized into **4 chapters**:

1. **Basics** -- Printing output, variables and bindings, arithmetic, and strings.
2. **Functions** -- Defining functions, if expressions, guards, and pattern matching.
3. **Lists** -- Creating lists, ranges, map, filter, take, drop, zip, and list comprehensions.
4. **Functional Patterns** -- Lambdas, folds, where/let bindings, and the Maybe type.

Let's get started.
`;

const haskellWhatsNextContent = `
## Congratulations

You have completed all 15 lessons. You now understand Haskell's core ideas: pure functions, pattern matching, list comprehensions, higher-order functions, folds, and the Maybe type.

## What to Explore Next

- **Type classes** -- \`Eq\`, \`Ord\`, \`Show\`, \`Functor\`, \`Monad\` — Haskell's polymorphism mechanism.
- **Monads** -- The abstraction behind \`IO\`, \`Maybe\`, \`Either\`, and \`List\`.
- **Algebraic data types** -- \`data Tree a = Leaf | Node a (Tree a) (Tree a)\` — how Haskell models the world.

## References

- [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/) — The classic free online introduction.
- [Real World Haskell](http://book.realworldhaskell.org/) — Practical Haskell for production use.
- [Haskell Wiki](https://wiki.haskell.org/) — Community-maintained documentation.
`;

const calculusIntroductionContent = `
## Why Calculus?

Calculus is the mathematics of change. It was invented independently by Newton and Leibniz in the 17th century to solve problems that algebra couldn't — the slope of a curve at a single point, the area under an arbitrary shape, the motion of planets.

Today, calculus underpins:

- **Physics** -- every differential equation in mechanics, electromagnetism, and quantum theory
- **Engineering** -- control systems, signal processing, structural analysis
- **Machine learning** -- gradient descent is pure applied calculus: minimize a loss function by following the negative gradient
- **Finance** -- Black-Scholes option pricing, continuous compounding
- **Computer graphics** -- curves, surfaces, and physically-based rendering

## Why Implement It in C?

Most calculus courses focus on analytic techniques: the power rule, integration by parts, u-substitution. This course takes a different angle — you implement the **numerical algorithms** that compute what pen-and-paper calculus describes.

This approach:

- Reveals the limit definition of the derivative as actual code
- Shows why some methods converge faster than others
- Prepares you for scientific computing, simulation, and numerical analysis
- Builds deep intuition: you can't implement something you don't understand

## What You Will Learn

This course covers the core of **Calculus 1** through C implementations:

1. **Limits & Derivatives** -- Numerical limits, central difference formula, second derivative, tangent line linearization
2. **Derivative Applications** -- Newton's method for root finding, finding critical points, the Mean Value Theorem
3. **Integration** -- Left/right Riemann sums, midpoint rule, trapezoidal rule, Simpson's rule
4. **Integral Applications** -- Average value, area between curves, volume of revolution (disk method)

Every function uses **function pointers** — C's way of passing functions as arguments. This is the gateway to understanding higher-order functions and functional programming.
`;

const calculusWhatsNextContent = `
## Congratulations

You have completed all 15 lessons. You can now implement the core algorithms of Calculus 1 in C — from numerical limits and derivatives through Riemann sums, Simpson's rule, and volumes of revolution.

## What to Explore Next

- **Calculus 2** -- Integration techniques (by parts, trig substitution, partial fractions), sequences and series, Taylor series, polar coordinates
- **Multivariable Calculus** -- Partial derivatives, gradient, divergence, curl, multiple integrals, Stokes' theorem
- **Numerical Analysis** -- Adaptive step-size methods, Gaussian quadrature, spline interpolation — the professional tools built on these foundations
- **Differential Equations** -- Everything here feeds directly into ODEs and PDEs

## Key Formulas to Remember

| Concept | Formula |
|---------|---------|
| Central difference | \`f'(x) ≈ (f(x+h) - f(x-h)) / (2h)\` |
| Second derivative | \`f''(x) ≈ (f(x+h) - 2f(x) + f(x-h)) / h²\` |
| Newton's method | \`xₙ₊₁ = xₙ - f(xₙ)/f'(xₙ)\` |
| Trapezoidal rule | \`h/2·[f(x₀) + 2f(x₁) + ... + 2f(xₙ₋₁) + f(xₙ)]\` |
| Simpson's rule | \`h/3·[f(x₀) + 4f(x₁) + 2f(x₂) + ... + 4f(xₙ₋₁) + f(xₙ)]\` |
| Volume of revolution | \`π ∫_a^b f(x)² dx\` |

## References

- [Calculus](https://www.stewartcalculus.com/) by James Stewart -- the standard university textbook
- [Calculus Made Easy](https://calculusmadeeasy.org/) by Silvanus P. Thompson -- free, clear, and written in 1910
- [Numerical Recipes in C](http://numerical.recipes/) -- the practitioner's guide to numerical algorithms
`;

const calculus2IntroductionContent = `
## Why Calculus 2?

Calculus 2 takes the foundations of Calculus 1 and extends them in three major directions: more powerful integration techniques, the theory of infinite series, and new coordinate systems for curves that can't be described with simple \`y = f(x)\` equations.

- **Integration Applications** -- Arc length, surface area, work done by variable forces, and improper integrals that span infinite intervals
- **Sequences and Series** -- Rigorous theory of convergence: geometric series, p-series, alternating series, and the ratio test
- **Taylor Series** -- Approximate any smooth function as a polynomial, with quantified error bounds — the computational backbone of numerical computing
- **Parametric and Polar** -- Curves defined by parameter or angle: circles, spirals, rose curves, and their arc lengths and areas

## The Computational Angle

This course implements every concept in C using **numerical algorithms** — no symbolic algebra. You will:

- Compute arc lengths and surface areas with numerical integration + numerical derivatives
- Approximate improper integrals by integrating to large finite bounds
- Implement partial sums to observe series convergence empirically
- Build a Taylor polynomial engine using recursive central differences
- Compute curvature using the second-derivative formula

Each function you write is a small numerical experiment. When your output matches the analytic formula, you know both the theory and the implementation are correct.

## Prerequisites

Calculus 1 (limits, derivatives, integrals) and basic C programming (pointers, loops, structs). The Calculus 1 in C course on this platform is ideal preparation.
`;

const calculus2WhatsNextContent = `
## Congratulations

You have completed all 15 lessons of Calculus 2. You can now implement arc length, surface area, improper integrals, partial sums, Taylor polynomials, parametric arc length, polar area, and curvature — all from scratch in C.

## What to Explore Next

- **Multivariable Calculus** -- Partial derivatives, gradient descent, Lagrange multipliers, double and triple integrals, line integrals, Stokes' theorem
- **Differential Equations** -- Apply integration and series directly to ODEs: separation of variables, series solutions, Laplace transforms
- **Numerical Analysis** -- Adaptive step-size quadrature, Gaussian quadrature, Chebyshev approximation — the professional tools built on these ideas
- **Complex Analysis** -- Taylor and Laurent series in the complex plane, residue theorem, conformal mappings

## Key Formulas

| Concept | Formula |
|---------|---------|
| Arc length | \`∫_a^b √(1 + f'²) dx\` |
| Surface area | \`2π ∫_a^b f(x)√(1+f'²) dx\` |
| Geometric series | \`a/(1-r)\` for \`|r|<1\` |
| Taylor polynomial | \`Σ f^(k)(a)/k! · (x-a)^k\` |
| Polar area | \`½ ∫_a^b r(θ)² dθ\` |
| Curvature | \`|f''| / (1+f'²)^{3/2}\` |

## References

- [Calculus](https://www.stewartcalculus.com/) by James Stewart — the standard university textbook (Chapters 8–12)
- [Numerical Recipes in C](http://numerical.recipes/) — the practitioner's guide to numerical algorithms
- [3Blue1Brown: Essence of Calculus](https://www.3blue1brown.com/topics/calculus) — visual intuition for the concepts
`;

const circuitsIntroductionContent = `
## Why Circuits in C?

Electronics is the foundation of every computing device — but circuit analysis is usually taught with hand calculations, not code. This course bridges that gap: every fundamental relationship is implemented as a C function and tested numerically.

- **DC Fundamentals** -- Ohm's law, series and parallel resistance, voltage and current dividers — the building blocks of every circuit
- **Circuit Analysis** -- Power dissipation, KCL node analysis, and the Wheatstone bridge — tools for solving arbitrary resistor networks
- **Transient Response** -- RC and RL exponential transients, RLC oscillations via RK4 — how circuits behave in time, not just at steady state
- **AC & Filters** -- Low-pass and high-pass filter gain, LC resonant frequency — the frequency domain and why it matters

## The Computational Angle

Each lesson asks you to implement a formula or numerical method in C. The verification is immediate: run your code and compare to the exact analytic result. When they match, you understand both the theory and the numbers.

No oscilloscope required. No breadboard. Just C and the equations.

## Prerequisites

Basic C programming (loops, arrays, pointers, function pointers). The ARM64 in C course on this platform is good preparation. No prior electronics knowledge needed — every concept is introduced from first principles.
`;

const circuitsWhatsNextContent = `
## Congratulations

You have completed all 15 lessons of Circuits in C. You can now implement Ohm's law, series/parallel resistance, voltage and current dividers, power calculations, KCL node analysis, Wheatstone bridge, RC and RL transients, RLC oscillators (RK4), capacitor/inductor energy, and RC filter frequency response — all from scratch in C.

## What to Explore Next

- **SPICE** — Simulate arbitrary circuits using ngspice netlist format. The Circuits in C course is the perfect foundation for understanding what SPICE is computing.
- **Signal Processing in Python** — Take filter design to the next level with scipy.signal: Butterworth filters, Bode plots, FFT analysis
- **Differential Equations** — The RLC circuit is just one second-order ODE. The diffeq course generalizes RK4 to any system
- **Embedded Systems** — Apply circuit knowledge to real hardware: ADC, PWM, I2C, SPI on microcontrollers

## Key Formulas

| Concept | Formula |
|---------|---------|
| Ohm's Law | \`V = I·R\` |
| Series resistance | \`R = R₁ + R₂ + ...\` |
| Parallel resistance | \`1/R = 1/R₁ + 1/R₂ + ...\` |
| Voltage divider | \`Vout = Vin·R₂/(R₁+R₂)\` |
| RC time constant | \`τ = R·C\` |
| RC charging | \`V(t) = Vs·(1 − e^{−t/τ})\` |
| RC low-pass gain | \`|H| = 1/√(1+(ωRC)²)\` |
| LC resonance | \`f₀ = 1/(2π√(LC))\` |

## References

- [The Art of Electronics](https://artofelectronics.net/) by Horowitz & Hill — the definitive practical guide
- [All About Circuits](https://www.allaboutcircuits.com/) — free online textbook series
- [Falstad Circuit Simulator](https://www.falstad.com/circuit/) — interactive visual circuit simulation
`;

const calculus3IntroductionContent = `
## Why Calculus 3?

Calculus 3 extends single-variable calculus into **multiple dimensions**. Instead of curves, we study surfaces. Instead of slopes, we study gradients. Instead of integrals over intervals, we integrate over areas and volumes.

- **Vectors in 3D** -- Dot products, cross products, magnitudes, and projections — the language of 3D geometry
- **Partial Derivatives** -- How a multivariable function changes as each variable moves independently
- **Optimization** -- Finding maxima and minima of surfaces using the gradient and the second derivative test
- **Multiple Integrals** -- Integrating over 2D regions and 3D volumes, in Cartesian and polar coordinates

## The Computational Angle

Every concept is implemented in C as a numerical algorithm. You will:

- Compute dot and cross products from scratch
- Approximate partial derivatives using central differences
- Find and classify critical points with the discriminant
- Evaluate double and triple integrals using the midpoint rule in 2D and 3D
- Integrate over polar coordinates using the Jacobian factor r

## Prerequisites

Calculus 1 and 2 (limits, derivatives, integrals, series) and basic C programming (pointers, function pointers, loops). Calculus 1 in C and Calculus 2 in C on this platform are ideal preparation.
`;

const calculus3WhatsNextContent = `
## Congratulations

You have completed all 15 lessons of Calculus 3. You can now implement dot and cross products, partial derivatives, gradients, tangent planes, the Laplacian, the second derivative test, and double, polar, and triple integrals — all from scratch in C.

## What to Explore Next

- **Vector Calculus** -- Line integrals, surface integrals, Green's theorem, Stokes' theorem, the divergence theorem
- **Differential Equations** -- Apply partial derivatives directly to PDEs: the heat equation, wave equation, and Laplace's equation
- **Numerical Analysis** -- Adaptive quadrature in multiple dimensions, Monte Carlo integration, finite element methods
- **Linear Algebra** -- The algebra behind gradients and Jacobians: eigenvalues, diagonalization, matrix calculus

## Key Formulas

| Concept | Formula |
|---------|---------|
| Dot product | \`a·b = ax·bx + ay·by + az·bz\` |
| Cross product | \`a×b = (ay·bz-az·by, az·bx-ax·bz, ax·by-ay·bx)\` |
| Gradient | \`∇f = (∂f/∂x, ∂f/∂y)\` |
| Directional derivative | \`D_u f = ∇f · û\` |
| Laplacian | \`∇²f = ∂²f/∂x² + ∂²f/∂y²\` |
| Discriminant | \`D = fxx·fyy - fxy²\` |
| Polar area element | \`dA = r dr dθ\` |

## References

- [Calculus](https://www.stewartcalculus.com/) by James Stewart — Chapters 12–16 cover multivariable calculus
- [3Blue1Brown: Multivariable Calculus](https://www.3blue1brown.com/) — visual intuition for gradients and partial derivatives
- [Numerical Recipes in C](http://numerical.recipes/) — multidimensional integration and optimization
`;

const diffeqIntroductionContent = `
## Why Differential Equations?

Differential equations are the mathematical language of change. Any time a quantity evolves over time — populations, temperatures, voltages, positions — a differential equation describes it. They are the foundation of physics, engineering, biology, economics, and climate science.

- **Physics** -- Newton's laws, electromagnetism, quantum mechanics, fluid dynamics
- **Biology** -- population dynamics, epidemics, neuron firing, gene expression
- **Engineering** -- control systems, circuit design, structural mechanics
- **Finance** -- Black-Scholes option pricing model

## Numerical Methods

Most differential equations cannot be solved analytically. Instead, we approximate solutions numerically by taking small steps:

- **Euler's method** -- the simplest approach: one step along the tangent line
- **Runge-Kutta 4 (RK4)** -- the workhorse of scientific computing: 4 slope evaluations per step, dramatically more accurate

These two methods form the foundation for every numerical ODE solver in production use (SciPy, MATLAB, Julia's DifferentialEquations.jl).

## What You Will Learn

This course teaches differential equations through code. Every concept is implemented from scratch in Python:

1. **Numerical Methods** -- Euler step, Euler integration, RK4 step, RK4 integration
2. **First-Order Models** -- Exponential decay, logistic growth, Newton's cooling law
3. **Systems & Oscillations** -- Vector Euler, simple harmonic motion, damped oscillator, Lotka-Volterra predator-prey
4. **Applications** -- SIR epidemic model, finding equilibria, stability analysis, Van der Pol oscillator

No external libraries needed — just pure Python arithmetic. Everything runs in your browser.
`;

const diffeqWhatsNextContent = `
## Congratulations

You have completed all 15 lessons. You can now solve differential equations numerically — both scalar ODEs and systems — and analyze their qualitative behavior through equilibria and stability.

## What to Explore Next

- **SciPy** -- \`scipy.integrate.solve_ivp\` uses adaptive RK45 with error control. It handles stiff equations and automatic step-size selection far beyond what we built here.
- **Stiff equations** -- Some ODEs require implicit methods (like backward Euler or the Runge-Kutta implicit family). Stiffness arises in chemical kinetics, electronics, and more.
- **Partial differential equations (PDEs)** -- ODEs involve one independent variable (time). PDEs involve space and time: heat equation, wave equation, Navier-Stokes.
- **Chaos theory** -- The Lorenz system (\`dx/dt = σ(y-x)\`, etc.) shows how deterministic ODEs can produce unpredictable, chaotic behavior.
- **Bifurcation theory** -- How equilibria appear, disappear, or change stability as parameters vary.

## References

- [Differential Equations, Dynamical Systems, and an Introduction to Chaos](https://www.sciencedirect.com/book/9780123820105/) by Hirsch, Smale, Devaney
- [Nonlinear Dynamics and Chaos](https://www.stevenstrogatz.com/books/nonlinear-dynamics-and-chaos-with-applications-to-physics-biology-chemistry-and-engineering) by Steven Strogatz — the most readable introduction to the subject
- [SciPy ODE documentation](https://docs.scipy.org/doc/scipy/reference/integrate.html) -- production-grade solvers
`;

const wavesIntroductionContent = `
## Why Waves & Acoustics in JavaScript?

Waves are everywhere — sound, light, seismic activity, quantum probability amplitudes. Acoustics in particular is tangible: you can *hear* the results of your computations. JavaScript's Web Audio API lets you play tones, demonstrate beats, and sweep Doppler shifts directly in the browser.

- **Wave Fundamentals** — Period, speed, wavelength, and temperature-dependent sound speed
- **Intensity & Perception** — The inverse-square law, decibel scale, acoustic beats, and the Doppler effect
- **Standing Waves & Resonance** — String harmonics, open and closed pipe modes, and wave superposition
- **Room Acoustics** — Sabine's reverberation formula, sound power levels, and acoustic reflection coefficients

## Physics You Can Hear

Each lesson includes working Web Audio code so you can *hear* what you are computing — beats pulsing at the frequency you calculate, Doppler sweeps, the hollow timbre of odd harmonics, constructive and destructive interference. Physics becomes tangible when it makes sound.

## Prerequisites

Basic JavaScript (functions, \`Math\`, \`console.log\`). No prior acoustics knowledge required — every concept is introduced from first principles.
`;

const wavesWhatsNextContent = `
## Congratulations

You have completed all 15 lessons of Waves & Acoustics in JavaScript. You can now compute wave periods and speeds, sound wavelengths, temperature-dependent sound speed, inverse-square intensity, decibel levels, beat frequencies, Doppler-shifted frequencies, string and pipe resonances, superposed amplitudes, reverberation times, sound power levels, and acoustic reflection coefficients — all in JavaScript.

## What's Next

- **Classical Mechanics in C** — The companion physics course: velocity, free fall, Newton's laws, energy, oscillations, and gravitation
- **Music Programming** — Apply Web Audio directly: synthesise tones, build chord and scale generators, sequence melodies, and design LFO effects
- **Electromagnetism** — Coulomb's law, electric fields, Biot-Savart, Maxwell's equations — the physics of waves extended to light

## References

- [Kinsler & Frey — Fundamentals of Acoustics](https://www.wiley.com/en-us/Fundamentals+of+Acoustics%2C+4th+Edition-p-9780471847892) — comprehensive textbook
- [Web Audio API specification](https://webaudio.github.io/web-audio-api/) — the browser sound engine
- [Wolframalpha](https://www.wolframalpha.com/) — verify acoustic formulas interactively
`;

const classicalMechanicsIntroductionContent = `
## Why Classical Mechanics in C?

Classical mechanics is the foundation of all physics. Every other field — thermodynamics, electromagnetism, quantum mechanics — builds on Newton's laws and the concept of energy. This course implements each fundamental law as a C function and verifies it numerically.

- **Kinematics** — Velocity, free fall, and projectile motion: how objects move through space and time
- **Forces & Newton's Laws** — Acceleration, friction, and centripetal motion: the causes of motion
- **Energy & Momentum** — Kinetic and potential energy, elastic collisions, and power: the conserved quantities that make physics tractable
- **Oscillations & Gravitation** — Simple harmonic motion, spring and pendulum periods, torque, and Newton's law of universal gravitation

## The Computational Approach

Rather than solving equations by hand, you implement each formula in C and verify it against known results. This builds both physical intuition and numerical fluency — the same approach used in physics simulations, game engines, and spacecraft trajectory planning.

## Prerequisites

Basic C programming (variables, functions, loops, \`printf\`). The C course on this platform is good preparation. No prior physics knowledge required — every concept is introduced from first principles.
`;

const classicalMechanicsWhatsNextContent = `
## Congratulations

You have completed all 15 lessons of Classical Mechanics in C. You can now compute velocity, free-fall distance, projectile range, Newton's-law acceleration, friction and centripetal forces, kinetic and potential energy, elastic collision outcomes, power, SHM displacement, spring and pendulum periods, torque, and Newton's gravitational force — all from scratch in C.

## What to Explore Next

- **Waves & Acoustics in JavaScript** — The next physics course: apply SHM to travelling waves, interference, Doppler effect, and resonance — then hear the results with the Web Audio API
- **Circuits in C** — Ohm's law, RC transients, RLC oscillators: apply Newton's-style differential equations to electric circuits
- **Orbital Mechanics in Three.js** — Use RK4 to integrate Newton's gravitational law and render live planetary orbits
- **Calculus in C** — Numerical derivatives and integrals: the mathematical backbone behind every physics simulation

## Key Formulas

| Concept | Formula |
|---------|---------|
| Average velocity | \`v = Δx / Δt\` |
| Free fall | \`h = v₀t + ½gt²\` |
| Projectile range | \`R = v₀² sin(2θ) / g\` |
| Newton's 2nd law | \`a = F / m\` |
| Friction | \`f = μN\` |
| Centripetal | \`a = v² / r\` |
| Kinetic energy | \`KE = ½mv²\` |
| Potential energy | \`PE = mgh\` |
| Elastic collision | \`v₁' = ((m₁−m₂)v₁ + 2m₂v₂) / (m₁+m₂)\` |
| Power | \`P = W / t\` |
| SHM | \`x = A cos(ωt)\` |
| Spring period | \`T = 2π√(m/k)\` |
| Pendulum period | \`T = 2π√(L/g)\` |
| Torque | \`τ = Fr sin(θ)\` |
| Gravity | \`F = Gm₁m₂/r²\` |

## References

- [Feynman Lectures on Physics Vol. 1](https://www.feynmanlectures.caltech.edu/) — the most readable introduction to mechanics
- [Classical Mechanics — Taylor](https://uscibooks.aip.org/books/classical-mechanics/) — the standard undergraduate text
`;

const musicIntroductionContent = `
## Why Music Programming?

Sound is mathematics made audible. Every note is a frequency, every rhythm is a time interval, every chord is a set of ratios. This course teaches you to generate, manipulate, and schedule sound directly in your browser using the **Web Audio API** — no plugins, no libraries, just JavaScript and math.

- **Notes & Frequencies** — MIDI numbers, the equal-temperament formula, frequency conversion, note names, and scale construction
- **Chords & Harmony** — Interval ratios, chord voicings (major, minor, dom7, maj7), and transposition
- **Synthesis & Rhythm** — BPM and note durations, arpeggiation, and precise note scheduling with onset times
- **Effects & Timbre** — Decibels, gain conversion, LFOs for tremolo and vibrato, and 16-step drum patterns

## The Web Audio API

The browser's built-in audio engine lets you create oscillators, connect them through gain and filter nodes, and schedule events with sample-accurate timing. Every lesson's code runs live in an iframe — you'll hear what you write.

\`\`\`js
const ac = new AudioContext();
const osc = ac.createOscillator();
const gain = ac.createGain();
osc.frequency.value = 440; // A4
osc.connect(gain);
gain.connect(ac.destination);
osc.start();
osc.stop(ac.currentTime + 1);
\`\`\`

## Prerequisites

Basic JavaScript (functions, arrays, Math object). No music theory background required — every concept is introduced from first principles.
`;

const musicWhatsNextContent = `
## Congratulations

You have completed all 15 lessons of Music Programming. You can now convert between MIDI and frequencies, name notes, build scales and chords, transpose progressions, schedule melodies with precise timing, apply gain in decibels, modulate parameters with an LFO, and build a 16-step drum machine — all with the Web Audio API.

## What to Explore Next

- **Tone.js** — A higher-level audio framework built on the Web Audio API. Provides Transport, Sequencer, and a large library of synthesis and effects nodes
- **Web MIDI API** — Connect a real MIDI keyboard and route its input to your Web Audio synthesis code
- **AudioWorklet** — Run custom DSP code on the audio thread for custom oscillators, convolution reverb, and spectral processing
- **SuperCollider** — The server-client audio synthesis language used by electronic musicians worldwide

## Key Formulas

| Concept | Formula |
|---------|---------|
| MIDI to frequency | \`440 × 2^((n − 69) / 12)\` |
| Frequency to MIDI | \`round(69 + 12 × log₂(f / 440))\` |
| Interval ratio | \`2^(semitones / 12)\` |
| Beat duration | \`60 / bpm\` |
| Note duration | \`60 / bpm × (4 / noteType)\` |
| dB to gain | \`10^(dB / 20)\` |
| Gain to dB | \`20 × log₁₀(gain)\` |
| LFO | \`depth × sin(2π × rate × t)\` |

## References

- [MDN Web Audio API](https://developer.mozilla.org/en-US/docs/Web/API/Web_Audio_API) — The canonical reference
- [The Audio Programming Book](https://mitpress.mit.edu/9780262014465/) — Comprehensive DSP and synthesis theory
- [Music Theory for Computer Musicians](https://www.mikejohnsonmusic.net/) by Michael Johnson
`;

const quantumIntroductionContent = `
## Why Quantum Computing?

Classical computers represent information as bits — 0 or 1. Quantum computers use **qubits**, which can exist in superpositions of 0 and 1 simultaneously, and can become **entangled** in ways that have no classical analogue.

This gives quantum computers extraordinary power for specific problems:

- **Grover's algorithm** searches an unsorted database of N items in O(√N) steps — a quadratic speedup over classical O(N) search.
- **Shor's algorithm** factors large integers in polynomial time, breaking RSA encryption.
- **Quantum simulation** models molecules and materials exponentially faster than classical methods.
- **Quantum key distribution** enables provably secure communication, guaranteed by the laws of physics.

## The Story

The idea that quantum mechanics could enable fundamentally new kinds of computation was first articulated by Richard Feynman in 1982. In 1985, David Deutsch formalized the quantum Turing machine and published the first quantum algorithm. Peter Shor's 1994 factoring algorithm and Lov Grover's 1996 search algorithm demonstrated concrete, dramatic speedups and sparked the modern era of quantum computing research.

Today, IBM, Google, IonQ, and others operate real quantum hardware accessible over the cloud. Google claimed **quantum supremacy** in 2019 when their 53-qubit Sycamore processor completed a specific calculation in 200 seconds that would take a classical supercomputer approximately 10,000 years.

## How This Course Works

We simulate quantum systems in Python using the mathematical formalism directly — no special libraries, just vectors and arithmetic. This lets you see exactly what quantum gates do to quantum states.

A qubit is represented as a 2-element list \`[alpha, beta]\`. Gates are functions that transform these lists. Multi-qubit systems are 4-element (or larger) lists using the tensor product.

## What You Will Learn

This course contains **15 lessons** organized into **4 chapters**:

1. **Quantum Bits** -- Qubits, normalization, and measurement probability.
2. **Single-Qubit Gates** -- The Hadamard, X, and Z gates.
3. **Multi-Qubit Systems** -- Tensor products, CNOT, Bell states, and entanglement.
4. **Quantum Algorithms** -- The Deutsch algorithm, Grover's search, quantum error correction, and BB84 key distribution.

Let's get started.
`;

const quantumWhatsNextContent = `
## Congratulations

You have completed the Quantum Computing course. You can now simulate qubits, apply quantum gates, create entangled states, and implement foundational quantum algorithms from scratch.

## What to Explore Next

- **Shor's Algorithm** -- Factor integers using the quantum Fourier transform and period finding.
- **Quantum Phase Estimation** -- A subroutine used in many advanced algorithms including HHL for linear systems.
- **Variational Quantum Eigensolver (VQE)** -- A hybrid classical-quantum algorithm for chemistry simulation.
- **Real Quantum Hardware** -- Run circuits on IBM Quantum (free tier) using the Qiskit SDK.

## Frameworks and Libraries

- [Qiskit](https://qiskit.org/) -- IBM's open-source quantum SDK for Python. The most widely used.
- [Cirq](https://quantumai.google/cirq) -- Google's quantum computing framework.
- [PennyLane](https://pennylane.ai/) -- Differentiable quantum programming for quantum machine learning.
- [QuTiP](https://qutip.org/) -- Quantum Toolbox in Python for open quantum systems.

## Further Reading

- **Quantum Computation and Quantum Information** by Nielsen & Chuang -- the definitive textbook.
- **Quantum Computing: An Applied Approach** by Jack Hidary -- more accessible, code-focused.
- [IBM Quantum Learning](https://learning.quantum.ibm.com/) -- Free interactive courses with real hardware access.
`;

const genomicsIntroductionContent = `
## Why Genomics?

Every living cell contains a complete copy of the organism's **genome** — roughly 3 billion DNA base pairs in humans. That sequence is the source code of life: it encodes every protein, regulates when and where each gene turns on, and determines susceptibility to disease.

For most of history, biologists had to study genes one at a time. Today, sequencing machines can read an entire genome in hours. The bottleneck is no longer generating the data — it is **understanding** it.

Most of the genome does not code for proteins. The vast regulatory landscape — promoters, enhancers, silencers, splice signals — controls which genes are active in which cell. Single-letter changes in these regions can cause cancer, developmental disorders, or heart disease. But the functional rules encoded in DNA are far too complex to write by hand.

**AlphaGenome**, released by Google DeepMind in 2025, is an AI model that reads up to one million base pairs of DNA and simultaneously predicts thousands of molecular properties: gene expression levels across tissues, RNA splice patterns, chromatin accessibility, transcription factor binding, histone marks, and more. It achieves state-of-the-art accuracy on 25 of 26 variant effect benchmarks.

## How This Course Works

This course teaches you the molecular biology and bioinformatics concepts behind AlphaGenome by implementing them yourself in Python. No biology background required — just Python.

Each lesson builds on the previous one:
- First you will understand what DNA is and how it encodes information.
- Then you will learn how genes are read: transcription, translation, and splicing.
- Then you will explore gene regulation: how the genome controls which genes turn on.
- Finally you will see how all of these concepts map directly onto AlphaGenome's architecture.

By the final lesson, you will have implemented — in miniature — the core operation that AlphaGenome performs: **variant effect prediction**.

## What You Will Learn

This course contains **15 lessons** organized into **4 chapters**:

1. **The DNA Alphabet** -- Sequences, base complement, GC content, and codons.
2. **Reading the Genome** -- Open reading frames, transcription, translation, and splice sites.
3. **Gene Regulation** -- Motif finding, CpG islands, single nucleotide variants.
4. **Genomics Meets AI** -- One-hot encoding, k-mer features, regulatory scoring, and variant effect prediction.

Let's get started.
`;

const genomicsWhatsNextContent = `
## Congratulations

You have completed the Genomics course. You now understand the molecular biology behind AlphaGenome — DNA sequences, gene expression, splicing, regulatory elements, and variant effect prediction.

## What to Explore Next

- **Run AlphaGenome yourself** -- The model is available for non-commercial use via the Python SDK at [deepmind.google.com/science/alphagenome](https://deepmind.google.com/science/alphagenome).
- **Enformer** -- DeepMind's earlier genomics model (2021), which AlphaGenome improves upon. The original paper is a good read.
- **Bioinformatics with Biopython** -- The \`biopython\` library implements professional versions of everything in this course.
- **ENCODE Project** -- The experimental dataset that underpins AlphaGenome's training data.
- **GTEx** -- Gene expression across human tissues; another key AlphaGenome training resource.

## Tools and Libraries

- [Biopython](https://biopython.org/) -- The standard Python library for bioinformatics.
- [pysam](https://pysam.readthedocs.io/) -- Reading genome alignment files.
- [pyBigWig](https://github.com/deeptools/pyBigWig) -- Reading genomic signal tracks.
- [Kipooi](https://kipoi.org/) -- A model zoo for genomics ML models.

## Further Reading

- **Molecular Biology of the Gene** by Watson et al. -- The canonical textbook.
- **Bioinformatics Algorithms** by Compeau & Pevzner -- Algorithmic approach to genomics.
- [AlphaGenome paper on Nature](https://www.nature.com/articles/s41586-025-10014-0) -- The original publication.
`;

const microgptIntroductionContent = `
## Why Build a GPT From Scratch?

Large language models like GPT-4 feel like magic — until you build one yourself. Then they feel like math.

This course takes you through the complete pipeline for building a miniature GPT in **pure Python**, with no ML frameworks. You will implement every component from scratch: the automatic differentiation engine, the tokenizer, the linear layer, softmax, RMS normalization, cross-entropy loss, scaled dot-product attention, multi-head attention, the training loop, and the Adam optimizer.

The result is a small model that learns to generate names character by character. It is the same architecture as GPT-2, just smaller.

## What is Autograd?

Most ML code hides the calculus. You call \`loss.backward()\` and gradients appear. This course shows you exactly how that works.

You will build a \`Value\` class that tracks every arithmetic operation in a computation graph. When you call \`backward()\`, it walks the graph in reverse topological order and applies the chain rule at each node — the same algorithm used by PyTorch and JAX under the hood.

## How This Course Works

Each lesson introduces one concept, provides a short explanation, and asks you to implement a single function. The starter code includes all the infrastructure you need — you only implement the function described.

By the final lesson, you will have implemented every component needed to train a transformer language model.

## What You Will Learn

This course contains **15 lessons** organized into **5 chapters**:

1. **Autograd Engine** -- Value class, arithmetic operations, backpropagation.
2. **Data & Tokens** -- Tokenizer, character-level vocabulary, training pairs.
3. **Neural Net Primitives** -- Linear layer, softmax, RMS normalization, cross-entropy loss.
4. **The Transformer** -- Single-head attention, multi-head attention.
5. **Training** -- SGD training loop, Adam optimizer.

Let's start.
`;

const microgptWhatsNextContent = `
## Congratulations

You have built a GPT from scratch. You now understand every component of a transformer language model: the autograd engine, tokenizer, linear layers, attention, and the Adam optimizer.

## What to Explore Next

- **Andrej Karpathy's micrograd** -- The autograd engine in this course is inspired by [micrograd](https://github.com/karpathy/micrograd). The original ~100-line implementation is worth reading.
- **Andrej Karpathy's makemore** -- A character-level language model built step by step, leading up to a full GPT. [The YouTube series](https://www.youtube.com/playlist?list=PLAqhIrjkxbuWI23v9cThsA9GvCAUhRvKZ) is excellent.
- **nanoGPT** -- A clean, minimal GPT-2 implementation in PyTorch by Karpathy. Around 300 lines of model code.
- **Attention Is All You Need** -- The 2017 paper that introduced the transformer architecture. Short and readable.
- **The Illustrated Transformer** -- Jay Alammar's visual walkthrough of the transformer. The best introduction for visual learners.

## Tools and Libraries

- [PyTorch](https://pytorch.org/) -- The standard deep learning framework. Now that you understand what \`autograd\` does, PyTorch will feel familiar.
- [Hugging Face Transformers](https://huggingface.co/docs/transformers/) -- Pre-trained models including GPT-2.
- [tiktoken](https://github.com/openai/tiktoken) -- OpenAI's fast BPE tokenizer (what GPT-4 uses).

## Further Reading

- **Deep Learning** by Goodfellow, Bengio & Courville -- The standard textbook.
- **The Little Book of Deep Learning** by François Fleuret -- A concise, free introduction.
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
    id: "sqlite",
    title: "SQLite",
    description: "Learn SQLite from scratch. Master queries, schema design, CRUD operations, window functions, and JSON functions in the world's most deployed database.",
    language: "sqlite",
    chapters: sqliteChapters,
    lessons: sqliteLessons,
    runtimeLabel: "SQLite runtime",
    introductionContent: sqliteIntroductionContent,
    whatsNextContent: sqliteWhatsNextContent,
  },
  {
    id: "redis",
    title: "Redis",
    description: "Learn Redis from scratch. Master strings, lists, sets, hashes, sorted sets, transactions, and caching patterns — all in a live Redis emulator.",
    language: "redis",
    chapters: redisChapters,
    lessons: redisLessons,
    runtimeLabel: "Redis emulator",
    introductionContent: redisIntroductionContent,
    whatsNextContent: redisWhatsNextContent,
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
    id: "cpp",
    title: "C++",
    description: "Learn C++ from scratch. Master object-oriented programming with classes, inheritance, virtual functions, and templates — compiled and run in your browser.",
    language: "cpp",
    chapters: cppChapters,
    lessons: cppLessons,
    runtimeLabel: "JSCPP",
    introductionContent: cppIntroductionContent,
    whatsNextContent: cppWhatsNextContent,
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
  {
    id: "coreutils",
    title: "Coreutils in C",
    description: "Reimplement the classic Unix command-line tools in C. Build echo, cat, grep, wc, head, tail, tr, uniq, and more from scratch — and understand how they work.",
    language: "c",
    chapters: coreutilsChapters,
    lessons: coreutilsLessons,
    runtimeLabel: "TCC compiler",
    introductionContent: coreutilsIntroductionContent,
    whatsNextContent: coreutilsWhatsNextContent,
  },
  {
    id: "javascript",
    title: "JavaScript",
    description: "Learn JavaScript from scratch. Master variables, functions, closures, arrays, and objects — the foundation of the web's most popular language.",
    language: "javascript",
    chapters: jsChapters,
    lessons: jsLessons,
    runtimeLabel: "JavaScript",
    introductionContent: jsIntroductionContent,
    whatsNextContent: jsWhatsNextContent,
  },
  {
    id: "typescript",
    title: "TypeScript",
    description: "Learn TypeScript from scratch. Master type annotations, interfaces, generics, and the type system that makes large-scale JavaScript development reliable.",
    language: "typescript",
    chapters: tsChapters,
    lessons: tsLessons,
    runtimeLabel: "TypeScript",
    introductionContent: tsIntroductionContent,
    whatsNextContent: tsWhatsNextContent,
  },
  {
    id: "linux",
    title: "Linux",
    description: "Learn Linux and the Bash shell from scratch. Master the command line: navigate the filesystem, manage files, process text with pipes, and write shell scripts.",
    language: "bash",
    chapters: linuxChapters,
    lessons: linuxLessons,
    runtimeLabel: "Linux shell",
    introductionContent: linuxIntroductionContent,
    whatsNextContent: linuxWhatsNextContent,
  },
  {
    id: "algorithms",
    title: "Algorithms",
    description: "Learn essential algorithms from scratch. Master sorting, searching, graph traversal, and dynamic programming — the foundations of every technical interview and production system.",
    language: "javascript",
    chapters: algorithmsChapters,
    lessons: algorithmsLessons,
    runtimeLabel: "JavaScript",
    introductionContent: algorithmsIntroductionContent,
    whatsNextContent: algorithmsWhatsNextContent,
  },
  {
    id: "distributed-systems",
    title: "Distributed Systems",
    description: "Learn distributed systems by implementing them. Build Lamport clocks, consistent hashing, quorum replication, CRDTs, Bloom filters, circuit breakers, and more — in JavaScript.",
    language: "javascript",
    chapters: distributedSystemsChapters,
    lessons: distributedSystemsLessons,
    runtimeLabel: "JavaScript",
    introductionContent: distributedSystemsIntroductionContent,
    whatsNextContent: distributedSystemsWhatsNextContent,
  },
  {
    id: "ruby",
    title: "Ruby",
    description: "Learn Ruby from scratch. Master variables, strings, arrays, hashes, iterators, classes, and modules — the expressive language behind Rails, GitHub, and Shopify.",
    language: "ruby",
    chapters: rubyChapters,
    lessons: rubyLessons,
    runtimeLabel: "ruby.wasm",
    introductionContent: rubyIntroductionContent,
    whatsNextContent: rubyWhatsNextContent,
  },
  {
    id: "trees",
    title: "Trees in C",
    description: "Learn tree data structures by implementing them in C. Build binary trees, master inorder/preorder/postorder traversals, and implement a Binary Search Tree from scratch.",
    language: "c",
    chapters: treesChapters,
    lessons: treesLessons,
    runtimeLabel: "TCC compiler",
    introductionContent: treesIntroductionContent,
    whatsNextContent: treesWhatsNextContent,
  },
  {
    id: "kernel",
    title: "Linux Internals",
    description: "Understand how the Linux kernel works by implementing its core data structures in C. Build a process scheduler, page table, slab allocator, ring buffer, semaphore, signal table, and syscall dispatcher.",
    language: "c",
    chapters: kernelChapters,
    lessons: kernelLessons,
    runtimeLabel: "TCC compiler",
    introductionContent: kernelIntroductionContent,
    whatsNextContent: kernelWhatsNextContent,
  },
  {
    id: "threejs",
    title: "Three.js",
    description: "Learn 3D graphics in the browser with Three.js. Build scenes, add lights, animate objects, and render a full solar system — all running live in your browser.",
    language: "javascript",
    chapters: threejsChapters,
    lessons: threejsLessons,
    runtimeLabel: "Three.js",
    introductionContent: threejsIntroductionContent,
    whatsNextContent: threejsWhatsNextContent,
  },
  {
    id: "linked-lists",
    title: "Linked Lists in C",
    description: "Master linked lists by implementing them from scratch in C. Build nodes with malloc, traverse and mutate lists, and solve classic interview problems: reversal, nth-from-end, and merging sorted lists.",
    language: "c",
    chapters: linkedListsChapters,
    lessons: linkedListsLessons,
    runtimeLabel: "TCC compiler",
    introductionContent: linkedListsIntroductionContent,
    whatsNextContent: linkedListsWhatsNextContent,
  },
  {
    id: "python",
    title: "Python",
    description: "Learn Python from the ground up. Master strings, lists, dictionaries, OOP, generators, and the standard library — the language powering AI, data science, and web development.",
    language: "python",
    chapters: pythonChapters,
    lessons: pythonLessons,
    runtimeLabel: "Pyodide",
    introductionContent: pythonIntroductionContent,
    whatsNextContent: pythonWhatsNextContent,
  },
  {
    id: "haskell",
    title: "Haskell",
    description: "Learn Haskell from the ground up. Master pure functions, pattern matching, list comprehensions, folds, lambdas, and the Maybe type — the language that makes functional programming beautiful.",
    language: "haskell",
    chapters: haskellChapters,
    lessons: haskellLessons,
    runtimeLabel: "Haskell",
    introductionContent: haskellIntroductionContent,
    whatsNextContent: haskellWhatsNextContent,
  },
  {
    id: "graphs",
    title: "Graphs in Python",
    description: "Master graph algorithms in Python. Implement BFS, DFS, Dijkstra, Bellman-Ford, topological sort, Union-Find, Kruskal's MST, and PageRank from scratch.",
    language: "python",
    chapters: graphsChapters,
    lessons: graphsLessons,
    runtimeLabel: "Pyodide",
    introductionContent: graphsIntroductionContent,
    whatsNextContent: graphsWhatsNextContent,
  },
  {
    id: "linear-algebra",
    title: "Linear Algebra in Python",
    description: "Master linear algebra with NumPy and SymPy. Build vectors and matrices, solve linear systems, compute eigenvalues, fit lines with least squares, and render beautiful equations with SymPy's pprint.",
    language: "python",
    chapters: linearAlgebraChapters,
    lessons: linearAlgebraLessons,
    runtimeLabel: "Pyodide",
    introductionContent: linearAlgebraIntroductionContent,
    whatsNextContent: linearAlgebraWhatsNextContent,
  },
  {
    id: "calculus",
    title: "Calculus 1 in C",
    description: "Learn Calculus 1 by implementing the algorithms in C. Compute limits, derivatives, and integrals numerically — Euler's method, Newton's method, Riemann sums, Simpson's rule, and volumes of revolution.",
    language: "c",
    chapters: calculusChapters,
    lessons: calculusLessons,
    runtimeLabel: "TCC compiler",
    introductionContent: calculusIntroductionContent,
    whatsNextContent: calculusWhatsNextContent,
  },
  {
    id: "diffeq",
    title: "Differential Equations in Python",
    description: "Solve differential equations numerically in Python. Implement Euler's method and RK4, model exponential decay, logistic growth, oscillations, epidemics, and predator-prey dynamics from scratch.",
    language: "python",
    chapters: diffeqChapters,
    lessons: diffeqLessons,
    runtimeLabel: "Pyodide",
    introductionContent: diffeqIntroductionContent,
    whatsNextContent: diffeqWhatsNextContent,
  },
  {
    id: "statistics",
    title: "Statistics in Python",
    description: "Master statistics with Python. Compute descriptive stats, model distributions, run t-tests and chi-square tests, build confidence intervals, fit regression lines, and estimate uncertainty with bootstrap resampling.",
    language: "python",
    chapters: statisticsChapters,
    lessons: statisticsLessons,
    runtimeLabel: "Pyodide",
    introductionContent: statisticsIntroductionContent,
    whatsNextContent: statisticsWhatsNextContent,
  },
  {
    id: "calculus2",
    title: "Calculus 2 in C",
    description: "Learn Calculus 2 by implementing the algorithms in C. Compute arc lengths, surface areas, improper integrals, partial sums, geometric series, Taylor polynomials, parametric arc length, polar area, and curvature.",
    language: "c",
    chapters: calculus2Chapters,
    lessons: calculus2Lessons,
    runtimeLabel: "TCC compiler",
    introductionContent: calculus2IntroductionContent,
    whatsNextContent: calculus2WhatsNextContent,
  },
  {
    id: "circuits",
    title: "Circuits in C",
    description: "Learn analog circuit analysis by implementing the algorithms in C. From Ohm's law to Wheatstone bridges, RC transients, RLC oscillators, and frequency-domain filter design.",
    language: "c",
    chapters: circuitsChapters,
    lessons: circuitsLessons,
    runtimeLabel: "TCC compiler",
    introductionContent: circuitsIntroductionContent,
    whatsNextContent: circuitsWhatsNextContent,
  },
  {
    id: "calculus3",
    title: "Calculus 3 in C",
    description: "Learn Multivariable Calculus by implementing the algorithms in C. Compute dot and cross products, partial derivatives, gradients, tangent planes, the Laplacian, double integrals, polar integrals, and triple integrals.",
    language: "c",
    chapters: calculus3Chapters,
    lessons: calculus3Lessons,
    runtimeLabel: "TCC compiler",
    introductionContent: calculus3IntroductionContent,
    whatsNextContent: calculus3WhatsNextContent,
  },
  {
    id: "music",
    title: "Music Programming",
    description: "Learn music programming with the Web Audio API. Generate tones, build chords and scales, schedule melodies with precise timing, apply LFOs and gain, and build a drum machine — all in JavaScript.",
    language: "javascript",
    chapters: musicChapters,
    lessons: musicLessons,
    runtimeLabel: "Web Audio API",
    introductionContent: musicIntroductionContent,
    whatsNextContent: musicWhatsNextContent,
  },
  {
    id: "waves",
    title: "Waves & Acoustics",
    description: "Learn wave physics and acoustics in JavaScript. Compute wave periods, Doppler shifts, standing wave harmonics, reverberation times, and acoustic reflection — and hear your results through the Web Audio API.",
    language: "javascript",
    chapters: wavesChapters,
    lessons: wavesLessons,
    runtimeLabel: "Web Audio API",
    introductionContent: wavesIntroductionContent,
    whatsNextContent: wavesWhatsNextContent,
  },
  {
    id: "classical-mechanics",
    title: "Classical Mechanics in C",
    description: "Learn classical mechanics by implementing the algorithms in C. Compute velocity, free fall, projectile range, Newton's laws, friction, energy, momentum, oscillations, and gravitational force from scratch.",
    language: "c",
    chapters: classicalMechanicsChapters,
    lessons: classicalMechanicsLessons,
    runtimeLabel: "TCC compiler",
    introductionContent: classicalMechanicsIntroductionContent,
    whatsNextContent: classicalMechanicsWhatsNextContent,
  },
  {
    id: "quantum",
    title: "Quantum Computing",
    description: "Learn quantum computing from first principles. Simulate qubits, apply quantum gates, create entangled Bell states, and implement Grover's search, the Deutsch algorithm, and the BB84 quantum key distribution protocol — all in Python.",
    language: "python",
    chapters: quantumChapters,
    lessons: quantumLessons,
    runtimeLabel: "Python runtime",
    introductionContent: quantumIntroductionContent,
    whatsNextContent: quantumWhatsNextContent,
  },
  {
    id: "genomics",
    title: "Genomics",
    description: "Learn computational genomics from first principles. Implement DNA complement, GC content, ORF finding, transcription, translation, splice site detection, motif search, CpG islands, one-hot encoding, and variant effect prediction — the exact concepts behind Google DeepMind's AlphaGenome.",
    language: "python",
    chapters: genomicsChapters,
    lessons: genomicsLessons,
    runtimeLabel: "Python runtime",
    introductionContent: genomicsIntroductionContent,
    whatsNextContent: genomicsWhatsNextContent,
  },
  {
    id: "microgpt",
    title: "MicroGPT",
    description: "Build a GPT from scratch in pure Python. Implement autograd, tokenization, attention, and train a transformer language model — no ML frameworks required.",
    language: "python",
    chapters: microgptChapters,
    lessons: microgptLessons,
    runtimeLabel: "Python runtime",
    introductionContent: microgptIntroductionContent,
    whatsNextContent: microgptWhatsNextContent,
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
