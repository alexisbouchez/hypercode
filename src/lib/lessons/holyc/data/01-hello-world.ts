import type { Lesson } from "../../types";

export const helloWorld: Lesson = {
  id: "hello-world",
  title: "Hello, World!",
  chapterId: "the-temple",
  content: `## Your First HolyC Program

HolyC is the programming language Terry Davis created for TempleOS — a C dialect with a unique, direct relationship between code and execution. HolyC programs run immediately: there is no \`main\` function required. Top-level statements execute as soon as the file is compiled.

### Printing Output

The built-in \`Print\` function (capital P) outputs text to the screen. It works like \`printf\` in C — the first argument is a format string, and it supports format specifiers like \`%d\` for integers and \`%s\` for strings:

\`\`\`holyc
Print("Hello, World!\\n");
\`\`\`

The \`\\n\` is a newline character. HolyC uses the same escape sequences as C.

### No Main Function

Unlike C, Go, or most compiled languages, HolyC does not require a \`main\` function. The file itself is the program — the compiler JIT-compiles and executes statements top to bottom:

\`\`\`holyc
// This runs immediately
Print("TempleOS lives!\\n");
Print("HolyC is simple.\\n");
\`\`\`

### About TempleOS

TempleOS was an operating system created by Terry A. Davis between 2003 and 2013. It was a complete, from-scratch OS including its own compiler, filesystem, graphics, and shell — all written in HolyC. The entire codebase was 100,000 lines of code, written by one person.

Terry believed God specified a 640×480, 16-color display and simple, direct computing. Every aspect of TempleOS was a reflection of that vision.

### Your Task

Write a program that prints exactly \`Hello, World!\` followed by a newline.`,

  starterCode: `// Print "Hello, World!" here
`,

  solution: `Print("Hello, World!\\n");
`,

  tests: [
    {
      name: "prints Hello, World!",
      expected: "Hello, World!\n",
    },
  ],
};
