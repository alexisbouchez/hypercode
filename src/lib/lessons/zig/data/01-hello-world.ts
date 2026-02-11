import type { Lesson } from "../../types";

export const helloWorld: Lesson = {
  id: "hello-world",
  title: "Hello, World!",
  chapterId: "foundations",
  content: `## The Anatomy of a Zig Program

Every Zig source file begins with imports. Unlike C, there is no preprocessor. Instead, Zig uses a built-in function called \`@import\` to bring in other modules and the standard library.

\`\`\`zig
const std = @import("std");
\`\`\`

This single line gives you access to the entire standard library through the \`std\` namespace. The \`const\` keyword means \`std\` cannot be reassigned. In Zig, everything is explicit and there are no hidden allocations or control flow.

### The Entry Point

The entry point of a Zig program is the \`pub fn main\` function. The \`pub\` keyword makes it public, meaning the runtime can find and call it:

\`\`\`zig
pub fn main() void {
    std.debug.print("Hello, World!\\n", .{});
}
\`\`\`

The return type \`void\` means the function returns nothing. If your \`main\` function needs to signal errors, you can use \`!void\` instead, which we will cover in a later lesson.

### Printing Output

Zig does not have a \`println\` function like Go or Python. Instead, you use \`std.debug.print\`, which takes a format string and a tuple of arguments:

\`\`\`zig
std.debug.print("Hello, {s}!\\n", .{"World"});
\`\`\`

The \`.{}\` syntax creates an anonymous tuple. When you have no format arguments, you pass an empty tuple: \`.{}\`.

The \`\\n\` at the end of the format string adds a newline character. Unlike \`fmt.Println\` in Go, \`std.debug.print\` does not automatically add a newline, so you must include one yourself.

### Format Specifiers

Zig's format strings use curly braces \`{}\` as placeholders:

| Specifier | Description |
|-----------|-------------|
| \`{}\` | Default formatting |
| \`{s}\` | String |
| \`{d}\` | Integer |
| \`{any}\` | Debug formatting for any type |

### No Hidden Control Flow

One of Zig's core design principles is "no hidden control flow." There are no hidden function calls, no operator overloading, and no implicit type conversions. What you see in the code is exactly what the machine does.

### Your Task

Write a program that prints exactly \`Hello, World!\` followed by a newline to the output.`,

  starterCode: `const std = @import("std");

pub fn main() void {
\t// Write your first Zig program here
}
`,

  solution: `const std = @import("std");

pub fn main() void {
\tstd.debug.print("Hello, World!\\n", .{});
}
`,

  tests: [
    {
      name: "prints Hello, World!",
      expected: "Hello, World!\n",
    },
  ],
};
