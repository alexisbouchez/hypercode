import type { Lesson } from "../../types";

export const loops: Lesson = {
  id: "loops",
  title: "Loops",
  chapterId: "control-flow",
  content: `## Looping in Zig

Zig has two loop constructs: \`while\` and \`for\`. There is no C-style three-component \`for\` loop. Instead, Zig's loops are minimal and explicit.

### While Loops

The \`while\` loop executes as long as its condition is true:

\`\`\`zig
var i: u32 = 0;
while (i < 5) {
    std.debug.print("{d}\\n", .{i});
    i += 1;
}
\`\`\`

### While with Continue Expression

Zig's \`while\` has an optional *continue expression* that runs at the end of each iteration, similar to the post-statement in a C \`for\` loop:

\`\`\`zig
var i: u32 = 0;
while (i < 5) : (i += 1) {
    std.debug.print("{d}\\n", .{i});
}
\`\`\`

This is the idiomatic way to write a counting loop in Zig. The continue expression \`(i += 1)\` runs after each iteration, including when \`continue\` is used.

### For Loops

Zig's \`for\` loop iterates over slices and arrays. It does not count. If you want to count, use \`while\`:

\`\`\`zig
const names = [_][]const u8{ "Alice", "Bob", "Charlie" };
for (names) |name| {
    std.debug.print("{s}\\n", .{name});
}
\`\`\`

You can also get the index:

\`\`\`zig
for (names, 0..) |name, i| {
    std.debug.print("{d}: {s}\\n", .{ i, name });
}
\`\`\`

### Break and Continue

\`break\` exits the loop immediately. \`continue\` skips to the next iteration:

\`\`\`zig
var i: u32 = 0;
while (i < 10) : (i += 1) {
    if (i == 5) break;
    if (i % 2 == 0) continue;
    std.debug.print("{d}\\n", .{i}); // prints 1, 3
}
\`\`\`

### Loops as Expressions

Like \`if\`, Zig loops can be expressions. A \`for\` or \`while\` loop with an \`else\` branch returns a value:

\`\`\`zig
const target: u32 = 7;
var i: u32 = 0;
const result = while (i < 10) : (i += 1) {
    if (i == target) break i;
} else 0; // default if loop completes without breaking
\`\`\`

### Ranges with \`0..n\`

You can iterate over a range by creating a range from a slice or using \`0..n\` syntax with \`for\`:

\`\`\`zig
for (0..5) |i| {
    std.debug.print("{d}\\n", .{i}); // prints 0 through 4
}
\`\`\`

### Your Task

Write a function \`fizzBuzz\` that takes a \`u32\` parameter \`n\` and prints the numbers from 1 to \`n\` (inclusive), one per line, with these substitutions:
- Print \`"FizzBuzz"\` if the number is divisible by both 3 and 5
- Print \`"Fizz"\` if the number is divisible by 3
- Print \`"Buzz"\` if the number is divisible by 5
- Print the number otherwise`,

  starterCode: `const std = @import("std");

pub fn fizzBuzz(n: u32) void {
\t// Your code here
}

pub fn main() void {
\tfizzBuzz(15);
}
`,

  solution: `const std = @import("std");

pub fn fizzBuzz(n: u32) void {
\tvar i: u32 = 1;
\twhile (i <= n) : (i += 1) {
\t\tif (i % 15 == 0) {
\t\t\tstd.debug.print("FizzBuzz\\n", .{});
\t\t} else if (i % 3 == 0) {
\t\t\tstd.debug.print("Fizz\\n", .{});
\t\t} else if (i % 5 == 0) {
\t\t\tstd.debug.print("Buzz\\n", .{});
\t\t} else {
\t\t\tstd.debug.print("{d}\\n", .{i});
\t\t}
\t}
}

pub fn main() void {
\tfizzBuzz(15);
}
`,

  tests: [
    {
      name: "fizzBuzz(15)",
      code: `const std = @import("std");

{{FUNC}}

pub fn main() void {
\tfizzBuzz(15);
}`,
      expected:
        "1\n2\nFizz\n4\nBuzz\nFizz\n7\n8\nFizz\nBuzz\n11\nFizz\n13\n14\nFizzBuzz\n",
    },
    {
      name: "fizzBuzz(5)",
      code: `const std = @import("std");

{{FUNC}}

pub fn main() void {
\tfizzBuzz(5);
}`,
      expected: "1\n2\nFizz\n4\nBuzz\n",
    },
    {
      name: "fizzBuzz(1)",
      code: `const std = @import("std");

{{FUNC}}

pub fn main() void {
\tfizzBuzz(1);
}`,
      expected: "1\n",
    },
    {
      name: "fizzBuzz(3)",
      code: `const std = @import("std");

{{FUNC}}

pub fn main() void {
\tfizzBuzz(3);
}`,
      expected: "1\n2\nFizz\n",
    },
  ],
};
