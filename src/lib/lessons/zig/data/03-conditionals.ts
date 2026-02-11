import type { Lesson } from "../../types";

export const conditionals: Lesson = {
  id: "conditionals",
  title: "Conditionals",
  chapterId: "control-flow",
  content: `## Making Decisions in Zig

### If / Else

Zig's \`if\` statement is straightforward. No parentheses around the condition, and braces are mandatory:

\`\`\`zig
if (x > 10) {
    std.debug.print("big\\n", .{});
} else if (x > 5) {
    std.debug.print("medium\\n", .{});
} else {
    std.debug.print("small\\n", .{});
}
\`\`\`

Note that Zig *does* require parentheses around the condition, unlike Go. This is one area where Zig follows C tradition.

### If as an Expression

One of Zig's powerful features is that \`if\` can be used as an expression, not just a statement. This means \`if\` can return a value:

\`\`\`zig
const label = if (x > 10) "big" else "small";
\`\`\`

This is similar to the ternary operator in C (\`? :\`), but more readable. The \`else\` branch is mandatory when using \`if\` as an expression.

### If with Optionals

Zig has a special \`if\` syntax for unwrapping optional values:

\`\`\`zig
const maybe_value: ?i32 = 42;
if (maybe_value) |value| {
    std.debug.print("got: {d}\\n", .{value});
} else {
    std.debug.print("no value\\n", .{});
}
\`\`\`

The \`|value|\` syntax captures the unwrapped non-null value. This eliminates null pointer errors at compile time.

### Switch

Zig's \`switch\` is an expression, which means it always produces a value. It is exhaustive: you must handle every possible case or provide an \`else\` branch:

\`\`\`zig
const result = switch (day) {
    .monday => "start of the week",
    .friday => "almost weekend",
    else => "regular day",
};
\`\`\`

Switch on integers:

\`\`\`zig
const label = switch (score) {
    0...59 => "fail",
    60...79 => "pass",
    80...100 => "excellent",
    else => "invalid",
};
\`\`\`

The \`...\` syntax defines an inclusive range. This is unique to Zig and makes range matching concise and clear.

### Switch vs If-Else Chains

Use \`switch\` when you have multiple discrete values or ranges to match against. Use \`if-else\` chains when conditions are complex expressions that do not map cleanly to a single value.

### Your Task

Write a function \`classifyTemp\` that takes an \`i32\` temperature in Celsius and returns a \`[]const u8\` string:
- \`"freezing"\` if temp <= 0
- \`"cold"\` if temp <= 15
- \`"warm"\` if temp <= 30
- \`"hot"\` if temp > 30`,

  starterCode: `const std = @import("std");

pub fn classifyTemp(temp: i32) []const u8 {
\t// Your code here
\treturn "";
}

pub fn main() void {
\tstd.debug.print("{s}\\n", .{classifyTemp(-5)});
\tstd.debug.print("{s}\\n", .{classifyTemp(10)});
\tstd.debug.print("{s}\\n", .{classifyTemp(25)});
\tstd.debug.print("{s}\\n", .{classifyTemp(35)});
}
`,

  solution: `const std = @import("std");

pub fn classifyTemp(temp: i32) []const u8 {
\tif (temp <= 0) {
\t\treturn "freezing";
\t} else if (temp <= 15) {
\t\treturn "cold";
\t} else if (temp <= 30) {
\t\treturn "warm";
\t} else {
\t\treturn "hot";
\t}
}

pub fn main() void {
\tstd.debug.print("{s}\\n", .{classifyTemp(-5)});
\tstd.debug.print("{s}\\n", .{classifyTemp(10)});
\tstd.debug.print("{s}\\n", .{classifyTemp(25)});
\tstd.debug.print("{s}\\n", .{classifyTemp(35)});
}
`,

  tests: [
    {
      name: "freezing (-5)",
      code: `const std = @import("std");

{{FUNC}}

pub fn main() void {
\tstd.debug.print("{s}\\n", .{classifyTemp(-5)});
}`,
      expected: "freezing\n",
    },
    {
      name: "cold (10)",
      code: `const std = @import("std");

{{FUNC}}

pub fn main() void {
\tstd.debug.print("{s}\\n", .{classifyTemp(10)});
}`,
      expected: "cold\n",
    },
    {
      name: "warm (25)",
      code: `const std = @import("std");

{{FUNC}}

pub fn main() void {
\tstd.debug.print("{s}\\n", .{classifyTemp(25)});
}`,
      expected: "warm\n",
    },
    {
      name: "hot (35)",
      code: `const std = @import("std");

{{FUNC}}

pub fn main() void {
\tstd.debug.print("{s}\\n", .{classifyTemp(35)});
}`,
      expected: "hot\n",
    },
    {
      name: "edge case: exactly 0",
      code: `const std = @import("std");

{{FUNC}}

pub fn main() void {
\tstd.debug.print("{s}\\n", .{classifyTemp(0)});
}`,
      expected: "freezing\n",
    },
  ],
};
