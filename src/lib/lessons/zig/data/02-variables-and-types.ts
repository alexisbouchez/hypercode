import type { Lesson } from "../../types";

export const variablesAndTypes: Lesson = {
  id: "variables-and-types",
  title: "Variables & Types",
  chapterId: "foundations",
  content: `## Variables in Zig

Zig has two ways to declare bindings: \`const\` and \`var\`. The distinction is simple and enforced by the compiler.

### Constants with \`const\`

A \`const\` binding cannot be changed after initialization. If you do not need to mutate a value, always use \`const\`. The compiler will warn you if you use \`var\` when \`const\` would suffice.

\`\`\`zig
const x: i32 = 42;
const name = "Zig"; // type inferred as *const [3:0]u8
\`\`\`

### Variables with \`var\`

A \`var\` binding can be reassigned:

\`\`\`zig
var count: u32 = 0;
count += 1;
\`\`\`

### Type Annotations and Inference

You can explicitly annotate the type, or let the compiler infer it:

\`\`\`zig
const explicit: i32 = 10;  // explicitly typed
const inferred = @as(i32, 10); // @as for explicit coercion
\`\`\`

### Basic Types

Zig has a rich set of primitive types. Unlike C, the sizes are explicit in the type name:

| Type | Description |
|------|-------------|
| \`u8\`, \`u16\`, \`u32\`, \`u64\` | Unsigned integers |
| \`i8\`, \`i16\`, \`i32\`, \`i64\` | Signed integers |
| \`f32\`, \`f64\` | Floating-point numbers |
| \`bool\` | Boolean (\`true\` or \`false\`) |
| \`usize\` | Pointer-sized unsigned integer |
| \`comptime_int\` | Compile-time known integer (no fixed size) |

The naming convention is clear: \`i\` for signed, \`u\` for unsigned, followed by the bit width. No ambiguity about sizes, ever.

> As Spock would say: "Fascinating." Zig's type precision is entirely logical --- no room for ambiguity when the bit width is right there in the name.

### Integer Literals

Zig supports several literal formats:

\`\`\`zig
const decimal = 42;
const hex = 0xFF;
const octal = 0o77;
const binary = 0b1010;
const with_separators = 1_000_000; // underscores for readability
\`\`\`

### Type Coercion

Zig does not perform implicit type widening. If you have a \`u8\` and need a \`u32\`, you must be explicit:

\`\`\`zig
const small: u8 = 10;
const big: u32 = small; // OK: safe widening from u8 to u32
\`\`\`

Widening to a larger type is allowed implicitly because no data can be lost. Narrowing requires an explicit \`@intCast\`.

### Undefined

In Zig, you can declare a variable without initializing it by assigning \`undefined\`:

\`\`\`zig
var buffer: [256]u8 = undefined;
\`\`\`

This is explicitly opting into uninitialized memory. Zig never hides dangerous behavior behind defaults.

### Your Task

Write a function \`describe\` that takes three parameters: a \`name\` of type \`[]const u8\`, a \`year\` of type \`u16\`, and a \`typed\` boolean of type \`bool\`. The function should print them in the format:

\`name: <name>, year: <year>, typed: <typed>\`

followed by a newline.

For example, calling \`describe("Zig", 2016, true)\` should print:
\`name: Zig, year: 2016, typed: true\``,

  starterCode: `const std = @import("std");

pub fn describe(name: []const u8, year: u16, typed: bool) void {
\t// Your code here
}

pub fn main() void {
\tdescribe("Zig", 2016, true);
}
`,

  solution: `const std = @import("std");

pub fn describe(name: []const u8, year: u16, typed: bool) void {
\tstd.debug.print("name: {s}, year: {d}, typed: {}\\n", .{ name, year, typed });
}

pub fn main() void {
\tdescribe("Zig", 2016, true);
}
`,

  tests: [
    {
      name: "describe Zig",
      code: `const std = @import("std");

{{FUNC}}

pub fn main() void {
\tdescribe("Zig", 2016, true);
}`,
      expected: "name: Zig, year: 2016, typed: true\n",
    },
    {
      name: "describe Rust",
      code: `const std = @import("std");

{{FUNC}}

pub fn main() void {
\tdescribe("Rust", 2010, true);
}`,
      expected: "name: Rust, year: 2010, typed: true\n",
    },
    {
      name: "describe Python with typed false",
      code: `const std = @import("std");

{{FUNC}}

pub fn main() void {
\tdescribe("Python", 1991, false);
}`,
      expected: "name: Python, year: 1991, typed: false\n",
    },
    {
      name: "describe C",
      code: `const std = @import("std");

{{FUNC}}

pub fn main() void {
\tdescribe("C", 1972, true);
}`,
      expected: "name: C, year: 1972, typed: true\n",
    },
  ],
};
