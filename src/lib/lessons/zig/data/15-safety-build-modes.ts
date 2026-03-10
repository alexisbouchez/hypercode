import type { Lesson } from "../../types";

export const safetyBuildModes: Lesson = {
  id: "safety-build-modes",
  title: "Safety & Build Modes",
  chapterId: "advanced",
  content: `## Safety Checks and Build Modes

Zig's design philosophy is "detectable illegal behavior." The compiler and runtime work together to catch bugs early, and you control how aggressively they do so through build modes. Understanding these modes is essential for writing production Zig code.

### The Four Build Modes

Zig provides four optimization modes, each with different trade-offs between safety, performance, and binary size:

**Debug** (default)
- No optimizations applied
- Runtime safety checks enabled: integer overflow detection, out-of-bounds indexing, null pointer dereference on optionals, and unreachable code detection
- Full stack traces on panic
- Best for development and debugging

**ReleaseSafe**
- Optimizations enabled (like \`-O2\` in C)
- Runtime safety checks still enabled
- Best for production code where correctness matters more than peak performance
- This is what most server software should use

**ReleaseFast**
- Aggressive optimizations (like \`-O3\` in C)
- All runtime safety checks removed
- Best for performance-critical inner loops, games, or code that has been thoroughly tested
- Undefined behavior is possible if your code has bugs

**ReleaseSmall**
- Optimized for binary size
- Safety checks removed
- Best for embedded systems or WebAssembly where code size is constrained

You select the mode when building:

\`\`\`
zig build -Doptimize=Debug
zig build -Doptimize=ReleaseSafe
zig build -Doptimize=ReleaseFast
zig build -Doptimize=ReleaseSmall
\`\`\`

### Runtime Safety Checks

In Debug and ReleaseSafe modes, Zig performs several checks at runtime:

**Integer overflow:** Arithmetic that overflows panics instead of silently wrapping:

\`\`\`zig
var x: u8 = 255;
x += 1; // PANIC in Debug/ReleaseSafe, wraps to 0 in ReleaseFast
\`\`\`

If you intentionally want wrapping arithmetic, use the wrapping operators:

\`\`\`zig
var x: u8 = 255;
x +%= 1; // Always wraps: x is now 0
\`\`\`

**Out-of-bounds:** Indexing past the end of an array or slice panics:

\`\`\`zig
const arr = [_]i32{ 1, 2, 3 };
const val = arr[5]; // PANIC: index out of bounds
\`\`\`

**Unreachable:** The \`unreachable\` keyword tells the compiler that a code path should never execute. In safe modes, reaching it panics. In fast modes, it is undefined behavior:

\`\`\`zig
fn classify(n: u8) []const u8 {
    if (n < 128) return "low";
    if (n >= 128) return "high";
    unreachable; // Safe modes: panic. Fast modes: UB.
}
\`\`\`

### @setRuntimeSafety

You can override the build mode's safety setting for a specific scope using \`@setRuntimeSafety\`:

\`\`\`zig
fn fastUncheckedAdd(a: u32, b: u32) u32 {
    @setRuntimeSafety(false);
    return a + b; // No overflow check even in Debug mode
}

fn alwaysSafeAdd(a: u32, b: u32) u32 {
    @setRuntimeSafety(true);
    return a + b; // Overflow check even in ReleaseFast mode
}
\`\`\`

This is useful when:
- A hot inner loop needs to skip bounds checks after you have proven correctness
- A critical function should always be checked regardless of build mode

### Wrapping vs Saturating Arithmetic

Zig provides explicit operators for non-standard arithmetic:

| Operator | Meaning | Example |
|----------|---------|---------|
| \`+\`  | Checked add (panics on overflow) | \`200 + 200\` panics for u8 |
| \`+%\` | Wrapping add | \`255 +% 1 = 0\` for u8 |
| \`+|\` | Saturating add | \`250 +| 10 = 255\` for u8 |

The same variants exist for subtraction (\`-\`, \`-%\`, \`-|\`) and multiplication (\`*\`, \`*%\`, \`*|\`).

> "Safety protocols engaged, Captain." In Debug mode, Zig's shields are at maximum --- every out-of-bounds access, every overflow is caught. In ReleaseFast mode, the shields are down: you gain speed but lose protection.

### Your Task

Write three functions that demonstrate different arithmetic behaviors:

- \`checkedAdd(a: u8, b: u8) ?u16\` --- adds \`a\` and \`b\` and returns the result as a \`u16\`. If the sum would exceed 255, return \`null\`. Otherwise, return the sum. (This simulates what a safety check does, but as a function you control.)
- \`wrappingAdd(a: u8, b: u8) u8\` --- adds \`a\` and \`b\` using wrapping arithmetic (\`+%\`). Always succeeds, wraps on overflow.
- \`saturatingAdd(a: u8, b: u8) u8\` --- adds \`a\` and \`b\` using saturating arithmetic (\`+|\`). Clamps to 255 instead of overflowing.`,

  starterCode: `const std = @import("std");

fn checkedAdd(a: u8, b: u8) ?u16 {
\t// Return null if a + b > 255, else return the sum as u16
\t_ = a;
\t_ = b;
\treturn null;
}

fn wrappingAdd(a: u8, b: u8) u8 {
\t// Use +% for wrapping addition
\t_ = a;
\t_ = b;
\treturn 0;
}

fn saturatingAdd(a: u8, b: u8) u8 {
\t// Use +| for saturating addition
\t_ = a;
\t_ = b;
\treturn 0;
}

pub fn main() !void {
\tconst checked = checkedAdd(200, 100);
\tif (checked) |val| {
\t\tstd.debug.print("checked: {}\\n", .{val});
\t} else {
\t\tstd.debug.print("checked: overflow\\n", .{});
\t}

\tstd.debug.print("wrapping: {}\\n", .{wrappingAdd(200, 100)});
\tstd.debug.print("saturating: {}\\n", .{saturatingAdd(200, 100)});
}
`,

  solution: `const std = @import("std");

fn checkedAdd(a: u8, b: u8) ?u16 {
\tconst wide_a: u16 = a;
\tconst wide_b: u16 = b;
\tconst sum = wide_a + wide_b;
\tif (sum > 255) return null;
\treturn sum;
}

fn wrappingAdd(a: u8, b: u8) u8 {
\treturn a +% b;
}

fn saturatingAdd(a: u8, b: u8) u8 {
\treturn a +| b;
}

pub fn main() !void {
\tconst checked = checkedAdd(200, 100);
\tif (checked) |val| {
\t\tstd.debug.print("checked: {}\\n", .{val});
\t} else {
\t\tstd.debug.print("checked: overflow\\n", .{});
\t}

\tstd.debug.print("wrapping: {}\\n", .{wrappingAdd(200, 100)});
\tstd.debug.print("saturating: {}\\n", .{saturatingAdd(200, 100)});
}
`,

  tests: [
    {
      name: "checkedAdd(100, 50) is 150",
      code: `const std = @import("std");

{{FUNC}}

pub fn main() !void {
\tconst result = checkedAdd(100, 50) orelse 9999;
\tstd.debug.print("{}\\n", .{result});
}`,
      expected: "150\n",
    },
    {
      name: "checkedAdd(200, 100) is null (overflow)",
      code: `const std = @import("std");

{{FUNC}}

pub fn main() !void {
\tconst result = checkedAdd(200, 100);
\tif (result) |_| {
\t\tstd.debug.print("has value\\n", .{});
\t} else {
\t\tstd.debug.print("null\\n", .{});
\t}
}`,
      expected: "null\n",
    },
    {
      name: "checkedAdd(255, 0) is 255",
      code: `const std = @import("std");

{{FUNC}}

pub fn main() !void {
\tconst result = checkedAdd(255, 0) orelse 9999;
\tstd.debug.print("{}\\n", .{result});
}`,
      expected: "255\n",
    },
    {
      name: "wrappingAdd(200, 100) wraps to 44",
      code: `const std = @import("std");

{{FUNC}}

pub fn main() !void {
\tstd.debug.print("{}\\n", .{wrappingAdd(200, 100)});
}`,
      expected: "44\n",
    },
    {
      name: "wrappingAdd(255, 1) wraps to 0",
      code: `const std = @import("std");

{{FUNC}}

pub fn main() !void {
\tstd.debug.print("{}\\n", .{wrappingAdd(255, 1)});
}`,
      expected: "0\n",
    },
    {
      name: "saturatingAdd(200, 100) saturates to 255",
      code: `const std = @import("std");

{{FUNC}}

pub fn main() !void {
\tstd.debug.print("{}\\n", .{saturatingAdd(200, 100)});
}`,
      expected: "255\n",
    },
    {
      name: "saturatingAdd(100, 50) is 150 (no saturation)",
      code: `const std = @import("std");

{{FUNC}}

pub fn main() !void {
\tstd.debug.print("{}\\n", .{saturatingAdd(100, 50)});
}`,
      expected: "150\n",
    },
  ],
};
