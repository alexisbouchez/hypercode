import type { Lesson } from "../../types";

export const errorHandling: Lesson = {
  id: "error-handling",
  title: "Error Handling",
  chapterId: "functions-chapter",
  content: `## Errors as Values in Zig

Zig does not have exceptions. Like Go, it treats errors as values. But Zig goes further: errors are a first-class part of the type system, enforced at compile time.

### Error Sets

An error set is an enumeration of possible error values:

\`\`\`zig
const FileError = error{
    NotFound,
    PermissionDenied,
    OutOfMemory,
};
\`\`\`

You can also let the compiler infer the error set from a function's body by using \`!\` without naming a specific set.

### Error Union Types

An error union combines a normal type with an error set. The syntax is \`ErrorSet!ValueType\`:

\`\`\`zig
fn divide(a: f64, b: f64) error{DivisionByZero}!f64 {
    if (b == 0.0) return error.DivisionByZero;
    return a / b;
}
\`\`\`

The inferred shorthand uses just \`!\`:

\`\`\`zig
fn divide(a: f64, b: f64) !f64 {
    if (b == 0.0) return error.DivisionByZero;
    return a / b;
}
\`\`\`

### try

The \`try\` keyword unwraps an error union. If the value is an error, it immediately returns that error from the current function:

\`\`\`zig
fn doWork() !void {
    const result = try divide(10.0, 0.0);
    // If divide returns an error, doWork returns that same error
    std.debug.print("result: {d}\\n", .{result});
}
\`\`\`

\`try x\` is shorthand for \`x catch |err| return err\`. It propagates errors up the call stack without boilerplate.

> Red alert! Error propagation in Zig works much like the Enterprise's alert system --- the problem is detected at the source and escalated up the chain of command until someone handles it.

### catch

The \`catch\` keyword handles errors inline. You can provide a default value or run a block:

\`\`\`zig
const value = divide(10.0, 0.0) catch 0.0;
// value is 0.0 if divide returned an error
\`\`\`

With a capture:

\`\`\`zig
const value = divide(10.0, 0.0) catch |err| {
    std.debug.print("error: {}\\n", .{err});
    return;
};
\`\`\`

### Error in main

The \`main\` function can return \`!void\` to propagate errors. If an error reaches \`main\`, the program terminates with an error trace:

\`\`\`zig
pub fn main() !void {
    const result = try divide(10.0, 3.0);
    std.debug.print("{d}\\n", .{result});
}
\`\`\`

### Custom Error Sets

You can define descriptive error sets for your domain:

\`\`\`zig
const ValidationError = error{
    TooShort,
    TooLong,
    InvalidCharacter,
};

fn validateUsername(name: []const u8) ValidationError!void {
    if (name.len < 3) return error.TooShort;
    if (name.len > 20) return error.TooLong;
}
\`\`\`

### Merging Error Sets

Error sets can be merged with \`||\`:

\`\`\`zig
const IoError = error{ ReadFailed, WriteFailed };
const ParseError = error{ InvalidFormat, UnexpectedToken };
const AppError = IoError || ParseError;
\`\`\`

### Your Task

Write a function \`validateAge\` that takes an \`i32\` and returns \`error{Negative,Unrealistic}!void\`:

- If age is negative, return \`error.Negative\`
- If age is greater than 150, return \`error.Unrealistic\`
- Otherwise, return normally (no error)

Then write a helper function \`checkAge\` that takes an \`i32\`, calls \`validateAge\`, and prints:
- \`"valid"\` if no error
- \`"age cannot be negative"\` if the error is \`Negative\`
- \`"age is unrealistic"\` if the error is \`Unrealistic\`

Each message should be followed by a newline.`,

  starterCode: `const std = @import("std");

pub fn validateAge(age: i32) error{ Negative, Unrealistic }!void {
\t// Your code here
}

pub fn checkAge(age: i32) void {
\t// Call validateAge and handle the result
\t// Print "valid", "age cannot be negative", or "age is unrealistic"
}

pub fn main() void {
\tcheckAge(25);
\tcheckAge(-1);
\tcheckAge(200);
}
`,

  solution: `const std = @import("std");

pub fn validateAge(age: i32) error{ Negative, Unrealistic }!void {
\tif (age < 0) return error.Negative;
\tif (age > 150) return error.Unrealistic;
}

pub fn checkAge(age: i32) void {
\tvalidateAge(age) catch |err| {
\t\tswitch (err) {
\t\t\terror.Negative => std.debug.print("age cannot be negative\\n", .{}),
\t\t\terror.Unrealistic => std.debug.print("age is unrealistic\\n", .{}),
\t\t}
\t\treturn;
\t};
\tstd.debug.print("valid\\n", .{});
}

pub fn main() void {
\tcheckAge(25);
\tcheckAge(-1);
\tcheckAge(200);
}
`,

  tests: [
    {
      name: "valid age (25)",
      code: `const std = @import("std");

{{FUNC}}

pub fn main() void {
\tcheckAge(25);
}`,
      expected: "valid\n",
    },
    {
      name: "negative age (-1)",
      code: `const std = @import("std");

{{FUNC}}

pub fn main() void {
\tcheckAge(-1);
}`,
      expected: "age cannot be negative\n",
    },
    {
      name: "unrealistic age (200)",
      code: `const std = @import("std");

{{FUNC}}

pub fn main() void {
\tcheckAge(200);
}`,
      expected: "age is unrealistic\n",
    },
    {
      name: "edge case: age 0 is valid",
      code: `const std = @import("std");

{{FUNC}}

pub fn main() void {
\tcheckAge(0);
}`,
      expected: "valid\n",
    },
    {
      name: "edge case: age 150 is valid",
      code: `const std = @import("std");

{{FUNC}}

pub fn main() void {
\tcheckAge(150);
}`,
      expected: "valid\n",
    },
  ],
};
