import type { Lesson } from "../../types";

export const structs: Lesson = {
  id: "structs",
  title: "Structs",
  chapterId: "data-structures",
  content: `## Grouping Data with Structs

Structs are Zig's primary tool for defining custom composite types. They group related data together and can have methods attached to them. If you are coming from C, Zig structs will feel familiar but with significant improvements. If you are coming from an object-oriented language, structs replace classes but without inheritance.

### Defining a Struct

\`\`\`zig
const Point = struct {
    x: f64,
    y: f64,
};
\`\`\`

Note the trailing comma after the last field. Zig encourages this style because it makes diffs cleaner when you add or remove fields.

### Creating Instances

\`\`\`zig
const p = Point{ .x = 3.0, .y = 4.0 };
\`\`\`

Fields are always initialized with the \`.\` prefix syntax. There is no positional initialization: you must name every field. This is a deliberate design choice that keeps code readable as structs evolve.

### Default Values

Fields can have default values. Any field with a default can be omitted when constructing the struct:

\`\`\`zig
const Config = struct {
    width: u32 = 800,
    height: u32 = 600,
    fullscreen: bool = false,
};

const c = Config{ .fullscreen = true };
// c.width is 800, c.height is 600
\`\`\`

### Methods

Methods in Zig are functions declared inside a struct that take \`self\` (or \`*self\` for mutation) as their first parameter:

\`\`\`zig
const Point = struct {
    x: f64,
    y: f64,

    fn distanceFromOrigin(self: Point) f64 {
        return @sqrt(self.x * self.x + self.y * self.y);
    }

    fn translate(self: *Point, dx: f64, dy: f64) void {
        self.x += dx;
        self.y += dy;
    }
};
\`\`\`

Call methods with dot syntax:

\`\`\`zig
var p = Point{ .x = 3.0, .y = 4.0 };
const d = p.distanceFromOrigin(); // 5.0
p.translate(1.0, 1.0);           // p is now {4.0, 5.0}
\`\`\`

When the method does not need to modify the struct, use \`self: Point\` (value). When it does, use \`self: *Point\` (pointer).

### Namespace Functions

Structs can also contain functions that do not take \`self\`. These act as namespace-scoped functions, similar to static methods:

\`\`\`zig
const Point = struct {
    x: f64,
    y: f64,

    fn origin() Point {
        return Point{ .x = 0, .y = 0 };
    }
};

const p = Point.origin();
\`\`\`

### Packed Structs

Zig also supports \`packed struct\` when you need exact control over memory layout, such as when mapping hardware registers or binary protocols:

\`\`\`zig
const Flags = packed struct {
    read: bool,
    write: bool,
    execute: bool,
    _padding: u5 = 0,
};
\`\`\`

A \`packed struct\` has no padding between fields and guarantees a specific bit layout.

### Your Task

Define a \`Rectangle\` struct with fields \`width\` and \`height\` (both \`f64\`).

Add two methods:
- \`area(self: Rectangle) f64\` --- returns the area (width times height).
- \`scale(self: *Rectangle, factor: f64) void\` --- multiplies both width and height by the given factor.`,

  starterCode: `const std = @import("std");

// Define your Rectangle struct here with width and height fields
// Add area() and scale() methods

pub fn main() !void {
\tvar r = Rectangle{ .width = 5.0, .height = 3.0 };
\tstd.debug.print("{d:.1}\\n", .{r.area()});
\tr.scale(2.0);
\tstd.debug.print("{d:.1}\\n", .{r.area()});
}
`,

  solution: `const std = @import("std");

const Rectangle = struct {
\twidth: f64,
\theight: f64,

\tfn area(self: Rectangle) f64 {
\t\treturn self.width * self.height;
\t}

\tfn scale(self: *Rectangle, factor: f64) void {
\t\tself.width *= factor;
\t\tself.height *= factor;
\t}
};

pub fn main() !void {
\tvar r = Rectangle{ .width = 5.0, .height = 3.0 };
\tstd.debug.print("{d:.1}\\n", .{r.area()});
\tr.scale(2.0);
\tstd.debug.print("{d:.1}\\n", .{r.area()});
}
`,

  tests: [
    {
      name: "area of 5x3",
      code: `const std = @import("std");

{{FUNC}}

pub fn main() !void {
\tconst r = Rectangle{ .width = 5.0, .height = 3.0 };
\tstd.debug.print("{d:.1}\\n", .{r.area()});
}
`,
      expected: "15.0\n",
    },
    {
      name: "area of 1x1",
      code: `const std = @import("std");

{{FUNC}}

pub fn main() !void {
\tconst r = Rectangle{ .width = 1.0, .height = 1.0 };
\tstd.debug.print("{d:.1}\\n", .{r.area()});
}
`,
      expected: "1.0\n",
    },
    {
      name: "scale by 2 then area",
      code: `const std = @import("std");

{{FUNC}}

pub fn main() !void {
\tvar r = Rectangle{ .width = 5.0, .height = 3.0 };
\tr.scale(2.0);
\tstd.debug.print("{d:.1}\\n", .{r.area()});
}
`,
      expected: "60.0\n",
    },
    {
      name: "scale by 0.5 then area",
      code: `const std = @import("std");

{{FUNC}}

pub fn main() !void {
\tvar r = Rectangle{ .width = 10.0, .height = 4.0 };
\tr.scale(0.5);
\tstd.debug.print("{d:.1}\\n", .{r.area()});
}
`,
      expected: "10.0\n",
    },
  ],
};
