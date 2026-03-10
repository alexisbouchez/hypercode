import type { Lesson } from "../../types";

export const structs: Lesson = {
  id: "structs",
  title: "Structs",
  chapterId: "classes-and-structures",
  content: `## Structs

In HolyC, the \`class\` keyword serves the role of both \`struct\` and \`class\` from C/C++. All classes are plain data structures — there are no methods, constructors, or access modifiers. This lesson covers practical struct patterns: nested classes, passing structs to functions, and building composite data types.

### Nested Classes

A class can contain another class as a member, creating hierarchical data structures:

\`\`\`holyc
class Address {
  U8 *city;
  U8 *state;
};

class Employee {
  U8 *name;
  I64 id;
  Address addr;
};

Employee e;
e.name = "Terry";
e.id = 1;
e.addr.city = "Las Vegas";
e.addr.state = "NV";

Print("%s (#%d) - %s, %s\\n", e.name, e.id, e.addr.city, e.addr.state);
// Terry (#1) - Las Vegas, NV
\`\`\`

### Struct Pointers and Functions

Functions commonly accept struct pointers to avoid copying and to allow mutation:

\`\`\`holyc
class Vec2 {
  F64 x, y;
};

F64 Magnitude(Vec2 *v) {
  return Sqrt(v->x * v->x + v->y * v->y);
}

U0 Scale(Vec2 *v, F64 factor) {
  v->x = v->x * factor;
  v->y = v->y * factor;
}

Vec2 v;
v.x = 3.0;
v.y = 4.0;
Print("%.1f\\n", Magnitude(&v));  // 5.0
Scale(&v, 2.0);
Print("%.1f %.1f\\n", v.x, v.y);  // 6.0 8.0
\`\`\`

### Composing Structs

Build complex types by nesting classes. A \`Line\` is two \`Point\`s:

\`\`\`holyc
class Point {
  I64 x, y;
};

class Line {
  Point start;
  Point end;
};

Line ln;
ln.start.x = 0;
ln.start.y = 0;
ln.end.x = 10;
ln.end.y = 5;

I64 dx = ln.end.x - ln.start.x;
I64 dy = ln.end.y - ln.start.y;
Print("dx=%d dy=%d\\n", dx, dy);  // dx=10 dy=5
\`\`\`

### Your Task

Define a \`Point\` class with \`I64 x\` and \`I64 y\` members. Define a \`Rect\` class with two \`Point\` members: \`origin\` and \`size\` (where \`size.x\` is width and \`size.y\` is height).

Write a function \`I64 Area(Rect *r)\` that returns the area (width * height).

Create a rectangle with origin (2, 3) and size (10, 5), then print the area.

Expected output: \`50\``,

  starterCode: `// Define Point and Rect classes

// Write Area function

// Create a Rect with origin (2,3) and size (10,5), print area
`,

  solution: `class Point {
  I64 x, y;
};

class Rect {
  Point origin;
  Point size;
};

I64 Area(Rect *r) {
  return r->size.x * r->size.y;
}

Rect r;
r.origin.x = 2;
r.origin.y = 3;
r.size.x = 10;
r.size.y = 5;
Print("%d\\n", Area(&r));
`,

  tests: [
    {
      name: "prints area of 10x5 rect",
      expected: "50\n",
    },
    {
      name: "area of 7x3 rect",
      code: '{{FUNC}}\nRect r;\nr.origin.x = 0;\nr.origin.y = 0;\nr.size.x = 7;\nr.size.y = 3;\nPrint("%d\\n", Area(&r));',
      expected: "21\n",
    },
    {
      name: "area of 1x1 rect",
      code: '{{FUNC}}\nRect r;\nr.origin.x = 0;\nr.origin.y = 0;\nr.size.x = 1;\nr.size.y = 1;\nPrint("%d\\n", Area(&r));',
      expected: "1\n",
    },
  ],
};
