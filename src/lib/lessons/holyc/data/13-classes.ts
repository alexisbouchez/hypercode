import type { Lesson } from "../../types";

export const classes: Lesson = {
  id: "classes",
  title: "Classes",
  chapterId: "classes-and-structures",
  content: `## Classes

HolyC uses \`class\` instead of C's \`struct\`/\`typedef struct\`. A class is a value type that groups related data together.

### Defining a Class

\`\`\`holyc
class Point {
  I64 x, y;
};
\`\`\`

The semicolon after the closing brace is required (same as C structs).

### Creating Instances

Declare a variable of the class type and set its members using the dot operator \`.\`:

\`\`\`holyc
Point p;
p.x = 10;
p.y = 20;
Print("x=%d y=%d\\n", p.x, p.y);
\`\`\`

### Pointers to Classes

Use \`->\` to access members through a pointer:

\`\`\`holyc
Point p;
p.x = 5;
p.y = 8;

Point *ptr = &p;
Print("x=%d y=%d\\n", ptr->x, ptr->y);
\`\`\`

### Classes as Function Parameters

Pass instances by value or by pointer:

\`\`\`holyc
class Rectangle {
  I64 width, height;
};

I64 Area(Rectangle *r) {
  return r->width * r->height;
}

Rectangle rect;
rect.width  = 6;
rect.height = 4;
Print("Area: %d\\n", Area(&rect));  // Area: 24
\`\`\`

### String Members

A \`U8 *\` member holds a pointer to a string:

\`\`\`holyc
class Person {
  U8 *name;
  I64 age;
};

Person p;
p.name = "Terry";
p.age  = 37;
Print("%s is %d years old\\n", p.name, p.age);
\`\`\`

### Your Task

Define a class \`Circle\` with an \`F64 radius\` field. Write a function \`F64 Circumference(Circle *c)\` that returns \`2 * 3.14159 * c->radius\`.

Create a circle with radius \`5.0\` and print the circumference with 2 decimal places.

Expected output: \`31.42\``,

  starterCode: `// Define Circle class and Circumference function

// Create a Circle with radius 5.0 and print circumference
`,

  solution: `class Circle {
  F64 radius;
};

F64 Circumference(Circle *c) {
  return 2.0 * 3.14159 * c->radius;
}

Circle circ;
circ.radius = 5.0;
Print("%.2f\\n", Circumference(&circ));
`,

  tests: [
    {
      name: "prints circumference of radius 5",
      expected: "31.42\n",
    },
  ],
};
