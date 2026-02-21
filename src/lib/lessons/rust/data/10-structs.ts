import type { Lesson } from "../../types";

export const structs: Lesson = {
  id: "structs",
  title: "Structs",
  chapterId: "structs-enums",
  content: `## Structs

Structs let you group related data together under a named type.

### Defining a Struct

\`\`\`rust
struct Point {
    x: f64,
    y: f64,
}
\`\`\`

### Creating Instances

\`\`\`rust
let p = Point { x: 1.0, y: 2.0 };
println!("{}", p.x); // 1
\`\`\`

Use **struct update syntax** to create a new instance based on another:

\`\`\`rust
let p2 = Point { x: 3.0, ..p }; // y copied from p
\`\`\`

### impl Blocks

Methods and associated functions live in \`impl\` blocks:

\`\`\`rust
impl Point {
    // Associated function (no &self) — called as Point::new(...)
    fn new(x: f64, y: f64) -> Self {
        Point { x, y }
    }

    // Method — takes &self
    fn distance_to_origin(&self) -> f64 {
        (self.x * self.x + self.y * self.y).sqrt()
    }

    // Mutable method — takes &mut self
    fn translate(&mut self, dx: f64, dy: f64) {
        self.x += dx;
        self.y += dy;
    }
}
\`\`\`

### Tuple Structs

\`\`\`rust
struct Color(u8, u8, u8);
let black = Color(0, 0, 0);
println!("{}", black.0); // 0
\`\`\`

### Your Task

Implement a \`Rectangle\` struct with \`width\` and \`height\` fields and these methods:

- \`new(width: f64, height: f64) -> Self\`
- \`area(&self) -> f64\`
- \`perimeter(&self) -> f64\`
- \`is_square(&self) -> bool\`
- \`scale(&self, factor: f64) -> Rectangle\` — returns a new scaled rectangle`,

  starterCode: `struct Rectangle {
    width: f64,
    height: f64,
}

impl Rectangle {
    fn new(width: f64, height: f64) -> Self {
        todo!()
    }

    fn area(&self) -> f64 {
        todo!()
    }

    fn perimeter(&self) -> f64 {
        todo!()
    }

    fn is_square(&self) -> bool {
        todo!()
    }

    fn scale(&self, factor: f64) -> Rectangle {
        todo!()
    }
}

fn main() {
    let r = Rectangle::new(3.0, 4.0);
    println!("{}", r.area());
    println!("{}", r.perimeter());
    println!("{}", Rectangle::new(5.0, 5.0).is_square());
    println!("{}", r.scale(2.0).area());
}
`,

  solution: `struct Rectangle {
    width: f64,
    height: f64,
}

impl Rectangle {
    fn new(width: f64, height: f64) -> Self {
        Rectangle { width, height }
    }

    fn area(&self) -> f64 {
        self.width * self.height
    }

    fn perimeter(&self) -> f64 {
        2.0 * (self.width + self.height)
    }

    fn is_square(&self) -> bool {
        (self.width - self.height).abs() < 1e-10
    }

    fn scale(&self, factor: f64) -> Rectangle {
        Rectangle::new(self.width * factor, self.height * factor)
    }
}

fn main() {
    let r = Rectangle::new(3.0, 4.0);
    println!("{}", r.area());
    println!("{}", r.perimeter());
    println!("{}", Rectangle::new(5.0, 5.0).is_square());
    println!("{}", r.scale(2.0).area());
}
`,

  tests: [
    {
      name: "area of 3x4 rectangle is 12",
      expected: "12\n",
      code: `{{FUNC}}
fn main() {
    println!("{}", Rectangle::new(3.0, 4.0).area());
}`,
    },
    {
      name: "perimeter of 3x4 rectangle is 14",
      expected: "14\n",
      code: `{{FUNC}}
fn main() {
    println!("{}", Rectangle::new(3.0, 4.0).perimeter());
}`,
    },
    {
      name: "5x5 rectangle is a square",
      expected: "true\n",
      code: `{{FUNC}}
fn main() {
    println!("{}", Rectangle::new(5.0, 5.0).is_square());
}`,
    },
    {
      name: "3x4 rectangle is not a square",
      expected: "false\n",
      code: `{{FUNC}}
fn main() {
    println!("{}", Rectangle::new(3.0, 4.0).is_square());
}`,
    },
    {
      name: "scale(2.0) area of 3x4 is 48",
      expected: "48\n",
      code: `{{FUNC}}
fn main() {
    println!("{}", Rectangle::new(3.0, 4.0).scale(2.0).area());
}`,
    },
  ],
};
