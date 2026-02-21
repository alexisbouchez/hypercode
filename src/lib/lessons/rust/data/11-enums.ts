import type { Lesson } from "../../types";

export const enums: Lesson = {
  id: "enums",
  title: "Enums",
  chapterId: "structs-enums",
  content: `## Enums

Enums let a type have one of several variants. Rust enums are much more powerful than in most languages — each variant can carry data.

### Basic Enum

\`\`\`rust
enum Direction {
    North,
    South,
    East,
    West,
}
let dir = Direction::North;
\`\`\`

### Enums with Data

Variants can hold different types and amounts of data:

\`\`\`rust
enum Message {
    Quit,                      // unit variant
    Move { x: i32, y: i32 },  // struct variant
    Write(String),             // tuple variant
    Color(u8, u8, u8),        // tuple variant with multiple fields
}
\`\`\`

### impl for Enums

\`\`\`rust
impl Direction {
    fn is_vertical(&self) -> bool {
        matches!(self, Direction::North | Direction::South)
    }
}
\`\`\`

### match on Enums

The compiler enforces that all variants are handled:

\`\`\`rust
let msg = Message::Write(String::from("hello"));
match msg {
    Message::Quit => println!("quit"),
    Message::Move { x, y } => println!("move to {},{}", x, y),
    Message::Write(text) => println!("write: {}", text),
    Message::Color(r, g, b) => println!("color: {},{},{}", r, g, b),
}
\`\`\`

### Your Task

1. Implement a \`Direction\` enum with \`North\`, \`South\`, \`East\`, \`West\` variants and methods:
   - \`opposite(&self) -> Direction\`
   - \`to_str(&self) -> &str\`

2. Implement a \`Shape\` enum with \`Circle(f64)\`, \`Rectangle(f64, f64)\`, and \`Triangle(f64, f64, f64)\` variants and:
   - \`area(&self) -> f64\` — use Heron's formula for triangles: \`s = (a+b+c)/2\`, \`area = sqrt(s*(s-a)*(s-b)*(s-c))\``,

  starterCode: `enum Direction {
    North,
    South,
    East,
    West,
}

impl Direction {
    fn opposite(&self) -> Direction {
        todo!()
    }

    fn to_str(&self) -> &str {
        todo!()
    }
}

enum Shape {
    Circle(f64),
    Rectangle(f64, f64),
    Triangle(f64, f64, f64),
}

impl Shape {
    fn area(&self) -> f64 {
        todo!()
    }
}

fn main() {
    println!("{}", Direction::North.opposite().to_str());
    println!("{:.4}", Shape::Circle(1.0).area());
    println!("{}", Shape::Rectangle(3.0, 4.0).area());
    println!("{:.4}", Shape::Triangle(3.0, 4.0, 5.0).area());
}
`,

  solution: `enum Direction {
    North,
    South,
    East,
    West,
}

impl Direction {
    fn opposite(&self) -> Direction {
        match self {
            Direction::North => Direction::South,
            Direction::South => Direction::North,
            Direction::East => Direction::West,
            Direction::West => Direction::East,
        }
    }

    fn to_str(&self) -> &str {
        match self {
            Direction::North => "North",
            Direction::South => "South",
            Direction::East => "East",
            Direction::West => "West",
        }
    }
}

enum Shape {
    Circle(f64),
    Rectangle(f64, f64),
    Triangle(f64, f64, f64),
}

impl Shape {
    fn area(&self) -> f64 {
        match self {
            Shape::Circle(r) => std::f64::consts::PI * r * r,
            Shape::Rectangle(w, h) => w * h,
            Shape::Triangle(a, b, c) => {
                let s = (a + b + c) / 2.0;
                (s * (s - a) * (s - b) * (s - c)).sqrt()
            }
        }
    }
}

fn main() {
    println!("{}", Direction::North.opposite().to_str());
    println!("{:.4}", Shape::Circle(1.0).area());
    println!("{}", Shape::Rectangle(3.0, 4.0).area());
    println!("{:.4}", Shape::Triangle(3.0, 4.0, 5.0).area());
}
`,

  tests: [
    {
      name: "Direction::North.opposite() is South",
      expected: "South\n",
      code: `{{FUNC}}
fn main() {
    println!("{}", Direction::North.opposite().to_str());
}`,
    },
    {
      name: "Direction::East.opposite() is West",
      expected: "West\n",
      code: `{{FUNC}}
fn main() {
    println!("{}", Direction::East.opposite().to_str());
}`,
    },
    {
      name: "Circle(1.0) area is 3.1416",
      expected: "3.1416\n",
      code: `{{FUNC}}
fn main() {
    println!("{:.4}", Shape::Circle(1.0).area());
}`,
    },
    {
      name: "Rectangle(3.0, 4.0) area is 12",
      expected: "12\n",
      code: `{{FUNC}}
fn main() {
    println!("{}", Shape::Rectangle(3.0, 4.0).area());
}`,
    },
    {
      name: "Triangle(3.0, 4.0, 5.0) area is 6.0000",
      expected: "6.0000\n",
      code: `{{FUNC}}
fn main() {
    println!("{:.4}", Shape::Triangle(3.0, 4.0, 5.0).area());
}`,
    },
  ],
};
