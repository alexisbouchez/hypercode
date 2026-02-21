import type { Lesson } from "../../types";

export const traits: Lesson = {
  id: "traits",
  title: "Traits",
  chapterId: "traits-generics",
  content: `## Traits

Traits define shared behavior. They are similar to interfaces in other languages.

### Defining a Trait

\`\`\`rust
trait Summary {
    fn summarize(&self) -> String;

    // Default implementation
    fn author(&self) -> String {
        String::from("(anonymous)")
    }
}
\`\`\`

### Implementing a Trait

\`\`\`rust
struct Article {
    title: String,
    content: String,
}

impl Summary for Article {
    fn summarize(&self) -> String {
        format!("{}: {}", self.title, &self.content[..50])
    }
}
\`\`\`

### Trait Bounds

Require that a type implements a trait:

\`\`\`rust
fn notify(item: &impl Summary) {
    println!("Breaking news! {}", item.summarize());
}

// Equivalent with where clause:
fn notify<T: Summary>(item: &T) {
    println!("Breaking news! {}", item.summarize());
}
\`\`\`

### Multiple Trait Bounds

\`\`\`rust
fn print_and_summarize(item: &(impl Summary + std::fmt::Display)) {
    println!("{} — {}", item, item.summarize());
}
\`\`\`

### impl Trait in Return Position

\`\`\`rust
fn make_summary() -> impl Summary {
    Article { title: String::from("..."), content: String::from("...") }
}
\`\`\`

### Common Standard Traits

- \`Display\` — for \`{}\` formatting
- \`Debug\` — for \`{:?}\` formatting (can \`#[derive(Debug)]\`)
- \`Clone\` — for \`.clone()\`
- \`PartialOrd\` / \`Ord\` — for comparison
- \`Iterator\` — for \`.next()\` iteration

### Your Task

Define an \`Area\` trait with:
- \`area(&self) -> f64\`
- \`larger_than(&self, other: &impl Area) -> bool\` (default implementation comparing areas)

Implement \`Area\` for \`Circle { radius: f64 }\` and \`Square { side: f64 }\`.

Also write \`print_area(shape: &impl Area) -> f64\` that returns the area.`,

  starterCode: `trait Area {
    fn area(&self) -> f64;
    fn larger_than(&self, other: &impl Area) -> bool {
        self.area() > other.area()
    }
}

struct Circle {
    radius: f64,
}

struct Square {
    side: f64,
}

impl Area for Circle {
    fn area(&self) -> f64 {
        todo!()
    }
}

impl Area for Square {
    fn area(&self) -> f64 {
        todo!()
    }
}

fn print_area(shape: &impl Area) -> f64 {
    shape.area()
}

fn main() {
    let c = Circle { radius: 1.0 };
    let s = Square { side: 2.0 };
    println!("{:.4}", c.area());
    println!("{}", s.area());
    println!("{}", s.larger_than(&c));
    println!("{:.4}", print_area(&Circle { radius: 3.0 }));
}
`,

  solution: `trait Area {
    fn area(&self) -> f64;
    fn larger_than(&self, other: &impl Area) -> bool {
        self.area() > other.area()
    }
}

struct Circle {
    radius: f64,
}

struct Square {
    side: f64,
}

impl Area for Circle {
    fn area(&self) -> f64 {
        std::f64::consts::PI * self.radius * self.radius
    }
}

impl Area for Square {
    fn area(&self) -> f64 {
        self.side * self.side
    }
}

fn print_area(shape: &impl Area) -> f64 {
    shape.area()
}

fn main() {
    let c = Circle { radius: 1.0 };
    let s = Square { side: 2.0 };
    println!("{:.4}", c.area());
    println!("{}", s.area());
    println!("{}", s.larger_than(&c));
    println!("{:.4}", print_area(&Circle { radius: 3.0 }));
}
`,

  tests: [
    {
      name: "Circle(1.0) area is 3.1416",
      expected: "3.1416\n",
      code: `{{FUNC}}
fn main() {
    println!("{:.4}", Circle { radius: 1.0 }.area());
}`,
    },
    {
      name: "Square(2.0) area is 4",
      expected: "4\n",
      code: `{{FUNC}}
fn main() {
    println!("{}", Square { side: 2.0 }.area());
}`,
    },
    {
      name: "Square(2.0) is larger than Circle(1.0)",
      expected: "true\n",
      code: `{{FUNC}}
fn main() {
    println!("{}", Square { side: 2.0 }.larger_than(&Circle { radius: 1.0 }));
}`,
    },
    {
      name: "print_area of Circle(3.0) is 28.2743",
      expected: "28.2743\n",
      code: `{{FUNC}}
fn main() {
    println!("{:.4}", print_area(&Circle { radius: 3.0 }));
}`,
    },
  ],
};
