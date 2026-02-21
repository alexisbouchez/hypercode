import type { Lesson } from "../../types";

export const generics: Lesson = {
  id: "generics",
  title: "Generics",
  chapterId: "traits-generics",
  content: `## Generics

Generics let you write code that works for many types without duplication.

### Generic Functions

\`\`\`rust
fn largest<T: PartialOrd>(list: &[T]) -> &T {
    let mut largest = &list[0];
    for item in list {
        if item > largest {
            largest = item;
        }
    }
    largest
}
\`\`\`

The \`<T: PartialOrd>\` syntax means "T must implement PartialOrd" — required for the \`>\` comparison.

### Generic Structs

\`\`\`rust
struct Pair<T> {
    first: T,
    second: T,
}

impl<T: std::fmt::Display + PartialOrd> Pair<T> {
    fn cmp_display(&self) {
        if self.first >= self.second {
            println!("Largest: {}", self.first);
        } else {
            println!("Largest: {}", self.second);
        }
    }
}
\`\`\`

### Monomorphization

Rust **monomorphizes** generic code at compile time — it creates a specialized version for each concrete type used. This means generics have **zero runtime cost** compared to writing type-specific code.

### Multiple Type Parameters

\`\`\`rust
struct Pair<T, U> {
    first: T,
    second: U,
}
\`\`\`

### Your Task

1. \`largest<T: PartialOrd>(list: &[T]) -> &T\` — returns a reference to the largest element.
2. Implement a generic \`Stack<T>\` with:
   - \`new() -> Self\`
   - \`push(&mut self, item: T)\`
   - \`pop(&mut self) -> Option<T>\`
   - \`peek(&self) -> Option<&T>\`
   - \`is_empty(&self) -> bool\`
   - \`size(&self) -> usize\``,

  starterCode: `fn largest<T: PartialOrd>(list: &[T]) -> &T {
    todo!()
}

struct Stack<T> {
    elements: Vec<T>,
}

impl<T> Stack<T> {
    fn new() -> Self {
        todo!()
    }
    fn push(&mut self, item: T) {
        todo!()
    }
    fn pop(&mut self) -> Option<T> {
        todo!()
    }
    fn peek(&self) -> Option<&T> {
        todo!()
    }
    fn is_empty(&self) -> bool {
        todo!()
    }
    fn size(&self) -> usize {
        todo!()
    }
}

fn main() {
    println!("{}", largest(&[34, 50, 25, 100, 65]));
    let mut s: Stack<i32> = Stack::new();
    s.push(1); s.push(2); s.push(3);
    println!("{}", s.peek().unwrap());
    println!("{}", s.pop().unwrap());
    println!("{}", s.size());
}
`,

  solution: `fn largest<T: PartialOrd>(list: &[T]) -> &T {
    let mut largest = &list[0];
    for item in list {
        if item > largest {
            largest = item;
        }
    }
    largest
}

struct Stack<T> {
    elements: Vec<T>,
}

impl<T> Stack<T> {
    fn new() -> Self {
        Stack { elements: Vec::new() }
    }
    fn push(&mut self, item: T) {
        self.elements.push(item);
    }
    fn pop(&mut self) -> Option<T> {
        self.elements.pop()
    }
    fn peek(&self) -> Option<&T> {
        self.elements.last()
    }
    fn is_empty(&self) -> bool {
        self.elements.is_empty()
    }
    fn size(&self) -> usize {
        self.elements.len()
    }
}

fn main() {
    println!("{}", largest(&[34, 50, 25, 100, 65]));
    let mut s: Stack<i32> = Stack::new();
    s.push(1); s.push(2); s.push(3);
    println!("{}", s.peek().unwrap());
    println!("{}", s.pop().unwrap());
    println!("{}", s.size());
}
`,

  tests: [
    {
      name: "largest(&[34,50,25,100,65]) returns 100",
      expected: "100\n",
      code: `{{FUNC}}
fn main() {
    println!("{}", largest(&[34, 50, 25, 100, 65]));
}`,
    },
    {
      name: "Stack peek after push 1,2,3 returns 3",
      expected: "3\n",
      code: `{{FUNC}}
fn main() {
    let mut s: Stack<i32> = Stack::new();
    s.push(1); s.push(2); s.push(3);
    println!("{}", s.peek().unwrap());
}`,
    },
    {
      name: "Stack pop returns 3",
      expected: "3\n",
      code: `{{FUNC}}
fn main() {
    let mut s: Stack<i32> = Stack::new();
    s.push(1); s.push(2); s.push(3);
    println!("{}", s.pop().unwrap());
}`,
    },
    {
      name: "Stack size after two pushes is 2",
      expected: "2\n",
      code: `{{FUNC}}
fn main() {
    let mut s: Stack<i32> = Stack::new();
    s.push(1); s.push(2);
    println!("{}", s.size());
}`,
    },
    {
      name: "Empty stack is_empty returns true",
      expected: "true\n",
      code: `{{FUNC}}
fn main() {
    let s: Stack<i32> = Stack::new();
    println!("{}", s.is_empty());
}`,
    },
  ],
};
