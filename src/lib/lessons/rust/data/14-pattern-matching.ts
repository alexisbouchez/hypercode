import type { Lesson } from "../../types";

export const patternMatching: Lesson = {
  id: "pattern-matching",
  title: "Pattern Matching",
  chapterId: "structs-enums",
  content: `## Pattern Matching

\`match\` is one of Rust's most powerful features. It compares a value against a series of patterns and executes the first that matches.

### Range Patterns

\`\`\`rust
match n {
    1..=5 => "one to five",
    6..=10 => "six to ten",
    _ => "other",
}
\`\`\`

### Guard Clauses

Add \`if\` conditions to match arms:

\`\`\`rust
match pair {
    (x, y) if x == y => "equal",
    (x, y) if x > y => "first larger",
    _ => "second larger",
}
\`\`\`

### Destructuring

Match can destructure tuples, structs, enums, and slices:

\`\`\`rust
// Destructure a tuple
let (a, b) = (1, 2);

// Destructure in match
match point {
    Point { x: 0, y } => println!("On y-axis at {}", y),
    Point { x, y: 0 } => println!("On x-axis at {}", x),
    Point { x, y } => println!("At ({}, {})", x, y),
}

// Slice patterns
match slice {
    [] => "empty",
    [x] => "one element",
    [first, .., last] => "many elements",
}
\`\`\`

### if let

For matching a single pattern, \`if let\` is more concise:

\`\`\`rust
if let Some(n) = maybe_number {
    println!("Got {}", n);
}
\`\`\`

### while let

\`\`\`rust
while let Some(top) = stack.pop() {
    println!("{}", top);
}
\`\`\`

### Your Task

1. \`classify(n: i32) -> &'static str\` — "negative", "zero", "small" (1-100), or "large".
2. \`describe_pair(pair: (i32, i32)) -> String\` — "origin", "x:N", "y:N", "diag:N", or "(x,y)".
3. \`head_tail(v: &[i32]) -> String\` — "empty", "single:N", or "head:N tail:N".`,

  starterCode: `fn classify(n: i32) -> &'static str {
    todo!()
}

fn describe_pair(pair: (i32, i32)) -> String {
    todo!()
}

fn head_tail(v: &[i32]) -> String {
    todo!()
}

fn main() {
    println!("{}", classify(-5));
    println!("{}", classify(0));
    println!("{}", describe_pair((3, 3)));
    println!("{}", head_tail(&[1, 2, 3, 4, 5]));
}
`,

  solution: `fn classify(n: i32) -> &'static str {
    match n {
        i32::MIN..=-1 => "negative",
        0 => "zero",
        1..=100 => "small",
        _ => "large",
    }
}

fn describe_pair(pair: (i32, i32)) -> String {
    match pair {
        (0, 0) => "origin".to_string(),
        (x, 0) => format!("x:{}", x),
        (0, y) => format!("y:{}", y),
        (x, y) if x == y => format!("diag:{}", x),
        (x, y) => format!("({},{})", x, y),
    }
}

fn head_tail(v: &[i32]) -> String {
    match v {
        [] => "empty".to_string(),
        [x] => format!("single:{}", x),
        [head, .., tail] => format!("head:{} tail:{}", head, tail),
    }
}

fn main() {
    println!("{}", classify(-5));
    println!("{}", classify(0));
    println!("{}", describe_pair((3, 3)));
    println!("{}", head_tail(&[1, 2, 3, 4, 5]));
}
`,

  tests: [
    {
      name: "classify(-5) returns negative",
      expected: "negative\n",
      code: `{{FUNC}}
fn main() {
    println!("{}", classify(-5));
}`,
    },
    {
      name: "classify(0) returns zero",
      expected: "zero\n",
      code: `{{FUNC}}
fn main() {
    println!("{}", classify(0));
}`,
    },
    {
      name: "classify(50) returns small",
      expected: "small\n",
      code: `{{FUNC}}
fn main() {
    println!("{}", classify(50));
}`,
    },
    {
      name: "classify(200) returns large",
      expected: "large\n",
      code: `{{FUNC}}
fn main() {
    println!("{}", classify(200));
}`,
    },
    {
      name: "describe_pair((0,0)) returns origin",
      expected: "origin\n",
      code: `{{FUNC}}
fn main() {
    println!("{}", describe_pair((0, 0)));
}`,
    },
    {
      name: "describe_pair((3,3)) returns diag:3",
      expected: "diag:3\n",
      code: `{{FUNC}}
fn main() {
    println!("{}", describe_pair((3, 3)));
}`,
    },
    {
      name: "describe_pair((2,5)) returns (2,5)",
      expected: "(2,5)\n",
      code: `{{FUNC}}
fn main() {
    println!("{}", describe_pair((2, 5)));
}`,
    },
    {
      name: "head_tail(&[1,2,3,4,5]) returns head:1 tail:5",
      expected: "head:1 tail:5\n",
      code: `{{FUNC}}
fn main() {
    println!("{}", head_tail(&[1, 2, 3, 4, 5]));
}`,
    },
    {
      name: "head_tail(&[]) returns empty",
      expected: "empty\n",
      code: `{{FUNC}}
fn main() {
    println!("{}", head_tail(&[]));
}`,
    },
  ],
};
