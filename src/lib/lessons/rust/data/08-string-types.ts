import type { Lesson } from "../../types";

export const stringTypes: Lesson = {
  id: "string-types",
  title: "String Types",
  chapterId: "ownership",
  content: `## String Types in Rust

Rust has two main string types:

### \`String\` — Owned, heap-allocated

\`String\` is a growable, heap-allocated UTF-8 string. You own it, and it is dropped when it goes out of scope:

\`\`\`rust
let mut s = String::from("hello");
s.push_str(", world"); // mutate in place
s.push('!');
println!("{}", s); // hello, world!
\`\`\`

### \`&str\` — Borrowed string slice

\`&str\` is a reference to a sequence of UTF-8 bytes. String literals like \`"hello"\` are \`&str\`:

\`\`\`rust
let s: &str = "hello"; // string literal — stored in binary
\`\`\`

### Converting Between Them

\`\`\`rust
let owned: String = "hello".to_string();
let owned2: String = String::from("hello");
let borrowed: &str = &owned; // auto-deref
\`\`\`

### Useful String Methods

\`\`\`rust
let s = "Hello, World!";
s.len()               // 13 (bytes, not chars)
s.contains("World")   // true
s.to_lowercase()      // "hello, world!"
s.trim()              // strips whitespace
s.split(", ")         // iterator of parts
s.chars()             // iterator of chars
\`\`\`

### format!

Build strings dynamically with \`format!\` — just like \`println!\` but returns a \`String\`:

\`\`\`rust
let name = "Alice";
let greeting = format!("Hello, {}!", name);
\`\`\`

### Your Task

1. \`is_palindrome(s: &str) -> bool\` — returns true if the string reads the same forward and backward.
2. \`count_vowels(s: &str) -> usize\` — counts vowels (a, e, i, o, u, case-insensitive).
3. \`title_case(s: &str) -> String\` — capitalizes the first letter of each word.`,

  starterCode: `fn is_palindrome(s: &str) -> bool {
    todo!()
}

fn count_vowels(s: &str) -> usize {
    todo!()
}

fn title_case(s: &str) -> String {
    todo!()
}

fn main() {
    println!("{}", is_palindrome("racecar"));
    println!("{}", count_vowels("Hello World"));
    println!("{}", title_case("hello world"));
}
`,

  solution: `fn is_palindrome(s: &str) -> bool {
    let chars: Vec<char> = s.chars().collect();
    let n = chars.len();
    (0..n / 2).all(|i| chars[i] == chars[n - 1 - i])
}

fn count_vowels(s: &str) -> usize {
    s.chars().filter(|c| "aeiouAEIOU".contains(*c)).count()
}

fn title_case(s: &str) -> String {
    s.split_whitespace()
        .map(|word| {
            let mut chars = word.chars();
            match chars.next() {
                None => String::new(),
                Some(first) => first.to_uppercase().to_string() + chars.as_str(),
            }
        })
        .collect::<Vec<_>>()
        .join(" ")
}

fn main() {
    println!("{}", is_palindrome("racecar"));
    println!("{}", count_vowels("Hello World"));
    println!("{}", title_case("hello world"));
}
`,

  tests: [
    {
      name: "is_palindrome(\"racecar\") returns true",
      expected: "true\n",
      code: `{{FUNC}}
fn main() {
    println!("{}", is_palindrome("racecar"));
}`,
    },
    {
      name: "is_palindrome(\"hello\") returns false",
      expected: "false\n",
      code: `{{FUNC}}
fn main() {
    println!("{}", is_palindrome("hello"));
}`,
    },
    {
      name: "count_vowels(\"Hello World\") returns 3",
      expected: "3\n",
      code: `{{FUNC}}
fn main() {
    println!("{}", count_vowels("Hello World"));
}`,
    },
    {
      name: "title_case(\"hello world\") returns Hello World",
      expected: "Hello World\n",
      code: `{{FUNC}}
fn main() {
    println!("{}", title_case("hello world"));
}`,
    },
    {
      name: "title_case(\"rust is great\") returns Rust Is Great",
      expected: "Rust Is Great\n",
      code: `{{FUNC}}
fn main() {
    println!("{}", title_case("rust is great"));
}`,
    },
  ],
};
