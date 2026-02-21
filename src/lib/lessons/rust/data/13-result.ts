import type { Lesson } from "../../types";

export const result: Lesson = {
  id: "result",
  title: "The Result Type",
  chapterId: "structs-enums",
  content: `## Result<T, E>

\`Result\` is used for recoverable errors. It is Rust's primary error handling mechanism:

\`\`\`rust
enum Result<T, E> {
    Ok(T),
    Err(E),
}
\`\`\`

### Using Result

\`\`\`rust
fn parse_number(s: &str) -> Result<i32, String> {
    s.parse::<i32>().map_err(|e| e.to_string())
}

match parse_number("42") {
    Ok(n) => println!("Got {}", n),
    Err(e) => println!("Error: {}", e),
}
\`\`\`

### Key Methods

\`\`\`rust
result.unwrap()          // panics on Err
result.unwrap_or(0)      // default on Err
result.is_ok()           // true if Ok
result.is_err()          // true if Err
result.map(|v| v * 2)    // transform Ok value
result.map_err(|e| ...)  // transform Err value
\`\`\`

### The ? Operator

In functions returning \`Result\`, \`?\` propagates errors automatically:

\`\`\`rust
fn read_and_parse(s: &str) -> Result<i32, String> {
    let n: i32 = s.parse().map_err(|e: std::num::ParseIntError| e.to_string())?;
    Ok(n * 2)
}
\`\`\`

### Custom Error Types

Define your own error enums for structured errors:

\`\`\`rust
#[derive(Debug)]
enum AppError {
    ParseError(String),
    DivisionByZero,
}
\`\`\`

### Your Task

1. \`checked_divide(a: f64, b: f64) -> Result<f64, MathError>\` — returns \`Err(MathError::DivisionByZero)\` if b is 0.
2. \`checked_sqrt(x: f64) -> Result<f64, MathError>\` — returns \`Err(MathError::NegativeSqrt)\` for negative x.
3. \`parse_positive(s: &str) -> Result<u32, String>\` — parses a non-negative integer.`,

  starterCode: `#[derive(Debug)]
enum MathError {
    DivisionByZero,
    NegativeSqrt,
}

fn checked_divide(a: f64, b: f64) -> Result<f64, MathError> {
    todo!()
}

fn checked_sqrt(x: f64) -> Result<f64, MathError> {
    todo!()
}

fn parse_positive(s: &str) -> Result<u32, String> {
    todo!()
}

fn main() {
    println!("{}", checked_divide(10.0, 2.0).unwrap());
    println!("{}", checked_divide(1.0, 0.0).is_err());
    println!("{:.4}", checked_sqrt(2.0).unwrap());
    println!("{}", parse_positive("42").unwrap());
    println!("{}", parse_positive("abc").is_err());
}
`,

  solution: `#[derive(Debug)]
enum MathError {
    DivisionByZero,
    NegativeSqrt,
}

fn checked_divide(a: f64, b: f64) -> Result<f64, MathError> {
    if b == 0.0 { Err(MathError::DivisionByZero) } else { Ok(a / b) }
}

fn checked_sqrt(x: f64) -> Result<f64, MathError> {
    if x < 0.0 { Err(MathError::NegativeSqrt) } else { Ok(x.sqrt()) }
}

fn parse_positive(s: &str) -> Result<u32, String> {
    s.trim().parse::<u32>().map_err(|e| e.to_string())
}

fn main() {
    println!("{}", checked_divide(10.0, 2.0).unwrap());
    println!("{}", checked_divide(1.0, 0.0).is_err());
    println!("{:.4}", checked_sqrt(2.0).unwrap());
    println!("{}", parse_positive("42").unwrap());
    println!("{}", parse_positive("abc").is_err());
}
`,

  tests: [
    {
      name: "checked_divide(10.0, 2.0) returns 5",
      expected: "5\n",
      code: `{{FUNC}}
fn main() {
    println!("{}", checked_divide(10.0, 2.0).unwrap());
}`,
    },
    {
      name: "checked_divide(1.0, 0.0) is Err",
      expected: "true\n",
      code: `{{FUNC}}
fn main() {
    println!("{}", checked_divide(1.0, 0.0).is_err());
}`,
    },
    {
      name: "checked_sqrt(2.0) returns 1.4142",
      expected: "1.4142\n",
      code: `{{FUNC}}
fn main() {
    println!("{:.4}", checked_sqrt(2.0).unwrap());
}`,
    },
    {
      name: "checked_sqrt(9.0) returns 3",
      expected: "3\n",
      code: `{{FUNC}}
fn main() {
    println!("{}", checked_sqrt(9.0).unwrap());
}`,
    },
    {
      name: "parse_positive(\"42\") returns 42",
      expected: "42\n",
      code: `{{FUNC}}
fn main() {
    println!("{}", parse_positive("42").unwrap());
}`,
    },
    {
      name: "parse_positive(\"abc\") is Err",
      expected: "true\n",
      code: `{{FUNC}}
fn main() {
    println!("{}", parse_positive("abc").is_err());
}`,
    },
  ],
};
