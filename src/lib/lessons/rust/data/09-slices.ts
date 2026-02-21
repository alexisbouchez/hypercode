import type { Lesson } from "../../types";

export const slices: Lesson = {
  id: "slices",
  title: "Slices",
  chapterId: "ownership",
  content: `## Slices

A **slice** is a reference to a contiguous sequence of elements in a collection. Slices do not have ownership.

### Slice Syntax

\`\`\`rust
let a = [1, 2, 3, 4, 5];
let slice = &a[1..3]; // [2, 3] — indices 1 and 2
let all   = &a[..];   // [1, 2, 3, 4, 5]
let first3 = &a[..3]; // [1, 2, 3]
let last2  = &a[3..]; // [4, 5]
\`\`\`

### Slice Type

The type of a slice of \`i32\` values is \`&[i32]\`. Functions that accept slices work with both arrays and vectors:

\`\`\`rust
fn sum(numbers: &[i32]) -> i32 {
    numbers.iter().sum()
}

let arr = [1, 2, 3];
let vec = vec![1, 2, 3];
sum(&arr); // works
sum(&vec); // also works
\`\`\`

### String Slices

String slices (\`&str\`) are references into a \`String\` or string literal:

\`\`\`rust
let s = String::from("hello world");
let hello = &s[0..5]; // "hello"
let world = &s[6..11]; // "world"
\`\`\`

### Common Slice Methods

\`\`\`rust
slice.len()        // number of elements
slice.is_empty()   // true if len() == 0
slice.contains(&x) // true if x is in slice
slice.iter()       // iterator over &T
slice.windows(n)   // overlapping windows of size n
slice.chunks(n)    // non-overlapping chunks of size n
\`\`\`

### Your Task

1. \`rotate_left(v: &[i32], k: usize) -> Vec<i32>\` — rotate the slice left by k positions (elements wrap around).
2. \`max_subarray_sum(arr: &[i32]) -> i32\` — find the contiguous subarray with the largest sum (Kadane's algorithm).
3. \`contains_duplicate(v: &[i32]) -> bool\` — return true if any value appears more than once.`,

  starterCode: `fn rotate_left(v: &[i32], k: usize) -> Vec<i32> {
    todo!()
}

fn max_subarray_sum(arr: &[i32]) -> i32 {
    todo!()
}

fn contains_duplicate(v: &[i32]) -> bool {
    todo!()
}

fn main() {
    println!("{:?}", rotate_left(&[1, 2, 3, 4, 5], 2));
    println!("{}", max_subarray_sum(&[-2, 1, -3, 4, -1, 2, 1, -5, 4]));
    println!("{}", contains_duplicate(&[1, 2, 3, 1]));
}
`,

  solution: `fn rotate_left(v: &[i32], k: usize) -> Vec<i32> {
    if v.is_empty() { return vec![]; }
    let k = k % v.len();
    [&v[k..], &v[..k]].concat()
}

fn max_subarray_sum(arr: &[i32]) -> i32 {
    let mut max_sum = arr[0];
    let mut current = arr[0];
    for &x in &arr[1..] {
        current = x.max(current + x);
        max_sum = max_sum.max(current);
    }
    max_sum
}

fn contains_duplicate(v: &[i32]) -> bool {
    let mut seen = std::collections::HashSet::new();
    v.iter().any(|&x| !seen.insert(x))
}

fn main() {
    println!("{:?}", rotate_left(&[1, 2, 3, 4, 5], 2));
    println!("{}", max_subarray_sum(&[-2, 1, -3, 4, -1, 2, 1, -5, 4]));
    println!("{}", contains_duplicate(&[1, 2, 3, 1]));
}
`,

  tests: [
    {
      name: "rotate_left(&[1,2,3,4,5], 2) returns [3,4,5,1,2]",
      expected: "[3, 4, 5, 1, 2]\n",
      code: `{{FUNC}}
fn main() {
    println!("{:?}", rotate_left(&[1, 2, 3, 4, 5], 2));
}`,
    },
    {
      name: "rotate_left(&[1,2,3], 0) returns [1,2,3]",
      expected: "[1, 2, 3]\n",
      code: `{{FUNC}}
fn main() {
    println!("{:?}", rotate_left(&[1, 2, 3], 0));
}`,
    },
    {
      name: "max_subarray_sum of [-2,1,-3,4,-1,2,1,-5,4] returns 6",
      expected: "6\n",
      code: `{{FUNC}}
fn main() {
    println!("{}", max_subarray_sum(&[-2, 1, -3, 4, -1, 2, 1, -5, 4]));
}`,
    },
    {
      name: "contains_duplicate(&[1,2,3,1]) returns true",
      expected: "true\n",
      code: `{{FUNC}}
fn main() {
    println!("{}", contains_duplicate(&[1, 2, 3, 1]));
}`,
    },
    {
      name: "contains_duplicate(&[1,2,3]) returns false",
      expected: "false\n",
      code: `{{FUNC}}
fn main() {
    println!("{}", contains_duplicate(&[1, 2, 3]));
}`,
    },
  ],
};
