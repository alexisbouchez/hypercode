import type { Lesson } from "../../types";

export const references: Lesson = {
  id: "references",
  title: "References and Borrowing",
  chapterId: "ownership",
  content: `## References and Borrowing

Instead of transferring ownership, you can **borrow** a value by taking a reference to it.

### Immutable References

\`&T\` is a reference to T. You can have as many immutable references as you like:

\`\`\`rust
fn calculate_length(s: &String) -> usize {
    s.len()
} // s is a reference — nothing is dropped here

let s = String::from("hello");
let len = calculate_length(&s); // borrow s
println!("{} has {} letters", s, len); // s still valid
\`\`\`

### Mutable References

\`&mut T\` allows modifying the borrowed value, but with a restriction: **you can have at most one mutable reference to a value in a given scope**:

\`\`\`rust
fn change(s: &mut String) {
    s.push_str(", world");
}
let mut s = String::from("hello");
change(&mut s);
\`\`\`

### The Borrowing Rules

1. At any given time, you can have **either** one mutable reference **or** any number of immutable references — never both.
2. References must always be valid (no dangling pointers).

These rules prevent data races at **compile time** — not at runtime.

### Slices as References

\`&[T]\` is an immutable slice — a reference to a contiguous sequence of T:

\`\`\`rust
fn first_element(slice: &[i32]) -> i32 {
    slice[0]
}
\`\`\`

### Your Task

Implement four functions using references (no ownership transfer):

1. \`sum_slice(numbers: &[i32]) -> i32\` — sum all elements.
2. \`largest_in_slice(numbers: &[i32]) -> i32\` — return the largest element.
3. \`double_all(numbers: &mut Vec<i32>)\` — double every element in-place.
4. \`count_positive(numbers: &[i32]) -> usize\` — count elements greater than zero.`,

  starterCode: `fn sum_slice(numbers: &[i32]) -> i32 {
    todo!()
}

fn largest_in_slice(numbers: &[i32]) -> i32 {
    todo!()
}

fn double_all(numbers: &mut Vec<i32>) {
    todo!()
}

fn count_positive(numbers: &[i32]) -> usize {
    todo!()
}

fn main() {
    let v = vec![1, 2, 3, 4, 5];
    println!("{}", sum_slice(&v));
    println!("{}", largest_in_slice(&v));
    let mut v2 = vec![1, 2, 3];
    double_all(&mut v2);
    println!("{:?}", v2);
    println!("{}", count_positive(&[-1, 2, -3, 4]));
}
`,

  solution: `fn sum_slice(numbers: &[i32]) -> i32 {
    numbers.iter().sum()
}

fn largest_in_slice(numbers: &[i32]) -> i32 {
    *numbers.iter().max().unwrap()
}

fn double_all(numbers: &mut Vec<i32>) {
    for n in numbers.iter_mut() {
        *n *= 2;
    }
}

fn count_positive(numbers: &[i32]) -> usize {
    numbers.iter().filter(|&&x| x > 0).count()
}

fn main() {
    let v = vec![1, 2, 3, 4, 5];
    println!("{}", sum_slice(&v));
    println!("{}", largest_in_slice(&v));
    let mut v2 = vec![1, 2, 3];
    double_all(&mut v2);
    println!("{:?}", v2);
    println!("{}", count_positive(&[-1, 2, -3, 4]));
}
`,

  tests: [
    {
      name: "sum_slice(&[1,2,3,4,5]) returns 15",
      expected: "15\n",
      code: `{{FUNC}}
fn main() {
    println!("{}", sum_slice(&[1, 2, 3, 4, 5]));
}`,
    },
    {
      name: "largest_in_slice(&[3,1,4,1,5,9]) returns 9",
      expected: "9\n",
      code: `{{FUNC}}
fn main() {
    println!("{}", largest_in_slice(&[3, 1, 4, 1, 5, 9]));
}`,
    },
    {
      name: "double_all doubles every element",
      expected: "[2, 4, 6]\n",
      code: `{{FUNC}}
fn main() {
    let mut v = vec![1, 2, 3];
    double_all(&mut v);
    println!("{:?}", v);
}`,
    },
    {
      name: "count_positive(&[-1,2,-3,4]) returns 2",
      expected: "2\n",
      code: `{{FUNC}}
fn main() {
    println!("{}", count_positive(&[-1, 2, -3, 4]));
}`,
    },
  ],
};
