import type { Lesson } from "../../types";

export const iterators: Lesson = {
  id: "iterators",
  title: "Iterators",
  chapterId: "traits-generics",
  content: `## Iterators

Iterators are Rust's primary tool for processing sequences of values. They are lazy — they produce values on demand.

### The Iterator Trait

\`\`\`rust
trait Iterator {
    type Item;
    fn next(&mut self) -> Option<Self::Item>;
    // ... many provided methods
}
\`\`\`

### Creating Iterators

\`\`\`rust
let v = vec![1, 2, 3];
v.iter()       // borrows — yields &T
v.iter_mut()   // mutable borrow — yields &mut T
v.into_iter()  // consumes — yields T
(1..=5)        // range — yields integers
\`\`\`

### Adapter Methods (lazy)

\`\`\`rust
iter.map(|x| x * 2)           // transform each element
iter.filter(|x| x % 2 == 0)   // keep elements matching predicate
iter.enumerate()               // (index, value) pairs
iter.zip(other)                // pair elements from two iterators
iter.flat_map(|x| some_iter)   // map then flatten
iter.take(n)                   // first n elements
iter.skip(n)                   // skip first n elements
iter.chain(other)              // concatenate two iterators
iter.scan(init, f)             // running state
\`\`\`

### Consumer Methods (eager)

\`\`\`rust
iter.collect::<Vec<_>>()       // collect into a collection
iter.sum::<i32>()              // sum all elements
iter.count()                   // count elements
iter.max()                     // maximum Option<T>
iter.min()                     // minimum Option<T>
iter.any(|x| x > 0)           // true if any matches
iter.all(|x| x > 0)           // true if all match
iter.find(|x| x > 0)          // first matching element
iter.fold(init, |acc, x| ...) // reduce with accumulator
\`\`\`

### Your Task

1. \`sum_of_squares(n: u32) -> u32\` — sum of squares from 1 to n.
2. \`filter_evens(v: &[i32]) -> Vec<i32>\` — keep only even numbers.
3. \`running_sum(v: &[i32]) -> Vec<i32>\` — running cumulative sum.
4. \`flat_zip(a: &[i32], b: &[i32]) -> Vec<i32>\` — interleave two slices: [a0,b0,a1,b1,...].`,

  starterCode: `fn sum_of_squares(n: u32) -> u32 {
    todo!()
}

fn filter_evens(v: &[i32]) -> Vec<i32> {
    todo!()
}

fn running_sum(v: &[i32]) -> Vec<i32> {
    todo!()
}

fn flat_zip(a: &[i32], b: &[i32]) -> Vec<i32> {
    todo!()
}

fn main() {
    println!("{}", sum_of_squares(5));
    println!("{:?}", filter_evens(&[1, 2, 3, 4, 5, 6]));
    println!("{:?}", running_sum(&[1, 2, 3, 4]));
    println!("{:?}", flat_zip(&[1, 3], &[2, 4]));
}
`,

  solution: `fn sum_of_squares(n: u32) -> u32 {
    (1..=n).map(|x| x * x).sum()
}

fn filter_evens(v: &[i32]) -> Vec<i32> {
    v.iter().copied().filter(|&x| x % 2 == 0).collect()
}

fn running_sum(v: &[i32]) -> Vec<i32> {
    v.iter().scan(0, |acc, &x| { *acc += x; Some(*acc) }).collect()
}

fn flat_zip(a: &[i32], b: &[i32]) -> Vec<i32> {
    a.iter().zip(b.iter()).flat_map(|(&x, &y)| [x, y]).collect()
}

fn main() {
    println!("{}", sum_of_squares(5));
    println!("{:?}", filter_evens(&[1, 2, 3, 4, 5, 6]));
    println!("{:?}", running_sum(&[1, 2, 3, 4]));
    println!("{:?}", flat_zip(&[1, 3], &[2, 4]));
}
`,

  tests: [
    {
      name: "sum_of_squares(5) returns 55",
      expected: "55\n",
      code: `{{FUNC}}
fn main() {
    println!("{}", sum_of_squares(5));
}`,
    },
    {
      name: "sum_of_squares(3) returns 14",
      expected: "14\n",
      code: `{{FUNC}}
fn main() {
    println!("{}", sum_of_squares(3));
}`,
    },
    {
      name: "filter_evens returns [2,4,6]",
      expected: "[2, 4, 6]\n",
      code: `{{FUNC}}
fn main() {
    println!("{:?}", filter_evens(&[1, 2, 3, 4, 5, 6]));
}`,
    },
    {
      name: "running_sum returns [1,3,6,10]",
      expected: "[1, 3, 6, 10]\n",
      code: `{{FUNC}}
fn main() {
    println!("{:?}", running_sum(&[1, 2, 3, 4]));
}`,
    },
    {
      name: "flat_zip([1,3],[2,4]) returns [1,2,3,4]",
      expected: "[1, 2, 3, 4]\n",
      code: `{{FUNC}}
fn main() {
    println!("{:?}", flat_zip(&[1, 3], &[2, 4]));
}`,
    },
  ],
};
