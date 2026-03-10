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

### \`scan()\` vs \`fold()\`

Both carry state through an iteration, but they serve different purposes:

**\`fold()\`** consumes the entire iterator and returns a single final value:

\`\`\`rust
let total = vec![1, 2, 3].iter().fold(0, |acc, &x| acc + x);
// total = 6
\`\`\`

**\`scan()\`** is like \`fold()\` but it is lazy and yields each intermediate state, producing a new iterator:

\`\`\`rust
let running: Vec<i32> = vec![1, 2, 3].iter()
    .scan(0, |acc, &x| { *acc += x; Some(*acc) })
    .collect();
// running = [1, 3, 6]
\`\`\`

Key differences:
- \`fold()\` returns one value. \`scan()\` returns an iterator of intermediate values.
- \`fold()\` is eager (consumes the iterator). \`scan()\` is lazy (produces an adapter).
- \`scan()\` can stop early by returning \`None\`.

### Your Task

1. \`sum_of_squares(n: u32) -> u32\` — sum of squares from 1 to n.
2. \`filter_evens(v: &[i32]) -> Vec<i32>\` — keep only even numbers.
3. \`running_sum(v: &[i32]) -> Vec<i32>\` — running cumulative sum (use \`scan()\`).
4. \`flat_zip(a: &[i32], b: &[i32]) -> Vec<i32>\` — interleave two slices: [a0,b0,a1,b1,...].
5. \`product_with_fold(v: &[i32]) -> i32\` — compute the product of all elements using \`fold()\`.
6. \`running_max(v: &[i32]) -> Vec<i32>\` — running maximum using \`scan()\`: for each position, the max of all elements up to and including that position.`,

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

fn product_with_fold(v: &[i32]) -> i32 {
    todo!()
}

fn running_max(v: &[i32]) -> Vec<i32> {
    todo!()
}

fn main() {
    println!("{}", sum_of_squares(5));
    println!("{:?}", filter_evens(&[1, 2, 3, 4, 5, 6]));
    println!("{:?}", running_sum(&[1, 2, 3, 4]));
    println!("{:?}", flat_zip(&[1, 3], &[2, 4]));
    println!("{}", product_with_fold(&[2, 3, 4]));
    println!("{:?}", running_max(&[3, 1, 4, 1, 5]));
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

fn product_with_fold(v: &[i32]) -> i32 {
    v.iter().fold(1, |acc, &x| acc * x)
}

fn running_max(v: &[i32]) -> Vec<i32> {
    v.iter().scan(i32::MIN, |max, &x| { *max = (*max).max(x); Some(*max) }).collect()
}

fn main() {
    println!("{}", sum_of_squares(5));
    println!("{:?}", filter_evens(&[1, 2, 3, 4, 5, 6]));
    println!("{:?}", running_sum(&[1, 2, 3, 4]));
    println!("{:?}", flat_zip(&[1, 3], &[2, 4]));
    println!("{}", product_with_fold(&[2, 3, 4]));
    println!("{:?}", running_max(&[3, 1, 4, 1, 5]));
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
    {
      name: "product_with_fold([2,3,4]) returns 24",
      expected: "24\n",
      code: `{{FUNC}}
fn main() {
    println!("{}", product_with_fold(&[2, 3, 4]));
}`,
    },
    {
      name: "product_with_fold on empty slice returns 1 (identity)",
      expected: "1\n",
      code: `{{FUNC}}
fn main() {
    println!("{}", product_with_fold(&[]));
}`,
    },
    {
      name: "running_max([3,1,4,1,5]) returns [3,3,4,4,5]",
      expected: "[3, 3, 4, 4, 5]\n",
      code: `{{FUNC}}
fn main() {
    println!("{:?}", running_max(&[3, 1, 4, 1, 5]));
}`,
    },
    {
      name: "running_max on descending [5,3,1] returns [5,5,5]",
      expected: "[5, 5, 5]\n",
      code: `{{FUNC}}
fn main() {
    println!("{:?}", running_max(&[5, 3, 1]));
}`,
    },
  ],
};
