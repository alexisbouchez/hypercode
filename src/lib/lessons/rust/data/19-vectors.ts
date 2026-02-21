import type { Lesson } from "../../types";

export const vectors: Lesson = {
  id: "vectors",
  title: "Vectors",
  chapterId: "collections",
  content: `## Vectors

\`Vec<T>\` is Rust's growable array. It stores elements contiguously on the heap.

### Creating Vectors

\`\`\`rust
let empty: Vec<i32> = Vec::new();
let v = vec![1, 2, 3, 4, 5];   // macro shorthand
let zeros = vec![0; 10];         // ten zeros
\`\`\`

### Modifying Vectors

\`\`\`rust
let mut v = Vec::new();
v.push(1);
v.push(2);
v.pop();     // removes last — returns Option<T>
v.insert(0, 99); // insert at index
v.remove(0);     // remove at index
v.sort();        // sort in place
v.dedup();       // remove consecutive duplicates (sort first)
v.retain(|&x| x > 0); // keep elements matching predicate
v.extend([4, 5, 6]);   // append elements
\`\`\`

### Accessing Elements

\`\`\`rust
v[0]         // panics if out of bounds
v.get(0)     // returns Option<&T> — safe
v.first()    // Option<&T>
v.last()     // Option<&T>
\`\`\`

### Iteration

\`\`\`rust
for x in &v { println!("{}", x); }
for x in &mut v { *x *= 2; }
for x in v { /* consumes v */ }
\`\`\`

### Partition and Drain

\`\`\`rust
let (evens, odds): (Vec<i32>, Vec<i32>) = v.into_iter().partition(|&x| x % 2 == 0);
\`\`\`

### Your Task

1. \`remove_duplicates(v: Vec<i32>) -> Vec<i32>\` — returns sorted vec with duplicates removed.
2. \`flatten(vecs: Vec<Vec<i32>>) -> Vec<i32>\` — flattens a vec of vecs.
3. \`two_pass_max(v: &[i32]) -> Option<i32>\` — returns the maximum element.
4. \`partition_evens_odds(v: Vec<i32>) -> (Vec<i32>, Vec<i32>)\` — splits into (evens, odds).`,

  starterCode: `fn remove_duplicates(mut v: Vec<i32>) -> Vec<i32> {
    todo!()
}

fn flatten(vecs: Vec<Vec<i32>>) -> Vec<i32> {
    todo!()
}

fn two_pass_max(v: &[i32]) -> Option<i32> {
    todo!()
}

fn partition_evens_odds(v: Vec<i32>) -> (Vec<i32>, Vec<i32>) {
    todo!()
}

fn main() {
    println!("{:?}", remove_duplicates(vec![3, 1, 4, 1, 5, 9, 2, 6, 5]));
    println!("{:?}", flatten(vec![vec![1, 2], vec![3, 4], vec![5]]));
    println!("{:?}", two_pass_max(&[3, 1, 4, 1, 5, 9]));
    let (evens, odds) = partition_evens_odds(vec![1, 2, 3, 4, 5, 6]);
    println!("{:?}", evens);
    println!("{:?}", odds);
}
`,

  solution: `fn remove_duplicates(mut v: Vec<i32>) -> Vec<i32> {
    v.sort();
    v.dedup();
    v
}

fn flatten(vecs: Vec<Vec<i32>>) -> Vec<i32> {
    vecs.into_iter().flatten().collect()
}

fn two_pass_max(v: &[i32]) -> Option<i32> {
    v.iter().copied().max()
}

fn partition_evens_odds(v: Vec<i32>) -> (Vec<i32>, Vec<i32>) {
    v.into_iter().partition(|&x| x % 2 == 0)
}

fn main() {
    println!("{:?}", remove_duplicates(vec![3, 1, 4, 1, 5, 9, 2, 6, 5]));
    println!("{:?}", flatten(vec![vec![1, 2], vec![3, 4], vec![5]]));
    println!("{:?}", two_pass_max(&[3, 1, 4, 1, 5, 9]));
    let (evens, odds) = partition_evens_odds(vec![1, 2, 3, 4, 5, 6]);
    println!("{:?}", evens);
    println!("{:?}", odds);
}
`,

  tests: [
    {
      name: "remove_duplicates returns [1,2,3,4,5,6,9]",
      expected: "[1, 2, 3, 4, 5, 6, 9]\n",
      code: `{{FUNC}}
fn main() {
    println!("{:?}", remove_duplicates(vec![3, 1, 4, 1, 5, 9, 2, 6, 5]));
}`,
    },
    {
      name: "remove_duplicates([1,1,1]) returns [1]",
      expected: "[1]\n",
      code: `{{FUNC}}
fn main() {
    println!("{:?}", remove_duplicates(vec![1, 1, 1]));
}`,
    },
    {
      name: "flatten([[1,2],[3,4],[5]]) returns [1,2,3,4,5]",
      expected: "[1, 2, 3, 4, 5]\n",
      code: `{{FUNC}}
fn main() {
    println!("{:?}", flatten(vec![vec![1, 2], vec![3, 4], vec![5]]));
}`,
    },
    {
      name: "two_pass_max(&[3,1,4,1,5,9]) returns Some(9)",
      expected: "Some(9)\n",
      code: `{{FUNC}}
fn main() {
    println!("{:?}", two_pass_max(&[3, 1, 4, 1, 5, 9]));
}`,
    },
    {
      name: "partition evens of [1,2,3,4,5,6] returns [2,4,6]",
      expected: "[2, 4, 6]\n",
      code: `{{FUNC}}
fn main() {
    let (evens, _odds) = partition_evens_odds(vec![1, 2, 3, 4, 5, 6]);
    println!("{:?}", evens);
}`,
    },
    {
      name: "partition odds of [1,2,3,4,5,6] returns [1,3,5]",
      expected: "[1, 3, 5]\n",
      code: `{{FUNC}}
fn main() {
    let (_evens, odds) = partition_evens_odds(vec![1, 2, 3, 4, 5, 6]);
    println!("{:?}", odds);
}`,
    },
  ],
};
