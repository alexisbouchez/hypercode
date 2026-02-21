import type { Lesson } from "../../types";

export const hashmaps: Lesson = {
  id: "hashmaps",
  title: "HashMaps",
  chapterId: "collections",
  content: `## HashMap<K, V>

\`HashMap\` stores key-value pairs with O(1) average-case lookup.

### Creating a HashMap

\`\`\`rust
use std::collections::HashMap;

let mut scores: HashMap<String, i32> = HashMap::new();
\`\`\`

### Inserting and Accessing

\`\`\`rust
scores.insert(String::from("Alice"), 10);
scores.insert(String::from("Bob"), 20);

// Access — panics if key missing
println!("{}", scores["Alice"]);

// Safe access
if let Some(score) = scores.get("Alice") {
    println!("{}", score);
}
\`\`\`

### Entry API

\`\`\`rust
// Insert if not present
scores.entry(String::from("Alice")).or_insert(50);

// Modify existing or insert new
let count = map.entry(word).or_insert(0);
*count += 1;
\`\`\`

### Iterating

\`\`\`rust
for (key, value) in &scores {
    println!("{}: {}", key, value);
}
\`\`\`

Note: HashMap iteration order is **non-deterministic**. Do not rely on order.

### Other Methods

\`\`\`rust
scores.contains_key("Alice")   // true
scores.remove("Bob")           // removes, returns Option<V>
scores.len()                   // number of entries
\`\`\`

### Your Task

1. \`word_count(text: &str) -> HashMap<&str, usize>\` — count occurrences of each word.
2. \`two_sum(nums: &[i32], target: i32) -> Option<(usize, usize)>\` — find two indices that sum to target.
3. \`most_frequent(v: &[i32]) -> i32\` — returns the most frequently occurring element.`,

  starterCode: `use std::collections::HashMap;

fn word_count(text: &str) -> HashMap<&str, usize> {
    todo!()
}

fn two_sum(nums: &[i32], target: i32) -> Option<(usize, usize)> {
    todo!()
}

fn most_frequent(v: &[i32]) -> i32 {
    todo!()
}

fn main() {
    let wc = word_count("the cat sat on the mat");
    println!("{}", wc["the"]);
    println!("{}", wc["cat"]);
    println!("{:?}", two_sum(&[2, 7, 11, 15], 9));
    println!("{}", most_frequent(&[1, 2, 2, 3, 2]));
}
`,

  solution: `use std::collections::HashMap;

fn word_count(text: &str) -> HashMap<&str, usize> {
    let mut map: HashMap<&str, usize> = HashMap::new();
    for word in text.split_whitespace() {
        *map.entry(word).or_insert(0) += 1;
    }
    map
}

fn two_sum(nums: &[i32], target: i32) -> Option<(usize, usize)> {
    let mut seen: HashMap<i32, usize> = HashMap::new();
    for (i, &num) in nums.iter().enumerate() {
        let complement = target - num;
        if let Some(&j) = seen.get(&complement) {
            return Some((j, i));
        }
        seen.insert(num, i);
    }
    None
}

fn most_frequent(v: &[i32]) -> i32 {
    let mut counts: HashMap<i32, usize> = HashMap::new();
    for &x in v {
        *counts.entry(x).or_insert(0) += 1;
    }
    *counts.iter().max_by_key(|(_, &v)| v).unwrap().0
}

fn main() {
    let wc = word_count("the cat sat on the mat");
    println!("{}", wc["the"]);
    println!("{}", wc["cat"]);
    println!("{:?}", two_sum(&[2, 7, 11, 15], 9));
    println!("{}", most_frequent(&[1, 2, 2, 3, 2]));
}
`,

  tests: [
    {
      name: "word_count: \"the\" appears 2 times",
      expected: "2\n",
      code: `{{FUNC}}
fn main() {
    let wc = word_count("the cat sat on the mat");
    println!("{}", wc["the"]);
}`,
    },
    {
      name: "word_count: \"cat\" appears 1 time",
      expected: "1\n",
      code: `{{FUNC}}
fn main() {
    let wc = word_count("the cat sat on the mat");
    println!("{}", wc["cat"]);
}`,
    },
    {
      name: "two_sum(&[2,7,11,15], 9) returns Some((0, 1))",
      expected: "Some((0, 1))\n",
      code: `{{FUNC}}
fn main() {
    println!("{:?}", two_sum(&[2, 7, 11, 15], 9));
}`,
    },
    {
      name: "two_sum(&[3,2,4], 6) returns Some((1, 2))",
      expected: "Some((1, 2))\n",
      code: `{{FUNC}}
fn main() {
    println!("{:?}", two_sum(&[3, 2, 4], 6));
}`,
    },
    {
      name: "most_frequent(&[1,2,2,3,2]) returns 2",
      expected: "2\n",
      code: `{{FUNC}}
fn main() {
    println!("{}", most_frequent(&[1, 2, 2, 3, 2]));
}`,
    },
    {
      name: "most_frequent(&[5,5,3,3,5]) returns 5",
      expected: "5\n",
      code: `{{FUNC}}
fn main() {
    println!("{}", most_frequent(&[5, 5, 3, 3, 5]));
}`,
    },
  ],
};
