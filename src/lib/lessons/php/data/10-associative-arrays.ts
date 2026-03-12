import type { Lesson } from "../../types";

export const associativeArrays: Lesson = {
  id: "associative-arrays",
  title: "Associative Arrays",
  chapterId: "data-structures",
  content: `## Associative Arrays

PHP's associative arrays map keys to values:

\`\`\`php
$person = [
    "name" => "Alice",
    "age" => 30,
    "city" => "Paris"
];

echo $person["name"] . "\\n"; // Alice
\`\`\`

### Iterating

\`\`\`php
foreach ($person as $key => $value) {
    echo "$key: $value\\n";
}
\`\`\`

### Modifying

\`\`\`php
$person["email"] = "alice@example.com"; // add
unset($person["city"]);                 // remove
\`\`\`

### Useful Functions

\`\`\`php
array_keys($person);          // ["name", "age"]
array_values($person);        // ["Alice", 30]
array_key_exists("name", $person); // true
\`\`\`

### Your Task

Create an associative array for a book with keys \`title\`, \`author\`, and \`year\`. Print each key-value pair as \`key: value\`.`,

  starterCode: `<?php
$book = [
    "title" => "1984",
    "author" => "Orwell",
    "year" => 1949
];

foreach ($book as $key => $value) {
    echo "$key: $value\\n";
}
`,

  solution: `<?php
$book = [
    "title" => "1984",
    "author" => "Orwell",
    "year" => 1949
];

foreach ($book as $key => $value) {
    echo "$key: $value\\n";
}
`,

  tests: [
    {
      name: "prints key-value pairs",
      expected: "title: 1984\nauthor: Orwell\nyear: 1949\n",
    },
  ],
};
