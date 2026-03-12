import type { Lesson } from "../../types";

export const associativeArrays: Lesson = {
  id: "associative-arrays",
  title: "Associative Arrays",
  chapterId: "data-structures",
  content: `## Associative Arrays

PHP arrays can use string keys (like dictionaries/maps):

\`\`\`php
$person = [
    "name" => "Alice",
    "age" => 30,
    "city" => "Paris",
];
echo $person["name"] . "\\n"; // Alice
\`\`\`

### Modifying

\`\`\`php
$person["email"] = "alice@example.com"; // add
unset($person["city"]);                  // remove
\`\`\`

### Iterating

\`\`\`php
foreach ($person as $key => $value) {
    echo "$key: $value\\n";
}
\`\`\`

### Checking Keys

\`\`\`php
echo array_key_exists("name", $person) ? "yes" : "no";
\`\`\`

### Your Task

Create an associative array for a book with keys \`title\`, \`author\`, and \`year\`. Print each value.`,

  starterCode: `<?php
$book = [
    "title" => "Dune",
    "author" => "Frank Herbert",
    "year" => 1965,
];
echo $book["title"] . "\\n";
echo $book["author"] . "\\n";
echo $book["year"] . "\\n";
`,

  solution: `<?php
$book = [
    "title" => "Dune",
    "author" => "Frank Herbert",
    "year" => 1965,
];
echo $book["title"] . "\\n";
echo $book["author"] . "\\n";
echo $book["year"] . "\\n";
`,

  tests: [
    {
      name: "prints book details",
      expected: "Dune\nFrank Herbert\n1965\n",
    },
  ],
};
