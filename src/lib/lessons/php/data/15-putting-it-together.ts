import type { Lesson } from "../../types";

export const puttingItTogether: Lesson = {
  id: "putting-it-together",
  title: "Putting It Together",
  chapterId: "oop",
  content: `## Putting It Together

Let's combine everything you've learned into a small project.

### Your Task

Create a \`Student\` class with:
- Properties: \`name\` and \`grades\` (an array of numbers)
- A method \`average()\` that returns the average grade
- A method \`status()\` that returns \`"passing"\` if average >= 60, else \`"failing"\`

Then create two students and print their name, average, and status.`,

  starterCode: `<?php
class Student {
    public $name;
    public $grades;

    public function __construct($name, $grades) {
        $this->name = $name;
        $this->grades = $grades;
    }

    public function average() {
        // Your code here
    }

    public function status() {
        // Your code here
    }
}

$alice = new Student("Alice", [90, 85, 92]);
$bob = new Student("Bob", [50, 55, 48]);
echo $alice->name . ": " . $alice->average() . " (" . $alice->status() . ")\\n";
echo $bob->name . ": " . $bob->average() . " (" . $bob->status() . ")\\n";
`,

  solution: `<?php
class Student {
    public $name;
    public $grades;

    public function __construct($name, $grades) {
        $this->name = $name;
        $this->grades = $grades;
    }

    public function average() {
        return array_sum($this->grades) / count($this->grades);
    }

    public function status() {
        return $this->average() >= 60 ? "passing" : "failing";
    }
}

$alice = new Student("Alice", [90, 85, 92]);
$bob = new Student("Bob", [50, 55, 48]);
echo $alice->name . ": " . $alice->average() . " (" . $alice->status() . ")\\n";
echo $bob->name . ": " . $bob->average() . " (" . $bob->status() . ")\\n";
`,

  tests: [
    {
      name: "prints student info correctly",
      expected: "Alice: 89 (passing)\nBob: 51 (failing)\n",
    },
  ],
};
