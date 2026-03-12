import type { Lesson } from "../../types";

export const classes: Lesson = {
  id: "classes",
  title: "Classes",
  chapterId: "oop",
  content: `## Classes and Objects

PHP supports full object-oriented programming:

\`\`\`php
class Dog {
    public $name;
    public $breed;

    public function __construct($name, $breed) {
        $this->name = $name;
        $this->breed = $breed;
    }

    public function bark() {
        return "$this->name says Woof!";
    }
}

$dog = new Dog("Rex", "Labrador");
echo $dog->bark() . "\\n";
\`\`\`

### Your Task

Create a \`Rectangle\` class with \`width\` and \`height\` properties. Add an \`area()\` method and a \`describe()\` method that prints \`WxH = area\`.`,

  starterCode: `<?php
class Rectangle {
    public $width;
    public $height;

    public function __construct($width, $height) {
        $this->width = $width;
        $this->height = $height;
    }

    public function area() {
        return $this->width * $this->height;
    }

    public function describe() {
        return $this->width . "x" . $this->height . " = " . $this->area();
    }
}

$r1 = new Rectangle(3, 4);
$r2 = new Rectangle(5, 6);
echo $r1->describe() . "\\n";
echo $r2->describe() . "\\n";
`,

  solution: `<?php
class Rectangle {
    public $width;
    public $height;

    public function __construct($width, $height) {
        $this->width = $width;
        $this->height = $height;
    }

    public function area() {
        return $this->width * $this->height;
    }

    public function describe() {
        return $this->width . "x" . $this->height . " = " . $this->area();
    }
}

$r1 = new Rectangle(3, 4);
$r2 = new Rectangle(5, 6);
echo $r1->describe() . "\\n";
echo $r2->describe() . "\\n";
`,

  tests: [
    {
      name: "describes rectangles correctly",
      expected: "3x4 = 12\n5x6 = 30\n",
    },
  ],
};
