import type { Lesson } from "../types";

export const structs: Lesson = {
  id: "structs",
  title: "Structs and Methods",
  chapterId: "custom-types",
  content: `## Custom Types

Structs are Go's way of grouping related data. If you are coming from an object-oriented language, structs are the closest thing to classes, but without inheritance.

### Defining a Struct

\`\`\`go
type Point struct {
    X float64
    Y float64
}
\`\`\`

### Creating Instances

\`\`\`go
// Named fields (preferred for clarity)
p1 := Point{X: 1.0, Y: 2.0}

// Positional (fragile, avoid unless struct is tiny)
p2 := Point{1.0, 2.0}

// Zero value (all fields are zero-valued)
var p3 Point // {0, 0}
\`\`\`

### Methods

Methods are functions attached to a type. They are declared with a *receiver* between the \`func\` keyword and the method name:

\`\`\`go
func (p Point) Distance() float64 {
    return math.Sqrt(p.X*p.X + p.Y*p.Y)
}
\`\`\`

Call methods with dot notation:

\`\`\`go
p := Point{X: 3, Y: 4}
fmt.Println(p.Distance()) // 5
\`\`\`

### Pointer Receivers

A value receiver gets a copy of the struct. A pointer receiver gets a reference and can modify the original:

\`\`\`go
func (p *Point) Scale(factor float64) {
    p.X *= factor
    p.Y *= factor
}
\`\`\`

Use a pointer receiver when:
- The method needs to modify the struct
- The struct is large and copying would be expensive
- You want consistency (if any method uses a pointer receiver, all should)

Go automatically handles the conversion: you can call a pointer-receiver method on a value, and vice versa.

### Your Task

Define a \`Rect\` struct with fields \`Width\` and \`Height\` (both \`float64\`).

Add two methods:
- \`Area()\` returns the area (\`Width * Height\`)
- \`Perimeter()\` returns the perimeter (\`2 * (Width + Height)\`)`,

  starterCode: `package main

import "fmt"

// Define your Rect struct here

// Add Area() method

// Add Perimeter() method

func main() {
\tr := Rect{Width: 5, Height: 3}
\tfmt.Printf("%.1f\\n", r.Area())
\tfmt.Printf("%.1f\\n", r.Perimeter())
}
`,

  solution: `package main

import "fmt"

type Rect struct {
\tWidth  float64
\tHeight float64
}

func (r Rect) Area() float64 {
\treturn r.Width * r.Height
}

func (r Rect) Perimeter() float64 {
\treturn 2 * (r.Width + r.Height)
}

func main() {
\tr := Rect{Width: 5, Height: 3}
\tfmt.Printf("%.1f\\n", r.Area())
\tfmt.Printf("%.1f\\n", r.Perimeter())
}
`,

  tests: [
    {
      name: "Area of 5x3",
      code: `package main

import "fmt"

{{FUNC}}

func main() {
\tr := Rect{Width: 5, Height: 3}
\tfmt.Printf("%.1f\\n", r.Area())
}`,
      expected: "15.0\n",
    },
    {
      name: "Perimeter of 5x3",
      code: `package main

import "fmt"

{{FUNC}}

func main() {
\tr := Rect{Width: 5, Height: 3}
\tfmt.Printf("%.1f\\n", r.Perimeter())
}`,
      expected: "16.0\n",
    },
    {
      name: "Area of 1x1",
      code: `package main

import "fmt"

{{FUNC}}

func main() {
\tr := Rect{Width: 1, Height: 1}
\tfmt.Printf("%.1f\\n", r.Area())
}`,
      expected: "1.0\n",
    },
    {
      name: "Area of 0 width",
      code: `package main

import "fmt"

{{FUNC}}

func main() {
\tr := Rect{Width: 0, Height: 5}
\tfmt.Printf("%.1f\\n", r.Area())
}`,
      expected: "0.0\n",
    },
  ],
};
