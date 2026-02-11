import type { Lesson } from "../types";

export const genericTypes: Lesson = {
  id: "generic-types",
  title: "Generic Types",
  chapterId: "generics",
  content: `## Generic Structs

Just like functions, struct types can have type parameters. This lets you build reusable data structures that work with any type.

\`\`\`go
type Pair[T any, U any] struct {
    First  T
    Second U
}
\`\`\`

You create instances by specifying the type arguments:

\`\`\`go
p := Pair[string, int]{First: "age", Second: 30}
\`\`\`

### Methods on Generic Types

Methods on a generic type must redeclare the type parameters in the receiver, but they cannot introduce new ones:

\`\`\`go
func (p Pair[T, U]) Swap() Pair[U, T] {
    return Pair[U, T]{First: p.Second, Second: p.First}
}
\`\`\`

### A Generic Container

Here is a practical example: a simple linked list:

\`\`\`go
type Node[T any] struct {
    Value T
    Next  *Node[T]
}

func (n *Node[T]) Append(val T) {
    current := n
    for current.Next != nil {
        current = current.Next
    }
    current.Next = &Node[T]{Value: val}
}
\`\`\`

### Type Constraint Interfaces

You can define interfaces that constrain type parameters to types supporting specific operations:

\`\`\`go
type Number interface {
    int | int8 | int16 | int32 | int64 | float32 | float64
}

type Stats[T Number] struct {
    Values []T
}

func (s Stats[T]) Sum() T {
    var total T
    for _, v := range s.Values {
        total += v
    }
    return total
}
\`\`\`

### Your Task

Implement a generic \`Stack[T any]\` struct backed by a slice. It should have three methods:

- \`Push(val T)\` -- adds a value to the top of the stack
- \`Pop() (T, bool)\` -- removes and returns the top value; returns the zero value of \`T\` and \`false\` if the stack is empty
- \`Peek() (T, bool)\` -- returns the top value without removing it; returns the zero value of \`T\` and \`false\` if the stack is empty`,

  starterCode: `package main

import "fmt"

type Stack[T any] struct {
\titems []T
}

func (s *Stack[T]) Push(val T) {
\t// Your code here
}

func (s *Stack[T]) Pop() (T, bool) {
\t// Your code here
\tvar zero T
\treturn zero, false
}

func (s *Stack[T]) Peek() (T, bool) {
\t// Your code here
\tvar zero T
\treturn zero, false
}

func main() {
\ts := &Stack[int]{}
\ts.Push(10)
\ts.Push(20)
\ts.Push(30)

\tval, ok := s.Peek()
\tfmt.Printf("Peek: %d (%v)\\n", val, ok)

\tval, ok = s.Pop()
\tfmt.Printf("Pop: %d (%v)\\n", val, ok)

\tval, ok = s.Pop()
\tfmt.Printf("Pop: %d (%v)\\n", val, ok)

\tval, ok = s.Pop()
\tfmt.Printf("Pop: %d (%v)\\n", val, ok)

\tval, ok = s.Pop()
\tfmt.Printf("Pop: %d (%v)\\n", val, ok)
}
`,

  solution: `package main

import "fmt"

type Stack[T any] struct {
\titems []T
}

func (s *Stack[T]) Push(val T) {
\ts.items = append(s.items, val)
}

func (s *Stack[T]) Pop() (T, bool) {
\tif len(s.items) == 0 {
\t\tvar zero T
\t\treturn zero, false
\t}
\tval := s.items[len(s.items)-1]
\ts.items = s.items[:len(s.items)-1]
\treturn val, true
}

func (s *Stack[T]) Peek() (T, bool) {
\tif len(s.items) == 0 {
\t\tvar zero T
\t\treturn zero, false
\t}
\treturn s.items[len(s.items)-1], true
}

func main() {
\ts := &Stack[int]{}
\ts.Push(10)
\ts.Push(20)
\ts.Push(30)

\tval, ok := s.Peek()
\tfmt.Printf("Peek: %d (%v)\\n", val, ok)

\tval, ok = s.Pop()
\tfmt.Printf("Pop: %d (%v)\\n", val, ok)

\tval, ok = s.Pop()
\tfmt.Printf("Pop: %d (%v)\\n", val, ok)

\tval, ok = s.Pop()
\tfmt.Printf("Pop: %d (%v)\\n", val, ok)

\tval, ok = s.Pop()
\tfmt.Printf("Pop: %d (%v)\\n", val, ok)
}
`,

  tests: [
    {
      name: "Push and Pop ints",
      code: `package main

import "fmt"

{{FUNC}}

func main() {
\ts := &Stack[int]{}
\ts.Push(10)
\ts.Push(20)
\tval, ok := s.Pop()
\tfmt.Printf("%d %v\\n", val, ok)
\tval, ok = s.Pop()
\tfmt.Printf("%d %v\\n", val, ok)
}`,
      expected: "20 true\n10 true\n",
    },
    {
      name: "Push and Peek strings",
      code: `package main

import "fmt"

{{FUNC}}

func main() {
\ts := &Stack[string]{}
\ts.Push("hello")
\ts.Push("world")
\tval, ok := s.Peek()
\tfmt.Printf("%s %v\\n", val, ok)
\tval, ok = s.Peek()
\tfmt.Printf("%s %v\\n", val, ok)
}`,
      expected: "world true\nworld true\n",
    },
    {
      name: "Pop from empty stack returns zero value and false",
      code: `package main

import "fmt"

{{FUNC}}

func main() {
\ts := &Stack[int]{}
\tval, ok := s.Pop()
\tfmt.Printf("%d %v\\n", val, ok)
}`,
      expected: "0 false\n",
    },
  ],
};
