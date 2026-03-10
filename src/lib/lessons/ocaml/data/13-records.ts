import type { Lesson } from "../../types";

export const records: Lesson = {
  id: "records",
  title: "Records",
  chapterId: "types",
  content: `## Records

In OCaml, records group named fields into a single value:

\`\`\`ocaml
type person = { name : string; age : int }
let alice = { name = "Alice"; age = 30 }
\`\`\`

You access fields with dot notation: \`alice.name\`.

### Simulating Records

Since our transpiler uses a simplified subset, we will represent records as tuples and use accessor functions — a common functional pattern:

\`\`\`ocaml
(* person = (name, age) *)
let make_person name age = (name, age)
let person_name p = fst p
let person_age p = snd p
\`\`\`

This is how many functional programs work internally — records are syntactic sugar over tuples with named accessors.

### Your Task

Create a "point" abstraction using tuples: \`make_point\`, \`point_x\`, \`point_y\`, and a \`manhattan\` function that computes the Manhattan distance between two points (\`|x1-x2| + |y1-y2|\`).`,

  starterCode: `let make_point x y = (x, y)
let point_x p = fst p
let point_y p = snd p

let manhattan p1 p2 =
  abs (point_x p1 - point_x p2) + abs (point_y p1 - point_y p2)

print_int (manhattan (make_point 1 2) (make_point 4 6))
print_newline ()
`,

  solution: `let make_point x y = (x, y)
let point_x p = fst p
let point_y p = snd p

let manhattan p1 p2 =
  abs (point_x p1 - point_x p2) + abs (point_y p1 - point_y p2)

print_int (manhattan (make_point 1 2) (make_point 4 6))
print_newline ()
`,

  tests: [
    {
      name: "manhattan (1,2) (4,6) = 7",
      expected: "7\n",
      code: `{{FUNC}}
let a = make_point 1 2
let b = make_point 4 6
print_int (manhattan a b)
print_newline ()
`,
    },
    {
      name: "manhattan (0,0) (0,0) = 0",
      expected: "0\n",
      code: `{{FUNC}}
let a = make_point 0 0
let b = make_point 0 0
print_int (manhattan a b)
print_newline ()
`,
    },
    {
      name: "point_x and point_y work",
      expected: "3\n5\n",
      code: `{{FUNC}}
let p = make_point 3 5
print_int (point_x p)
print_newline ()
print_int (point_y p)
print_newline ()
`,
    },
  ],
};
