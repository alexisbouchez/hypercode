import type { Lesson } from "../../types";

export const modules: Lesson = {
  id: "modules",
  title: "Modules",
  chapterId: "advanced",
  content: `## Modules

OCaml's module system organizes code into namespaced units. You have already been using modules like \`List\` and \`String\`:

\`\`\`ocaml
List.length [1; 2; 3]     (* 3 *)
String.length "hello"     (* 5 *)
\`\`\`

### Defining Modules

\`\`\`ocaml
module Math = struct
  let pi = 3.14159
  let square x = x * x
end

let () = print_int (Math.square 5)
\`\`\`

### Module Signatures

Signatures define the interface:

\`\`\`ocaml
module type STACK = sig
  type 'a t
  val empty : 'a t
  val push : 'a -> 'a t -> 'a t
  val pop : 'a t -> 'a t
end
\`\`\`

### Practicing the Pattern

Since our runtime uses a simplified OCaml subset, we will practice the module concept by building a small "stack" library using lists and functions — the same approach modules use internally.

### Your Task

Implement a stack using lists: \`stack_empty\`, \`stack_push\`, \`stack_pop\`, \`stack_top\`, and \`stack_size\`.`,

  starterCode: `let stack_push x s = x :: s

let stack_pop s = List.tl s

let stack_top s = List.hd s

let stack_size s = List.length s

print_int (stack_top (stack_push 30 (stack_push 20 (stack_push 10 []))))
print_newline ()
print_int (stack_size (stack_push 30 (stack_push 20 (stack_push 10 []))))
print_newline ()
print_int (stack_top (stack_pop (stack_push 30 (stack_push 20 (stack_push 10 [])))))
print_newline ()
`,

  solution: `let stack_push x s = x :: s

let stack_pop s = List.tl s

let stack_top s = List.hd s

let stack_size s = List.length s

print_int (stack_top (stack_push 30 (stack_push 20 (stack_push 10 []))))
print_newline ()
print_int (stack_size (stack_push 30 (stack_push 20 (stack_push 10 []))))
print_newline ()
print_int (stack_top (stack_pop (stack_push 30 (stack_push 20 (stack_push 10 [])))))
print_newline ()
`,

  tests: [
    {
      name: "top of stack is 30",
      expected: "30\n",
      code: `{{FUNC}}
print_int (stack_top (stack_push 30 (stack_push 20 (stack_push 10 []))))
print_newline ()
`,
    },
    {
      name: "stack size is 3",
      expected: "3\n",
      code: `{{FUNC}}
print_int (stack_size (stack_push 30 (stack_push 20 (stack_push 10 []))))
print_newline ()
`,
    },
    {
      name: "pop then top gives 20",
      expected: "20\n",
      code: `{{FUNC}}
print_int (stack_top (stack_pop (stack_push 30 (stack_push 20 (stack_push 10 [])))))
print_newline ()
`,
    },
  ],
};
