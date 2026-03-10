import type { Lesson } from "../../types";

export const functors: Lesson = {
  id: "functors",
  title: "Functors Overview",
  chapterId: "advanced",
  content: `## Functors

A **functor** in OCaml is a module that takes another module as a parameter — essentially a function from modules to modules.

\`\`\`ocaml
module type COMPARABLE = sig
  type t
  val compare : t -> t -> int
end

module MakeSorter (C : COMPARABLE) = struct
  let sort lst = List.sort C.compare lst
end
\`\`\`

You then instantiate it:

\`\`\`ocaml
module IntSorter = MakeSorter(struct
  type t = int
  let compare a b = a - b
end)
\`\`\`

### The Functor Pattern

The key idea is **abstraction over behavior**: you write generic code that works with any module matching a signature. This is similar to generics in other languages but operates at the module level.

### Practicing the Pattern

We can express the functor concept using higher-order functions — pass a comparison function to create specialized sort/search operations.

### Your Task

Write a function \`make_sorter\` that takes a comparison function and returns a sorting function. Then use it to create \`sort_asc\` and \`sort_desc\`.`,

  starterCode: `let make_sorter cmp = fun lst -> List.sort cmp lst

let sort_asc = make_sorter (fun a b -> compare a b)
let sort_desc = make_sorter (fun a b -> compare b a)

let rec show_list lst =
  if List.length lst = 0 then print_newline ()
  else (print_int (List.hd lst); print_string " "; show_list (List.tl lst))

show_list (sort_asc [3; 1; 4; 1; 5])
show_list (sort_desc [3; 1; 4; 1; 5])
`,

  solution: `let make_sorter cmp = fun lst -> List.sort cmp lst

let sort_asc = make_sorter (fun a b -> compare a b)
let sort_desc = make_sorter (fun a b -> compare b a)

let rec show_list lst =
  if List.length lst = 0 then print_newline ()
  else (print_int (List.hd lst); print_string " "; show_list (List.tl lst))

show_list (sort_asc [3; 1; 4; 1; 5])
show_list (sort_desc [3; 1; 4; 1; 5])
`,

  tests: [
    {
      name: "sort ascending",
      expected: "1 1 3 4 5 \n",
      code: `{{FUNC}}
show_list (sort_asc [3; 1; 4; 1; 5])
`,
    },
    {
      name: "sort descending",
      expected: "5 4 3 1 1 \n",
      code: `{{FUNC}}
show_list (sort_desc [3; 1; 4; 1; 5])
`,
    },
    {
      name: "sort empty list",
      expected: "\n",
      code: `{{FUNC}}
show_list (sort_asc [])
`,
    },
  ],
};
