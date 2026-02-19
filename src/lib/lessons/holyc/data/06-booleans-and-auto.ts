import type { Lesson } from "../../types";

export const booleansAndAuto: Lesson = {
  id: "booleans-and-auto",
  title: "Booleans and Auto",
  chapterId: "types-and-variables",
  content: `## Booleans and Auto

### The Bool Type

HolyC has a \`Bool\` type (capital B) with two constants: \`TRUE\` and \`FALSE\`.

\`\`\`holyc
Bool is_ready = TRUE;
Bool is_done  = FALSE;
\`\`\`

Internally, \`Bool\` is an integer: \`TRUE\` is \`1\` and \`FALSE\` is \`0\`. You can print booleans with \`%d\`:

\`\`\`holyc
Bool flag = TRUE;
Print("%d\\n", flag);  // prints: 1
\`\`\`

### Using Booleans in Conditions

Booleans work naturally in \`if\` statements:

\`\`\`holyc
Bool found = TRUE;
if (found) {
  Print("Found it!\\n");
}

Bool empty = FALSE;
if (!empty) {
  Print("Not empty\\n");
}
\`\`\`

### Auto Type Inference

The \`auto\` keyword lets the compiler infer the type from the initializer — similar to \`var\` in other languages:

\`\`\`holyc
auto count = 42;         // inferred as I64
auto message = "hello";  // inferred as U8* (string pointer)
auto ratio = 3.14;       // inferred as F64
\`\`\`

\`auto\` is useful when the type is obvious from context and you want to avoid repetition.

### Your Task

Declare a \`Bool\` variable \`logged_in = TRUE\` and an \`auto\` variable \`username = "terry"\`.

If \`logged_in\` is true, print: \`Welcome, terry\`

Otherwise print: \`Access denied\``,

  starterCode: `Bool logged_in = TRUE;
auto username = "terry";
// Print the appropriate message
`,

  solution: `Bool logged_in = TRUE;
auto username = "terry";
if (logged_in) {
  Print("Welcome, %s\\n", username);
} else {
  Print("Access denied\\n");
}
`,

  tests: [
    {
      name: "prints welcome message",
      expected: "Welcome, terry\n",
    },
  ],
};
