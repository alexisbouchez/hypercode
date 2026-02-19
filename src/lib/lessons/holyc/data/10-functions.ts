import type { Lesson } from "../../types";

export const functions: Lesson = {
  id: "functions",
  title: "Functions",
  chapterId: "functions",
  content: `## Functions

In HolyC, functions are declared by writing the return type first, then the function name, then parameters in parentheses.

### Syntax

\`\`\`holyc
ReturnType FunctionName(ParamType param1, ParamType param2) {
  // body
  return value;
}
\`\`\`

### Example

\`\`\`holyc
I64 Add(I64 a, I64 b) {
  return a + b;
}

I64 result = Add(3, 4);
Print("%d\\n", result);  // 7
\`\`\`

### Void Functions

Use \`U0\` as the return type for functions that return nothing (equivalent to C's \`void\`):

\`\`\`holyc
U0 Greet(U8 *name) {
  Print("Hello, %s!\\n", name);
}

Greet("Terry");  // Hello, Terry!
\`\`\`

### Forward Declarations

In HolyC, functions can call other functions defined later in the file — the compiler handles forward references automatically. This is a convenience over C where you'd need prototypes.

### Recursion

Recursive functions work as expected:

\`\`\`holyc
I64 Factorial(I64 n) {
  if (n <= 1) return 1;
  return n * Factorial(n - 1);
}

Print("%d\\n", Factorial(5));  // 120
\`\`\`

### Your Task

Write a function \`I64 Square(I64 n)\` that returns \`n * n\`. Call it with \`9\` and print the result.

Expected output: \`81\``,

  starterCode: `// Define Square here

// Call it with 9 and print the result
`,

  solution: `I64 Square(I64 n) {
  return n * n;
}

Print("%d\\n", Square(9));
`,

  tests: [
    {
      name: "Square(9) returns 81",
      code: "{{FUNC}}\nPrint(\"%d\\n\", Square(9));",
      expected: "81\n",
    },
    {
      name: "Square(5) returns 25",
      code: "{{FUNC}}\nPrint(\"%d\\n\", Square(5));",
      expected: "25\n",
    },
  ],
};
