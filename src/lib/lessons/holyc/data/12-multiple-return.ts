import type { Lesson } from "../../types";

export const multipleReturn: Lesson = {
  id: "multiple-return",
  title: "Multiple Return Values",
  chapterId: "functions",
  content: `## Multiple Return Values

HolyC functions can only return a single value, like C. To return multiple values, you use **output parameters** — pointers that the function writes into.

### Output Parameters

Pass pointers as extra arguments, and the function writes its results through them using the dereference operator \`*\`:

\`\`\`holyc
U0 Divide(I64 a, I64 b, I64 *quotient, I64 *remainder) {
  *quotient  = a / b;
  *remainder = a % b;
}

I64 q, r;
Divide(17, 5, &q, &r);
Print("17 / 5 = %d remainder %d\\n", q, r);
// 17 / 5 = 3 remainder 2
\`\`\`

The \`&\` operator takes the **address** of a variable, giving a pointer to it.
The \`*\` operator on the left of an assignment **dereferences** the pointer and writes through it.

### Another Example: Min and Max

\`\`\`holyc
U0 MinMax(I64 a, I64 b, I64 *mn, I64 *mx) {
  if (a < b) {
    *mn = a;
    *mx = b;
  } else {
    *mn = b;
    *mx = a;
  }
}

I64 lo, hi;
MinMax(7, 3, &lo, &hi);
Print("min=%d max=%d\\n", lo, hi);
// min=3 max=7
\`\`\`

### Returning a Struct

For complex return values, you can define a \`class\` and return it by value:

\`\`\`holyc
class Pair {
  I64 first, second;
};

Pair MakePair(I64 a, I64 b) {
  Pair p;
  p.first  = a;
  p.second = b;
  return p;
}

Pair result = MakePair(10, 20);
Print("%d %d\\n", result.first, result.second);
\`\`\`

### Your Task

Write \`U0 Swap(I64 *a, I64 *b)\` that swaps the values at two pointers.

Then declare \`I64 x = 3\` and \`I64 y = 9\`, swap them, and print \`x=%d y=%d\`.

Expected output: \`x=9 y=3\``,

  starterCode: `// Define Swap here

I64 x = 3;
I64 y = 9;
// Call Swap and print x and y
`,

  solution: `U0 Swap(I64 *a, I64 *b) {
  I64 tmp = *a;
  *a = *b;
  *b = tmp;
}

I64 x = 3;
I64 y = 9;
Swap(&x, &y);
Print("x=%d y=%d\\n", x, y);
`,

  tests: [
    {
      name: "swaps correctly",
      expected: "x=9 y=3\n",
    },
  ],
};
