import type { Lesson } from "../../types";

export const compileTimeExpressions: Lesson = {
  id: "compile-time-expressions",
  title: "Compile-Time Expressions",
  chapterId: "advanced-holyc",
  content: `## Compile-Time Expressions

One of HolyC's most distinctive features is \`#exe\` — a block that runs during **compilation**, not at runtime. In TempleOS, the compiler and runtime share the same environment, so this boundary is unusually thin.

### The #exe Block

\`\`\`holyc
#exe {
  Print("This runs at compile time!\\n");
  I64 x = 6 * 7;
  // x is now available as a compile-time constant
}
\`\`\`

When the compiler encounters a \`#exe\` block, it immediately executes its contents. Output appears before the program "starts". This replaces C's \`#define\` macros with actual executable code.

### Compute Constants at Compile Time

\`\`\`holyc
I64 BUFFER_SIZE;
#exe { BUFFER_SIZE = 1024; }

Print("Buffer: %d bytes\\n", BUFFER_SIZE);
\`\`\`

This is more powerful than \`#define\` because any arbitrary logic can run at compile time:

\`\`\`holyc
I64 MAX_PRIMES;
#exe {
  MAX_PRIMES = 0;
  for (I64 i = 2; i < 100; i++) {
    Bool prime = TRUE;
    for (I64 j = 2; j < i; j++) {
      if (i % j == 0) { prime = FALSE; }
    }
    if (prime) MAX_PRIMES++;
  }
}
Print("Primes below 100: %d\\n", MAX_PRIMES);
\`\`\`

### No #define

HolyC intentionally omits the C preprocessor's \`#define\`. All metaprogramming is done through \`#exe\` and HolyC's direct compiler access. This makes metaprogramming a first-class concept rather than a text-substitution hack.

### Your Task

Use \`#exe\` to compute \`6 * 7\` into a variable \`ANSWER\` at compile time. Then print it at runtime.

Expected output: \`The answer is 42\``,

  starterCode: `I64 ANSWER;
#exe {
  // Compute 6 * 7 and store in ANSWER
}
// Print "The answer is N"
`,

  solution: `I64 ANSWER;
#exe {
  ANSWER = 6 * 7;
}
Print("The answer is %d\\n", ANSWER);
`,

  tests: [
    {
      name: "prints the answer",
      expected: "The answer is 42\n",
    },
  ],
};
