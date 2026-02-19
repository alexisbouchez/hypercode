import type { Lesson } from "../../types";

export const comments: Lesson = {
  id: "comments",
  title: "Comments",
  chapterId: "the-temple",
  content: `## Comments

HolyC supports the same comment styles as C.

### Single-Line Comments

Use \`//\` to comment out the rest of a line:

\`\`\`holyc
// This is a comment — the compiler ignores this line
Print("Active code\\n");  // Inline comment
\`\`\`

### Multi-Line Comments

Use \`/* ... */\` to span multiple lines:

\`\`\`holyc
/*
  This is a block comment.
  It can span as many lines as needed.
  The compiler ignores everything between the delimiters.
*/
Print("Running\\n");
\`\`\`

### Why Comment?

Terry Davis wrote TempleOS with extensive comments explaining his design choices, references to scripture, and technical rationale. Good comments make code self-explanatory — they explain *why*, not just *what*.

\`\`\`holyc
// Multiply by 2 using a left shift (faster on some CPUs)
I64 doubled = value << 1;
\`\`\`

### Your Task

Write a program with:
- A single-line comment explaining what the program does
- A block comment with your name
- A \`Print\` call that outputs \`TempleOS\`

The output must be exactly \`TempleOS\` followed by a newline.`,

  starterCode: `// TODO: add a single-line comment

/*
  TODO: add a block comment
*/

// Print "TempleOS"
`,

  solution: `// This program prints the name of the OS
/*
  Written in the spirit of Terry Davis
*/
Print("TempleOS\\n");
`,

  tests: [
    {
      name: "prints TempleOS",
      expected: "TempleOS\n",
    },
  ],
};
