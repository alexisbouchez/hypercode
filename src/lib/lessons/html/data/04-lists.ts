import type { Lesson } from "../../types";

export const lists: Lesson = {
  id: "lists",
  title: "Lists",
  chapterId: "structure",
  content: `## Unordered and Ordered Lists

HTML has two main list types:

**Unordered list** (\`<ul>\`) — bullets:
\`\`\`html
<ul>
  <li>Apples</li>
  <li>Bananas</li>
  <li>Oranges</li>
</ul>
\`\`\`

**Ordered list** (\`<ol>\`) — numbers:
\`\`\`html
<ol>
  <li>First step</li>
  <li>Second step</li>
  <li>Third step</li>
</ol>
\`\`\`

Each item uses the \`<li>\` (list item) tag. Lists can be nested by placing a \`<ul>\` or \`<ol>\` inside an \`<li>\`.

### Your Task

Create:
- An unordered list with at least 3 items
- An ordered list with at least 2 items`,

  starterCode: `<!-- Create an unordered list and an ordered list -->
`,

  solution: `<ul>
  <li>Item one</li>
  <li>Item two</li>
  <li>Item three</li>
</ul>
<ol>
  <li>First</li>
  <li>Second</li>
</ol>
`,

  tests: [
    {
      name: "has an unordered list",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/<ul>/i.test(html));`,
    },
    {
      name: "has an ordered list",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/<ol>/i.test(html));`,
    },
    {
      name: "has at least 3 list items",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log((html.match(/<li>/gi)||[]).length >= 3);`,
    },
    {
      name: "uses the li tag",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/<li>/i.test(html));`,
    },
  ],
};
