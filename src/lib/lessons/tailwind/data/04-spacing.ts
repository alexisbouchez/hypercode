import type { Lesson } from "../../types";

export const spacing: Lesson = {
  id: "spacing",
  title: "Spacing",
  chapterId: "utilities",
  content: `## Tailwind Spacing

Tailwind's spacing scale uses multiples of 4px (0.25rem):
- \`p-1\` = 4px, \`p-2\` = 8px, \`p-4\` = 16px, \`p-8\` = 32px, \`p-16\` = 64px

**Padding shortcuts:**
- \`p-4\` — all sides
- \`px-4\` — left and right (x-axis)
- \`py-4\` — top and bottom (y-axis)
- \`pt-4\`, \`pr-4\`, \`pb-4\`, \`pl-4\` — individual sides

**Margin** works the same:
- \`m-4\`, \`mx-auto\` (center), \`mt-4\`, etc.

**Space between children:**
- \`space-x-4\` — horizontal gap between siblings
- \`space-y-4\` — vertical gap between siblings

\`\`\`html
<div class="p-8 mx-auto">
  <div class="space-y-4">
    <div class="px-6 py-3">Item</div>
  </div>
</div>
\`\`\`

### Your Task

Create a container with padding, centered with \`mx-auto\`, and use \`space-y-4\` on a list of items.`,

  starterCode: `<!-- Use padding, margin, and space-y utilities -->
<div>
  <div>
    <div>Spaced item 1</div>
    <div>Spaced item 2</div>
    <div>Spaced item 3</div>
  </div>
</div>
`,

  solution: `<div class="p-8 m-4 bg-gray-100">
  <div class="mt-4 mb-4 px-6 py-3 bg-white rounded shadow">px/py padding</div>
  <div class="space-y-4">
    <div class="p-4 bg-blue-100">Spaced item 1</div>
    <div class="p-4 bg-blue-100">Spaced item 2</div>
    <div class="p-4 bg-blue-100">Spaced item 3</div>
  </div>
</div>
`,

  tests: [
    {
      name: "uses a padding utility",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/\\bp-\\d/.test(html));`,
    },
    {
      name: "uses a margin utility",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/\\bm[tblrxy]?-\\d/.test(html));`,
    },
    {
      name: "uses space-y utility",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/\\bspace-y-\\d/.test(html));`,
    },
    {
      name: "uses px or py padding",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/\\bp[xy]-\\d/.test(html));`,
    },
  ],
};
