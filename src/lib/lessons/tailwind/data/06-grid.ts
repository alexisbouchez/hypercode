import type { Lesson } from "../../types";

export const gridTailwind: Lesson = {
  id: "grid-tailwind",
  title: "Grid",
  chapterId: "layout",
  content: `## CSS Grid in Tailwind

Create grids with a few utilities:

\`\`\`html
<div class="grid grid-cols-3 gap-4">
  <div>1</div>
  <div>2</div>
  <div>3</div>
</div>
\`\`\`

**Columns:**
- \`grid-cols-1\` through \`grid-cols-12\`
- \`grid-cols-none\` — no grid

**Gap:**
- \`gap-4\`, \`gap-x-4\`, \`gap-y-4\`

**Spanning:**
- \`col-span-2\` — span 2 columns
- \`col-span-full\` — full width
- \`row-span-2\` — span 2 rows

**Auto columns:**
- \`auto-cols-fr\` — equal width auto columns

### Your Task

Create a 3-column grid with \`gap\` and at least one item with \`col-span-2\`.`,

  starterCode: `<!-- Create a CSS grid with Tailwind -->
<div>
  <div>Spans 2 columns</div>
  <div>1 column</div>
  <div>1 column</div>
  <div>1 column</div>
  <div>1 column</div>
</div>
`,

  solution: `<div class="grid grid-cols-3 gap-4 p-6">
  <div class="bg-blue-100 p-4 rounded col-span-2">Spans 2 columns</div>
  <div class="bg-green-100 p-4 rounded">1 column</div>
  <div class="bg-yellow-100 p-4 rounded">1 column</div>
  <div class="bg-red-100 p-4 rounded">1 column</div>
  <div class="bg-purple-100 p-4 rounded">1 column</div>
</div>
`,

  tests: [
    {
      name: "uses grid class",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/\\bgrid\\b/.test(html));`,
    },
    {
      name: "uses grid-cols",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/\\bgrid-cols-\\d/.test(html));`,
    },
    {
      name: "uses col-span",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/\\bcol-span-\\d/.test(html));`,
    },
    {
      name: "uses gap utility",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/\\bgap-\\d/.test(html));`,
    },
  ],
};
