import type { Lesson } from "../../types";

export const colorUtilities: Lesson = {
  id: "color-utilities",
  title: "Color Utilities",
  chapterId: "utilities",
  content: `## Tailwind Color System

Tailwind includes a curated color palette. Colors follow the pattern \`{color}-{shade}\`:

**Colors**: slate, gray, zinc, neutral, stone, red, orange, amber, yellow, lime, green, emerald, teal, cyan, sky, blue, indigo, violet, purple, fuchsia, pink, rose, white, black

**Shades**: 50, 100, 200, 300, 400, 500, 600, 700, 800, 900, 950

\`\`\`html
<div class="bg-blue-500">       blue background    </div>
<div class="text-red-600">      red text           </div>
<div class="border-green-400">  green border       </div>
\`\`\`

**Light/dark variants:**
- \`bg-blue-100\` — very light blue (good for backgrounds)
- \`bg-blue-500\` — medium blue (good for buttons)
- \`bg-blue-900\` — very dark blue

### Your Task

Create 4 elements using different color combinations: bg and text colors from at least 3 different color families, and at least one border color.`,

  starterCode: `<!-- Use Tailwind color utilities for backgrounds, text, and borders -->
<div class="p-4 space-y-4">
  <div class="p-3 rounded">Red palette</div>
  <div class="p-3 rounded">Green palette</div>
  <div class="p-3 rounded">Blue with border</div>
  <div class="p-3 rounded">Purple background</div>
</div>
`,

  solution: `<div class="p-4 space-y-4">
  <div class="bg-red-100 text-red-800 p-3 rounded">Red palette</div>
  <div class="bg-green-100 text-green-800 p-3 rounded">Green palette</div>
  <div class="bg-blue-100 text-blue-800 p-3 rounded border border-blue-300">Blue with border</div>
  <div class="bg-purple-500 text-white p-3 rounded">Purple background</div>
</div>
`,

  tests: [
    {
      name: "uses a bg color with shade",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/\\bbg-[a-z]+-\\d+/.test(html));`,
    },
    {
      name: "uses a text color with shade",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/\\btext-[a-z]+-\\d+/.test(html));`,
    },
    {
      name: "uses a border color",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/\\bborder-[a-z]+-\\d+/.test(html));`,
    },
    {
      name: "uses at least 3 bg color classes",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log((html.match(/\\bbg-[a-z]+-\\d+/g)||[]).length >= 3);`,
    },
  ],
};
