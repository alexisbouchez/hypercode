import type { Lesson } from "../../types";

export const flexboxTailwind: Lesson = {
  id: "flexbox-tailwind",
  title: "Flexbox",
  chapterId: "layout",
  content: `## Flexbox in Tailwind

Apply flexbox with a single class:

\`\`\`html
<div class="flex items-center justify-between gap-4">
  <div>Left</div>
  <div>Right</div>
</div>
\`\`\`

**Direction:**
- \`flex-row\` (default) — horizontal
- \`flex-col\` — vertical
- \`flex-row-reverse\`, \`flex-col-reverse\`

**Alignment (cross axis):**
- \`items-start\`, \`items-center\`, \`items-end\`, \`items-stretch\`

**Justification (main axis):**
- \`justify-start\`, \`justify-center\`, \`justify-end\`
- \`justify-between\`, \`justify-around\`, \`justify-evenly\`

**Gap:**
- \`gap-4\` — equal gap in both directions
- \`gap-x-4\`, \`gap-y-4\` — axis-specific

**Wrap:**
- \`flex-wrap\` — wrap to next line

**Item utilities:**
- \`flex-1\` — grow to fill
- \`flex-none\` — don't grow or shrink
- \`self-center\` — override alignment for one item

### Your Task

Create a flex row with \`items-center\`, \`justify-between\`, and \`gap-4\` containing 3 colored boxes.`,

  starterCode: `<!-- Create a flexbox layout with Tailwind -->
<div>
  <div>Left</div>
  <div>Center</div>
  <div>Right</div>
</div>
`,

  solution: `<div class="flex flex-row items-center justify-between gap-4 p-6 bg-gray-100">
  <div class="bg-blue-500 text-white p-4 rounded">Left</div>
  <div class="bg-green-500 text-white p-4 rounded">Center</div>
  <div class="bg-red-500 text-white p-4 rounded">Right</div>
</div>
`,

  tests: [
    {
      name: "uses flex class",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/\\bflex\\b/.test(html));`,
    },
    {
      name: "uses items-center",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/\\bitems-center\\b/.test(html));`,
    },
    {
      name: "uses justify-between",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/\\bjustify-between\\b/.test(html));`,
    },
    {
      name: "uses gap utility",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/\\bgap-\\d/.test(html));`,
    },
  ],
};
