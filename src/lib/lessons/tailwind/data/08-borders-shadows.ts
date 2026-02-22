import type { Lesson } from "../../types";

export const bordersShadows: Lesson = {
  id: "borders-shadows",
  title: "Borders & Shadows",
  chapterId: "layout",
  content: `## Borders and Shadows in Tailwind

**Borders:**
- \`border\` — 1px solid (default color)
- \`border-2\`, \`border-4\`, \`border-8\` — thickness
- \`border-gray-300\`, \`border-blue-500\` — color

**Border radius:**
- \`rounded-none\` — no radius
- \`rounded-sm\`, \`rounded\`, \`rounded-md\`, \`rounded-lg\`, \`rounded-xl\`, \`rounded-2xl\`, \`rounded-3xl\`
- \`rounded-full\` — pill / circle

**Shadows:**
- \`shadow-sm\` — subtle shadow
- \`shadow\` — small shadow
- \`shadow-md\` — medium shadow
- \`shadow-lg\` — large shadow
- \`shadow-xl\`, \`shadow-2xl\` — very large
- \`shadow-none\` — remove shadow
- \`shadow-inner\` — inset shadow

\`\`\`html
<div class="border-2 border-blue-500 rounded-xl shadow-lg p-4">
  Styled card
</div>
\`\`\`

### Your Task

Create 4 elements demonstrating different border styles, rounded corners, and shadow depths.`,

  starterCode: `<!-- Use border, rounded, and shadow utilities -->
<div class="p-6 space-y-4">
  <div class="p-4">Default border</div>
  <div class="p-4">Thick blue border, rounded</div>
  <div class="p-4">Medium shadow</div>
  <div class="p-4">Large shadow</div>
</div>
`,

  solution: `<div class="p-6 space-y-4">
  <div class="border border-gray-300 p-4">Default border</div>
  <div class="border-2 border-blue-500 rounded-lg p-4">Thick blue border, rounded</div>
  <div class="shadow-md rounded-xl p-4">Medium shadow</div>
  <div class="shadow-lg rounded-2xl border border-gray-100 p-4">Large shadow</div>
</div>
`,

  tests: [
    {
      name: "uses border class",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/\\bborder\\b/.test(html));`,
    },
    {
      name: "uses rounded corners",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/\\brounded/.test(html));`,
    },
    {
      name: "uses shadow",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/\\bshadow/.test(html));`,
    },
    {
      name: "uses large rounded corners",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/\\brounded-(xl|2xl|3xl|full)/.test(html));`,
    },
  ],
};
