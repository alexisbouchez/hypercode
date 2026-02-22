import type { Lesson } from "../../types";

export const buttons: Lesson = {
  id: "buttons",
  title: "Buttons",
  chapterId: "components",
  content: `## Styling Buttons with Tailwind

Interactive states use variant prefixes:

\`\`\`html
<button class="bg-blue-600 hover:bg-blue-700 active:bg-blue-800 text-white px-6 py-3 rounded-lg">
  Click Me
</button>
\`\`\`

**State variants:**
- \`hover:\` — when mouse is over
- \`focus:\` — when keyboard-focused
- \`active:\` — while being clicked
- \`disabled:\` — when disabled

**Transition for smoothness:**
- \`transition-colors\` — animate color changes
- \`transition-all\` — animate all properties
- \`duration-200\` — 200ms transition

**Focus ring for accessibility:**
\`\`\`html
<button class="focus:outline-none focus:ring-2 focus:ring-blue-500">
  Accessible Button
</button>
\`\`\`

**Cursor:**
- \`cursor-pointer\` — hand cursor on hover

### Your Task

Create 3 buttons: primary (blue), secondary (white), and danger (red), each with hover, focus, and active states.`,

  starterCode: `<!-- Create styled buttons with hover, focus, and active states -->
<div class="p-8 space-y-4">

</div>
`,

  solution: `<div class="p-8 space-y-4">
  <button class="bg-blue-600 hover:bg-blue-700 text-white font-semibold px-6 py-3 rounded-lg transition-colors duration-200 focus:outline-none focus:ring-2 focus:ring-blue-500">
    Primary Button
  </button>
  <button class="bg-white hover:bg-gray-50 text-gray-800 font-semibold px-6 py-3 rounded-lg border border-gray-300 transition-colors duration-200">
    Secondary Button
  </button>
  <button class="bg-red-600 hover:bg-red-700 active:bg-red-800 text-white font-semibold px-6 py-3 rounded-lg transition-colors">
    Danger Button
  </button>
</div>
`,

  tests: [
    {
      name: "uses hover state",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/\\bhover:/.test(html));`,
    },
    {
      name: "uses focus state",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/\\bfocus:/.test(html));`,
    },
    {
      name: "uses active state",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/\\bactive:/.test(html));`,
    },
    {
      name: "uses transition",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/\\btransition/.test(html));`,
    },
  ],
};
