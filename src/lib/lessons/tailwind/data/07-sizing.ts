import type { Lesson } from "../../types";

export const sizing: Lesson = {
  id: "sizing",
  title: "Width & Height",
  chapterId: "layout",
  content: `## Width and Height in Tailwind

**Width:**
- \`w-full\` — 100%
- \`w-screen\` — 100vw
- \`w-1/2\`, \`w-1/3\`, \`w-2/3\`, \`w-1/4\`, \`w-3/4\` — fractions
- \`w-4\`, \`w-8\`, \`w-16\` — fixed sizes (1rem, 2rem, 4rem)
- \`w-px\` — 1px
- \`w-auto\` — auto

**Height:**
- \`h-full\`, \`h-screen\`, \`h-4\`, \`h-16\`, \`h-auto\`
- \`min-h-screen\` — minimum viewport height
- \`min-h-0\` — min-height: 0

**Max/min sizing:**
- \`max-w-sm\`, \`max-w-md\`, \`max-w-lg\`, \`max-w-xl\`, \`max-w-2xl\`, \`max-w-full\`
- \`max-w-xs\` — 20rem (320px)
- \`max-w-prose\` — 65ch (readable text width)
- \`min-w-0\`, \`min-w-full\`

\`\`\`html
<div class="max-w-2xl mx-auto">
  <div class="w-full h-48">Full width, fixed height</div>
  <div class="w-1/2">Half width</div>
</div>
\`\`\`

### Your Task

Create elements using \`w-full\`, a fraction width, a fixed height, and \`max-w-\`.`,

  starterCode: `<!-- Use sizing utilities -->
<div class="p-4 space-y-4">
  <div>Full width</div>
  <div>Half width</div>
  <div>Square</div>
  <div>Max width</div>
</div>
`,

  solution: `<div class="p-4 space-y-4">
  <div class="w-full bg-blue-100 p-3">Full width</div>
  <div class="w-1/2 bg-green-100 p-3">Half width</div>
  <div class="h-24 w-24 bg-red-100 rounded flex items-center justify-center">Square</div>
  <div class="max-w-md mx-auto bg-purple-100 p-3">Max width md</div>
</div>
`,

  tests: [
    {
      name: "uses w-full",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/\\bw-full\\b/.test(html));`,
    },
    {
      name: "uses a fraction width",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/\\bw-\\d+\\/\\d/.test(html));`,
    },
    {
      name: "uses a height utility",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/\\bh-\\d+/.test(html));`,
    },
    {
      name: "uses max-w utility",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/\\bmax-w-/.test(html));`,
    },
  ],
};
