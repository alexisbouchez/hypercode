import type { Lesson } from "../../types";

export const typographyUtilities: Lesson = {
  id: "typography-utilities",
  title: "Typography Utilities",
  chapterId: "utilities",
  content: `## Tailwind Typography

**Text size** scale: \`text-xs\`, \`text-sm\`, \`text-base\`, \`text-lg\`, \`text-xl\`, \`text-2xl\`, \`text-3xl\`, \`text-4xl\`, \`text-5xl\`

**Font weight**: \`font-thin\` (100) through \`font-black\` (900):
- \`font-light\` (300)
- \`font-normal\` (400)
- \`font-medium\` (500)
- \`font-semibold\` (600)
- \`font-bold\` (700)
- \`font-extrabold\` (800)

**Text color**: \`text-gray-900\`, \`text-blue-600\`, \`text-red-500\`, etc.

**Text alignment**: \`text-left\`, \`text-center\`, \`text-right\`, \`text-justify\`

**Line height**: \`leading-none\`, \`leading-tight\`, \`leading-normal\`, \`leading-relaxed\`, \`leading-loose\`

**Letter spacing**: \`tracking-tight\`, \`tracking-normal\`, \`tracking-wide\`, \`tracking-wider\`

\`\`\`html
<h1 class="text-3xl font-bold text-gray-900">Title</h1>
<p class="text-base text-gray-600 leading-relaxed">Body text</p>
<p class="text-sm text-gray-400 tracking-wide">Caption</p>
\`\`\`

### Your Task

Create a heading with \`font-bold\`, a paragraph with \`font-semibold\`, and a smaller text with text-gray styling.`,

  starterCode: `<!-- Style text elements with Tailwind typography utilities -->
<div class="p-6">
  <h1>Main Title</h1>
  <h2>Subtitle</h2>
  <p>Regular paragraph text</p>
  <p>Small caption text</p>
</div>
`,

  solution: `<div class="p-6">
  <h1 class="text-3xl font-bold text-gray-900 mb-2">Main Title</h1>
  <h2 class="text-xl font-semibold text-blue-700 mb-2">Subtitle</h2>
  <p class="text-base text-gray-600 leading-relaxed">Regular paragraph text</p>
  <p class="text-sm text-gray-400">Small caption text</p>
</div>
`,

  tests: [
    {
      name: "uses a text size class",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/\\btext-(sm|base|lg|xl|2xl|3xl)/.test(html));`,
    },
    {
      name: "uses font-bold",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/\\bfont-bold\\b/.test(html));`,
    },
    {
      name: "uses a text-gray color",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/\\btext-gray/.test(html));`,
    },
    {
      name: "uses font-semibold",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/\\bfont-semibold\\b/.test(html));`,
    },
  ],
};
