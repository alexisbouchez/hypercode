import type { Lesson } from "../../types";

export const firstComponent: Lesson = {
  id: "first-component",
  title: "Your First Component",
  chapterId: "utilities",
  content: `## Welcome to Tailwind CSS

Tailwind CSS is a utility-first CSS framework. Instead of writing CSS rules, you apply pre-built utility classes directly in HTML:

\`\`\`html
<!-- Traditional CSS approach -->
<div class="card">Hello</div>

<!-- Tailwind approach -->
<div class="bg-blue-500 text-white p-4 rounded-lg font-bold">Hello</div>
\`\`\`

**Common utilities:**
- \`p-4\` — padding: 1rem (16px)
- \`m-2\` — margin: 0.5rem
- \`text-xl\` — font-size: 1.25rem
- \`font-bold\` — font-weight: 700
- \`bg-blue-500\` — background: a blue color
- \`text-white\` — color: white
- \`rounded-lg\` — border-radius: 0.5rem
- \`text-center\` — text-align: center

The number scale (100–900 for colors, 1–16 for spacing) gives you precise control.

### Your Task

Create a styled \`<div>\` using at least: \`p-4\`, \`bg-blue-500\`, \`text-white\`, and \`rounded\`.`,

  starterCode: `<!-- Style this div with Tailwind utility classes -->
<div>
  Hello, Tailwind!
</div>
`,

  solution: `<div class="text-xl font-bold text-center p-4 bg-blue-500 text-white rounded-lg shadow-md">
  Hello, Tailwind!
</div>
`,

  tests: [
    {
      name: "uses p-4 padding",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/\\bp-4\\b/.test(html));`,
    },
    {
      name: "uses a bg-blue class",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/\\bbg-blue/.test(html));`,
    },
    {
      name: "uses text-white",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/\\btext-white\\b/.test(html));`,
    },
    {
      name: "uses rounded corners",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/\\brounded/.test(html));`,
    },
  ],
};
