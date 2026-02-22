import type { Lesson } from "../../types";

export const cards: Lesson = {
  id: "cards",
  title: "Cards",
  chapterId: "components",
  content: `## Building Card Components

Cards are one of the most common UI patterns. A good card combines:

- **Container**: \`bg-white rounded-xl shadow-lg overflow-hidden\`
- **Image/Banner**: colored div or actual image
- **Body**: \`p-6\` with heading, text, optional footer
- **Interactive button**: \`bg-blue-500 hover:bg-blue-600 text-white px-4 py-2 rounded\`

\`\`\`html
<div class="bg-white rounded-xl shadow-lg overflow-hidden max-w-sm">
  <div class="bg-gradient-to-r from-blue-500 to-purple-600 h-32"></div>
  <div class="p-6">
    <h2 class="text-xl font-bold mb-2">Card Title</h2>
    <p class="text-gray-600 text-sm">Description text here.</p>
    <button class="mt-4 bg-blue-500 text-white px-4 py-2 rounded">
      Action
    </button>
  </div>
</div>
\`\`\`

**Gradient backgrounds:**
- \`from-blue-500 to-purple-600\` — linear gradient
- Add \`bg-gradient-to-r\` (or \`to-br\`, \`to-b\`, etc.) to specify direction

### Your Task

Build a complete card component with: \`rounded-xl\`, \`shadow\`, a gradient banner, and an \`overflow-hidden\` container.`,

  starterCode: `<!-- Build a card component -->
<div class="max-w-sm mx-auto mt-8">

</div>
`,

  solution: `<div class="max-w-sm mx-auto mt-8">
  <div class="bg-white rounded-xl shadow-lg overflow-hidden">
    <div class="bg-gradient-to-r from-blue-500 to-purple-600 h-32"></div>
    <div class="p-6">
      <h2 class="text-xl font-bold text-gray-900 mb-2">Card Title</h2>
      <p class="text-gray-600 text-sm">Card description with some detail text.</p>
      <button class="mt-4 bg-blue-500 text-white px-4 py-2 rounded-lg font-medium">
        Learn More
      </button>
    </div>
  </div>
</div>
`,

  tests: [
    {
      name: "uses rounded-xl",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/\\brounded-xl\\b/.test(html));`,
    },
    {
      name: "uses shadow",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/\\bshadow/.test(html));`,
    },
    {
      name: "uses gradient from-",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/\\bfrom-/.test(html));`,
    },
    {
      name: "uses overflow-hidden",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/\\boverflow-hidden\\b/.test(html));`,
    },
  ],
};
