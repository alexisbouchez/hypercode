import type { Lesson } from "../../types";

export const responsiveBasics: Lesson = {
  id: "responsive-basics",
  title: "Responsive Basics",
  chapterId: "responsive",
  content: `## Responsive Design in Tailwind

Tailwind is **mobile-first**: unprefixed classes apply to all screen sizes; prefixed classes apply from a breakpoint upward.

**Breakpoints:**
- \`sm:\` — 640px+
- \`md:\` — 768px+
- \`lg:\` — 1024px+
- \`xl:\` — 1280px+
- \`2xl:\` — 1536px+

\`\`\`html
<!-- Mobile: text-base, Tablet+: text-xl, Desktop+: text-3xl -->
<h1 class="text-base md:text-xl lg:text-3xl">Responsive Title</h1>
\`\`\`

\`\`\`html
<!-- Stack on mobile, side-by-side on tablet -->
<div class="flex flex-col md:flex-row gap-4">
  <div class="w-full md:w-1/2">Left</div>
  <div class="w-full md:w-1/2">Right</div>
</div>
\`\`\`

\`\`\`html
<!-- Padding increases at larger screens -->
<div class="p-4 md:p-8 lg:p-12">Content</div>
\`\`\`

### Your Task

Create a heading and paragraph that use \`md:\` and \`lg:\` prefixes to change text size and padding at different breakpoints.`,

  starterCode: `<!-- Add responsive breakpoint classes -->
<div>
  <h1>Responsive Heading</h1>
  <p>This text changes size at different breakpoints.</p>
  <button>Responsive Button</button>
</div>
`,

  solution: `<div class="p-4 md:p-8 lg:p-12">
  <h1 class="text-2xl md:text-4xl lg:text-5xl font-bold text-gray-900">
    Responsive Heading
  </h1>
  <p class="text-sm md:text-base lg:text-lg text-gray-600 mt-4">
    This text changes size at different breakpoints.
  </p>
  <button class="mt-4 px-4 md:px-8 py-2 md:py-3 bg-blue-600 text-white text-sm md:text-base rounded-lg">
    Responsive Button
  </button>
</div>
`,

  tests: [
    {
      name: "uses md: prefix",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/\\bmd:/.test(html));`,
    },
    {
      name: "uses lg: prefix",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/\\blg:/.test(html));`,
    },
    {
      name: "uses responsive text sizing",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/md:text-/.test(html));`,
    },
    {
      name: "uses responsive padding",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/md:p/.test(html));`,
    },
  ],
};
