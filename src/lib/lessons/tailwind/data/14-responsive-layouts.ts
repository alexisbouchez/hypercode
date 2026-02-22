import type { Lesson } from "../../types";

export const responsiveLayouts: Lesson = {
  id: "responsive-layouts",
  title: "Responsive Layouts",
  chapterId: "responsive",
  content: `## Responsive Grid Layouts

The most common responsive pattern: 1 column on mobile -> 2 on tablet -> 3 on desktop.

\`\`\`html
<div class="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
  <div>Card 1</div>
  <div>Card 2</div>
  <div>Card 3</div>
</div>
\`\`\`

**Responsive flex:**
\`\`\`html
<!-- Stack on mobile, row on tablet+ -->
<div class="flex flex-col md:flex-row gap-4">
  <div class="w-full md:w-1/3">Sidebar</div>
  <div class="w-full md:w-2/3">Main</div>
</div>
\`\`\`

**Hide/show at breakpoints:**
\`\`\`html
<div class="hidden md:block">Hidden on mobile</div>
<div class="block md:hidden">Visible on mobile only</div>
\`\`\`

**Responsive max-width containers:**
\`\`\`html
<div class="max-w-sm md:max-w-2xl lg:max-w-4xl mx-auto">
  Content with expanding container
</div>
\`\`\`

### Your Task

Create a card grid that goes from 1 column on mobile to 2 on tablet (\`md:\`) to 3 on desktop (\`lg:\`).`,

  starterCode: `<!-- Create a responsive card grid -->
<div class="max-w-6xl mx-auto p-4">
  <div>
    <div>Card One</div>
    <div>Card Two</div>
    <div>Card Three</div>
  </div>
</div>
`,

  solution: `<div class="max-w-6xl mx-auto p-4 md:p-8">
  <div class="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
    <div class="bg-blue-100 rounded-xl p-6">
      <h3 class="font-bold text-lg mb-2">Card One</h3>
      <p class="text-gray-600">Full width on mobile, half on tablet, third on desktop.</p>
    </div>
    <div class="bg-green-100 rounded-xl p-6">
      <h3 class="font-bold text-lg mb-2">Card Two</h3>
      <p class="text-gray-600">Responsive grid layout.</p>
    </div>
    <div class="bg-purple-100 rounded-xl p-6">
      <h3 class="font-bold text-lg mb-2">Card Three</h3>
      <p class="text-gray-600">Adapts to screen size automatically.</p>
    </div>
  </div>
</div>
`,

  tests: [
    {
      name: "uses grid-cols-1 for mobile",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/\\bgrid-cols-1\\b/.test(html));`,
    },
    {
      name: "uses md:grid-cols responsive",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/\\bmd:grid-cols-\\d/.test(html));`,
    },
    {
      name: "uses lg:grid-cols responsive",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/\\blg:grid-cols-\\d/.test(html));`,
    },
    {
      name: "uses multiple md: prefixes",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log((html.match(/\\bmd:/g)||[]).length >= 2);`,
    },
  ],
};
