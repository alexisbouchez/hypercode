import type { Lesson } from "../../types";

export const navbar: Lesson = {
  id: "navbar",
  title: "Navbar",
  chapterId: "components",
  content: `## Building a Navigation Bar

A typical navbar has:
- Dark/colored background
- Brand name on the left
- Nav links on the right (or centered)
- Hover effects on links
- A call-to-action button

\`\`\`html
<nav class="bg-gray-900 text-white px-6 py-4">
  <div class="max-w-6xl mx-auto flex items-center justify-between">
    <div class="text-xl font-bold">Brand</div>
    <ul class="flex gap-6">
      <li><a href="#" class="hover:text-blue-400 transition-colors">Home</a></li>
      <li><a href="#" class="hover:text-blue-400 transition-colors">About</a></li>
    </ul>
  </div>
</nav>
\`\`\`

Key patterns:
- \`flex items-center justify-between\` — space brand and links
- \`max-w-6xl mx-auto\` — constrain and center content
- \`hover:text-blue-400 transition-colors\` — smooth hover effect

### Your Task

Build a full navbar with a brand name, at least 3 navigation links with hover effects, and a sign-in button.`,

  starterCode: `<!-- Build a navigation bar -->
<nav class="bg-gray-900 text-white px-6 py-4">

</nav>
`,

  solution: `<nav class="bg-gray-900 text-white px-6 py-4">
  <div class="max-w-6xl mx-auto flex items-center justify-between">
    <div class="text-xl font-bold text-blue-400">MyBrand</div>
    <ul class="flex items-center gap-6">
      <li><a href="#" class="hover:text-blue-400 transition-colors">Home</a></li>
      <li><a href="#" class="hover:text-blue-400 transition-colors">About</a></li>
      <li><a href="#" class="hover:text-blue-400 transition-colors">Blog</a></li>
      <li>
        <a href="#" class="bg-blue-600 hover:bg-blue-700 px-4 py-2 rounded-lg transition-colors">
          Sign In
        </a>
      </li>
    </ul>
  </div>
</nav>
`,

  tests: [
    {
      name: "has a nav element",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/<nav/.test(html));`,
    },
    {
      name: "uses justify-between",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/\\bjustify-between\\b/.test(html));`,
    },
    {
      name: "uses hover:text- class",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/\\bhover:text-/.test(html));`,
    },
    {
      name: "uses flex with items-center",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/\\bflex\\b/.test(html) && /\\bitems-center\\b/.test(html));`,
    },
  ],
};
