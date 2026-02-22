import type { Lesson } from "../../types";

export const hoverDark: Lesson = {
  id: "hover-dark",
  title: "Hover & Dark Mode",
  chapterId: "responsive",
  content: `## Hover States and Dark Mode

**Hover** and other interactive states:
\`\`\`html
<button class="bg-blue-600 hover:bg-blue-700 active:scale-95 transition-all">
  Click Me
</button>
\`\`\`

**Available state variants:** \`hover:\`, \`focus:\`, \`active:\`, \`disabled:\`, \`group-hover:\`

**Dark mode** with the \`dark:\` variant:
\`\`\`html
<div class="bg-white dark:bg-gray-900 text-gray-900 dark:text-white">
  Theme-aware content
</div>
\`\`\`

Enable dark mode in \`tailwind.config.js\`:
\`\`\`js
module.exports = {
  darkMode: 'class',  // or 'media' for OS preference
}
\`\`\`

With \`'class'\` mode, add \`dark\` class to \`<html>\`:
\`\`\`html
<html class="dark">...</html>
\`\`\`

With \`'media'\` mode, dark mode follows OS preference automatically.

**Combining variants:**
\`\`\`html
<button class="dark:bg-blue-500 dark:hover:bg-blue-600">
  Dark mode button
</button>
\`\`\`

### Your Task

Create a theme-aware card using \`dark:\` variants for background, text, and hover states.`,

  starterCode: `<!-- Create a dark mode aware card -->
<div class="dark:bg-gray-900 min-h-screen p-8">
  <div class="max-w-sm mx-auto">

  </div>
</div>
`,

  solution: `<div class="dark:bg-gray-900 min-h-screen p-8">
  <div class="max-w-sm mx-auto">
    <div class="bg-white dark:bg-gray-800 rounded-xl shadow-lg p-6 transition-all duration-300">
      <h2 class="text-xl font-bold text-gray-900 dark:text-white mb-4">Theme-Aware Card</h2>
      <p class="text-gray-600 dark:text-gray-300 mb-4">This card adapts to dark mode.</p>
      <button class="bg-blue-600 hover:bg-blue-700 dark:bg-blue-500 dark:hover:bg-blue-600 text-white px-6 py-2 rounded-lg transition-colors focus:ring-2 focus:ring-blue-500 focus:ring-offset-2">
        Toggle Theme
      </button>
    </div>
  </div>
</div>
`,

  tests: [
    {
      name: "uses dark: prefix",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/\\bdark:/.test(html));`,
    },
    {
      name: "uses dark:hover: combined variant",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/\\bdark:hover:/.test(html));`,
    },
    {
      name: "uses dark:bg- class",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/\\bdark:bg-/.test(html));`,
    },
    {
      name: "uses dark:text- class",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/\\bdark:text-/.test(html));`,
    },
  ],
};
