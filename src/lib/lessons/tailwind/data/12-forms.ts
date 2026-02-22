import type { Lesson } from "../../types";

export const formsTailwind: Lesson = {
  id: "forms-tailwind",
  title: "Forms",
  chapterId: "components",
  content: `## Styling Forms with Tailwind

Form inputs need careful styling for usability:

\`\`\`html
<input
  type="email"
  class="w-full border border-gray-300 rounded-lg px-4 py-2
         focus:outline-none focus:ring-2 focus:ring-blue-500"
  placeholder="Email"
>
\`\`\`

**Key input classes:**
- \`w-full\` — full width
- \`border border-gray-300\` — subtle border
- \`rounded-lg\` — rounded corners
- \`px-4 py-2\` — comfortable padding
- \`focus:outline-none\` — remove default browser focus ring
- \`focus:ring-2 focus:ring-blue-500\` — custom focus indicator

**Labels:**
\`\`\`html
<label class="block text-sm font-medium text-gray-700 mb-1">
  Email address
</label>
\`\`\`

**Grouping fields:**
\`\`\`html
<div class="space-y-4">
  <div>
    <label ...>Name</label>
    <input ...>
  </div>
</div>
\`\`\`

### Your Task

Create a sign-in form with email + password inputs using focus ring styles, and a submit button.`,

  starterCode: `<!-- Create a styled form with focus states -->
<div class="max-w-md mx-auto p-8">
  <form class="space-y-4">

  </form>
</div>
`,

  solution: `<div class="max-w-md mx-auto p-8">
  <form class="space-y-4">
    <div>
      <label class="block text-sm font-medium text-gray-700 mb-1">Email</label>
      <input type="email" class="w-full border border-gray-300 rounded-lg px-4 py-2 focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-transparent" placeholder="you@example.com">
    </div>
    <div>
      <label class="block text-sm font-medium text-gray-700 mb-1">Password</label>
      <input type="password" class="w-full border border-gray-300 rounded-lg px-4 py-2 focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-transparent">
    </div>
    <button type="submit" class="w-full bg-blue-600 hover:bg-blue-700 text-white font-semibold py-2 rounded-lg transition-colors">
      Sign In
    </button>
  </form>
</div>
`,

  tests: [
    {
      name: "uses focus:ring",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/\\bfocus:ring/.test(html));`,
    },
    {
      name: "uses w-full",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/\\bw-full\\b/.test(html));`,
    },
    {
      name: "uses border-gray",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/\\bborder-gray/.test(html));`,
    },
    {
      name: "uses space-y",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/\\bspace-y-\\d/.test(html));`,
    },
  ],
};
