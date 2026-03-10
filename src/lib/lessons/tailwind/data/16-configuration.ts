import type { Lesson } from "../../types";

export const configuration: Lesson = {
  id: "configuration",
  title: "Configuration & Customization",
  chapterId: "configuration",
  content: `## Tailwind Configuration

Tailwind is configured through \`tailwind.config.js\`. This file lets you customize the default theme, add plugins, and control which utilities are generated.

### Extending the Theme

Use \`theme.extend\` to add custom values without overriding defaults:
\`\`\`js
module.exports = {
  theme: {
    extend: {
      colors: {
        brand: {
          light: '#3fbaeb',
          DEFAULT: '#0fa9e6',
          dark: '#0c87b8',
        },
      },
      spacing: {
        '72': '18rem',
        '84': '21rem',
        '96': '24rem',
      },
      fontFamily: {
        display: ['Oswald', 'sans-serif'],
      },
    },
  },
}
\`\`\`

Custom values become first-class utilities: \`bg-brand\`, \`text-brand-light\`, \`p-72\`, \`font-display\`.

### Arbitrary Values (JIT Mode)

Tailwind's JIT (Just-In-Time) engine generates utilities on demand. Since Tailwind v3, JIT is the default — every class is generated only when used, keeping CSS bundles tiny.

JIT also enables **arbitrary values** using square bracket notation:
\`\`\`html
<div class="bg-[#1da1f2] text-[14px] p-[5px] grid-cols-[1fr_500px_2fr]">
  Custom one-off values
</div>
\`\`\`

Arbitrary values work with any utility: \`w-[calc(100%-2rem)]\`, \`top-[117px]\`, \`bg-[#custom]\`.

### Plugins

Tailwind's plugin system lets you register new utilities, components, or base styles:
\`\`\`js
const plugin = require('tailwindcss/plugin')

module.exports = {
  plugins: [
    require('@tailwindcss/forms'),       // form reset
    require('@tailwindcss/typography'),   // prose class
    require('@tailwindcss/aspect-ratio'), // aspect-w/h
    plugin(function({ addUtilities }) {
      addUtilities({
        '.text-shadow': {
          textShadow: '0 2px 4px rgba(0,0,0,0.10)',
        },
      })
    }),
  ],
}
\`\`\`

The official \`@tailwindcss/typography\` plugin provides the \`prose\` class for beautiful typographic defaults on rendered HTML content.

### Your Task

Build a product card that demonstrates configuration concepts:
1. Use **arbitrary values** (square bracket notation) for at least one color and one size
2. Use the \`prose\` class (from the typography plugin) on a text block
3. Use at least one custom spacing or sizing value via arbitrary notation (e.g., \`w-[300px]\`)`,

  starterCode: `<!-- Build a product card using arbitrary values and prose -->
<div class="max-w-md mx-auto p-8">
  <div class="rounded-xl shadow-lg overflow-hidden">

  </div>
</div>
`,

  solution: `<div class="max-w-md mx-auto p-8">
  <div class="rounded-xl shadow-lg overflow-hidden">
    <div class="bg-[#0fa9e6] p-[20px]">
      <h2 class="text-white text-xl font-bold">Pro Plan</h2>
      <p class="text-blue-100 text-[13px]">Everything you need</p>
    </div>
    <div class="p-6 bg-white">
      <div class="prose mb-4">
        <p>Get unlimited access to all features including priority support, advanced analytics, and custom integrations.</p>
      </div>
      <div class="flex items-center gap-2 mb-4">
        <span class="text-3xl font-bold text-gray-900">$49</span>
        <span class="text-gray-500">/month</span>
      </div>
      <button class="w-[300px] bg-[#0fa9e6] hover:bg-[#0c87b8] text-white py-3 rounded-lg font-semibold transition-colors">
        Get Started
      </button>
    </div>
  </div>
</div>
`,

  tests: [
    {
      name: "uses arbitrary color value with bracket notation",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/\\b(?:bg|text|border)-\\[#[0-9a-fA-F]+\\]/.test(html));`,
    },
    {
      name: "uses prose class for typography plugin",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/\\bprose\\b/.test(html));`,
    },
    {
      name: "uses arbitrary size value with bracket notation",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/\\b(?:w|h|p|m|gap|text|top|left|right|bottom)-\\[\\d+px\\]/.test(html));`,
    },
  ],
};
