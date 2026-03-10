import type { Lesson } from "../../types";

export const firstStyles: Lesson = {
  id: "first-styles",
  title: "Your First Styles",
  chapterId: "selectors",
  content: `## Applying CSS to HTML

CSS (Cascading Style Sheets) adds visual styles to HTML. You can embed CSS directly in HTML using a \`<style>\` tag in the \`<head>\`:

\`\`\`html
<style>
  h1 {
    color: blue;
    font-size: 2em;
  }
  p {
    color: gray;
  }
</style>
<h1>Hello</h1>
<p>World</p>
\`\`\`

A CSS rule has:
- **Selector** — what to style (\`h1\`, \`p\`, \`.class\`, \`#id\`)
- **Declaration block** — \`{ property: value; }\`

**Inheritance:** Some CSS properties (like \`color\`, \`font-family\`, \`line-height\`) are **inherited** by child elements automatically. Others (like \`border\`, \`padding\`, \`margin\`) are not. This means if you set \`color: navy\` on \`body\`, all text inside inherits that color unless overridden.

**CSS Variables (Custom Properties):** You can define reusable values with \`--\` prefix:
\`\`\`css
:root { --primary: #2563eb; --gap: 1rem; }
h1   { color: var(--primary); margin-bottom: var(--gap); }
\`\`\`

Common properties:
- \`color\` — text color
- \`font-size\` — text size
- \`background-color\` — background fill
- \`margin\` — outer spacing
- \`padding\` — inner spacing

### Your Task

Style an \`<h1>\` with a color and font-size, and a \`<p>\` with a different color.`,

  starterCode: `<style>
/* Style h1 and p elements */
</style>
<h1>My Styled Page</h1>
<p>This paragraph needs styling.</p>
`,

  solution: `<style>
h1 { color: #2563eb; font-size: 2em; margin-bottom: 0.5em; }
p { color: #374151; line-height: 1.6; }
</style>
<h1>My Styled Page</h1>
<p>This paragraph has been styled with CSS.</p>
`,

  tests: [
    {
      name: "has a style tag",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/<style>/i.test(html));`,
    },
    {
      name: "has a color property",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/color\\s*:/i.test(html));`,
    },
    {
      name: "has an h1 selector",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/h1\\s*\\{/i.test(html));`,
    },
    {
      name: "has a p selector",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/p\\s*\\{/i.test(html));`,
    },
  ],
};
