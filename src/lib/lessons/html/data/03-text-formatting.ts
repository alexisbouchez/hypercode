import type { Lesson } from "../../types";

export const textFormatting: Lesson = {
  id: "text-formatting",
  title: "Text Formatting",
  chapterId: "structure",
  content: `## Text Formatting Tags

HTML provides several inline tags for formatting text within paragraphs:

| Tag | Purpose | Renders as |
|-----|---------|-----------|
| \`<strong>\` | Important text | **bold** |
| \`<em>\` | Emphasized text | *italic* |
| \`<code>\` | Inline code | \`monospace\` |
| \`<mark>\` | Highlighted text | highlighted |
| \`<s>\` | Strikethrough | ~~crossed out~~ |

Example:

\`\`\`html
<p><strong>Important:</strong> This is <em>really</em> critical.</p>
<p>Run <code>npm install</code> to install dependencies.</p>
<p><mark>Note this</mark> and <s>ignore that</s>.</p>
\`\`\`

### Your Task

Create paragraphs that use \`<strong>\`, \`<em>\`, \`<code>\`, and \`<mark>\` tags.`,

  starterCode: `<!-- Use text formatting tags -->
`,

  solution: `<p><strong>Bold text</strong> and <em>italic text</em></p>
<p>Inline <code>code example</code> here</p>
<p><mark>Highlighted</mark> and <s>strikethrough</s></p>
`,

  tests: [
    {
      name: "uses the strong tag",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/<strong>/i.test(html));`,
    },
    {
      name: "uses the em tag",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/<em>/i.test(html));`,
    },
    {
      name: "uses the code tag",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/<code>/i.test(html));`,
    },
    {
      name: "uses the mark tag",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/<mark>/i.test(html));`,
    },
  ],
};
