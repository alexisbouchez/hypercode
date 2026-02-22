import type { Lesson } from "../../types";

export const headingsParagraphs: Lesson = {
  id: "headings-paragraphs",
  title: "Headings & Paragraphs",
  chapterId: "structure",
  content: `## Headings and Paragraphs

HTML has six heading levels, from \`<h1>\` (most important) to \`<h6>\` (least important):

\`\`\`html
<h1>Page Title</h1>
<h2>Section Title</h2>
<h3>Subsection</h3>
<h4>Minor heading</h4>
<h5>Small heading</h5>
<h6>Smallest heading</h6>
\`\`\`

Paragraphs use the \`<p>\` tag:

\`\`\`html
<p>This is a paragraph. It can span multiple sentences.</p>
\`\`\`

Browsers automatically add space before and after headings and paragraphs.

### Your Task

Create a page with:
- An \`<h1>\` main title
- An \`<h2>\` section title
- An \`<h3>\` subsection title
- A \`<p>\` paragraph with some text`,

  starterCode: `<!-- Create headings and a paragraph -->
`,

  solution: `<h1>Main Title</h1>
<h2>Section Title</h2>
<h3>Subsection</h3>
<p>This is a paragraph with some text about the section above.</p>
`,

  tests: [
    {
      name: "has an h1 element",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/<h1>/i.test(html));`,
    },
    {
      name: "has an h2 element",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/<h2>/i.test(html));`,
    },
    {
      name: "has an h3 element",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/<h3>/i.test(html));`,
    },
    {
      name: "has a paragraph element",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/<p>/i.test(html));`,
    },
  ],
};
