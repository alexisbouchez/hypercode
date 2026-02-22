import type { Lesson } from "../../types";

export const links: Lesson = {
  id: "links",
  title: "Links",
  chapterId: "content",
  content: `## Anchor Tags

Links are created with the \`<a>\` (anchor) tag and a \`href\` attribute:

\`\`\`html
<a href="https://example.com">Visit Example</a>
\`\`\`

**Open in a new tab** with \`target="_blank"\`:
\`\`\`html
<a href="https://example.com" target="_blank">Open in new tab</a>
\`\`\`

**Email links** use \`mailto:\`:
\`\`\`html
<a href="mailto:hello@example.com">Send an email</a>
\`\`\`

**Link to a page section** using an id:
\`\`\`html
<a href="#section1">Jump to section</a>
<h2 id="section1">Section 1</h2>
\`\`\`

### Your Task

Create three links:
1. A regular link to any URL
2. A link that opens in a new tab (\`target="_blank"\`)
3. A \`mailto:\` email link`,

  starterCode: `<!-- Create links: regular, new tab, and mailto -->
`,

  solution: `<a href="https://example.com">Visit Example</a>
<a href="https://openai.com" target="_blank">OpenAI (new tab)</a>
<a href="mailto:hello@example.com">Send Email</a>
`,

  tests: [
    {
      name: "has an anchor tag",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/<a\\s/i.test(html));`,
    },
    {
      name: "has an href attribute",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/href=/i.test(html));`,
    },
    {
      name: "has target blank",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/target="_blank"/i.test(html));`,
    },
    {
      name: "has a mailto link",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/mailto:/i.test(html));`,
    },
  ],
};
