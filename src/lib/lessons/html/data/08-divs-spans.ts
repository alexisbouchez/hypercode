import type { Lesson } from "../../types";

export const divsSpans: Lesson = {
  id: "divs-spans",
  title: "Divs & Spans",
  chapterId: "content",
  content: `## Divs and Spans

\`<div>\` and \`<span>\` are generic container elements with no built-in meaning.

**\`<div>\`** is a **block-level** element — it takes up the full width:
\`\`\`html
<div class="card">
  <p>Content inside a div</p>
</div>
\`\`\`

**\`<span>\`** is an **inline** element — it sits within text:
\`\`\`html
<p>The sky is <span style="color: blue;">blue</span> today.</p>
\`\`\`

Use \`class\` and \`id\` attributes to target them with CSS:

\`\`\`html
<div id="header" class="container nav-bar">
  <span class="logo">MyBrand</span>
</div>
\`\`\`

### Your Task

Create a \`<div>\` with a \`class\` attribute containing another \`<div>\`, and use a \`<span>\` to style part of a paragraph.`,

  starterCode: `<!-- Use div and span elements -->
`,

  solution: `<div class="container">
  <div class="card">
    <p>This is a <span style="color: blue;">blue word</span> in a paragraph.</p>
  </div>
</div>
`,

  tests: [
    {
      name: "has a div element",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/<div/i.test(html));`,
    },
    {
      name: "has a span element",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/<span/i.test(html));`,
    },
    {
      name: "has nested divs",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log((html.match(/<div/gi)||[]).length >= 2);`,
    },
    {
      name: "has a class attribute",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/class=/i.test(html));`,
    },
  ],
};
