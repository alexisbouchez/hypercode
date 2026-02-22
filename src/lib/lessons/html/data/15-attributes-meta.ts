import type { Lesson } from "../../types";

export const attributesMeta: Lesson = {
  id: "attributes-meta",
  title: "HTML Attributes & Meta",
  chapterId: "semantic",
  content: `## HTML Attributes and Meta Tags

**Global attributes** work on any element:
- \`id\` — unique identifier (only one per page)
- \`class\` — CSS class names (space-separated)
- \`data-*\` — custom data attributes
- \`style\` — inline CSS

\`\`\`html
<div id="main" class="container dark" data-theme="night">
  Content
</div>
\`\`\`

**Meta tags** go inside \`<head>\` and provide metadata about the page:

\`\`\`html
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <meta name="description" content="Page description for search engines">
  <title>Page Title</title>
</head>
\`\`\`

- \`charset="UTF-8"\` — character encoding (always include this)
- \`viewport\` — makes the page responsive on mobile

### Your Task

Write a complete HTML document with \`<meta charset>\`, \`<meta name="viewport">\`, and a \`<div>\` using \`id\`, \`class\`, and \`data-*\` attributes.`,

  starterCode: `<!DOCTYPE html>
<html lang="en">
<head>
  <!-- Add meta tags here -->
  <title>My Page</title>
</head>
<body>
  <!-- Add a div with id, class, and data attributes -->
</body>
</html>
`,

  solution: `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>My Page</title>
</head>
<body>
  <div id="main" class="container" data-theme="dark">
    <p>Content here</p>
  </div>
</body>
</html>
`,

  tests: [
    {
      name: "has meta charset",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/meta.*charset/i.test(html));`,
    },
    {
      name: "has meta viewport",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/meta.*viewport/i.test(html));`,
    },
    {
      name: "has an id attribute",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/\\bid=/i.test(html));`,
    },
    {
      name: "has a data attribute",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/data-/i.test(html));`,
    },
  ],
};
