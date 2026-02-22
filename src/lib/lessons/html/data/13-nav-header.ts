import type { Lesson } from "../../types";

export const navHeader: Lesson = {
  id: "nav-header",
  title: "Nav & Header",
  chapterId: "semantic",
  content: `## Navigation and Header

A navigation bar typically combines \`<header>\`, \`<nav>\`, and a list of links:

\`\`\`html
<header>
  <h1>Brand Name</h1>
  <nav>
    <ul>
      <li><a href="/">Home</a></li>
      <li><a href="/about">About</a></li>
      <li><a href="/contact">Contact</a></li>
    </ul>
  </nav>
</header>
\`\`\`

Why use a list for navigation?
- Semantically correct — nav items are a list
- Screen readers announce "navigation list with N items"
- Easy to style with CSS (remove bullets, display inline)

The \`<nav>\` element explicitly marks navigation landmarks, so users of assistive technology can jump directly to the nav.

### Your Task

Create a \`<header>\` containing a brand name heading and a \`<nav>\` with at least 3 links in a list.`,

  starterCode: `<!-- Create a header with navigation -->
`,

  solution: `<header>
  <h1>My Site</h1>
  <nav>
    <ul>
      <li><a href="#">Home</a></li>
      <li><a href="#">About</a></li>
      <li><a href="#">Contact</a></li>
    </ul>
  </nav>
</header>
`,

  tests: [
    {
      name: "has a nav element",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/<nav>/i.test(html));`,
    },
    {
      name: "has anchor links",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/<a\\s/i.test(html));`,
    },
    {
      name: "has an unordered list",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/<ul>/i.test(html));`,
    },
    {
      name: "has a header element",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/<header>/i.test(html));`,
    },
  ],
};
