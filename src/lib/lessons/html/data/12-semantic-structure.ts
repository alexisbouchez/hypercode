import type { Lesson } from "../../types";

export const semanticStructure: Lesson = {
  id: "semantic-structure",
  title: "Semantic Structure",
  chapterId: "semantic",
  content: `## Semantic HTML

Semantic HTML uses meaningful tags that describe their purpose — not just their appearance. This helps search engines, screen readers, and developers understand the page structure.

Key semantic elements:

\`\`\`html
<header>   <!-- Site header, logo, navigation -->
<nav>      <!-- Navigation links -->
<main>     <!-- Main page content (use once per page) -->
<section>  <!-- Thematic group of content -->
<article>  <!-- Self-contained content (blog post, news article) -->
<aside>    <!-- Sidebar, related content -->
<footer>   <!-- Site footer -->
\`\`\`

A typical page structure:

\`\`\`html
<header>
  <h1>My Site</h1>
</header>
<main>
  <section>
    <h2>About</h2>
    <p>Welcome!</p>
  </section>
</main>
<footer>
  <p>© 2024</p>
</footer>
\`\`\`

### Your Task

Create a page with \`<header>\`, \`<main>\`, \`<section>\`, and \`<footer>\` elements.`,

  starterCode: `<!-- Create a semantic page structure -->
`,

  solution: `<header>
  <h1>My Website</h1>
</header>
<main>
  <section>
    <h2>About</h2>
    <p>Welcome to my site.</p>
  </section>
</main>
<footer>
  <p>© 2024 My Website</p>
</footer>
`,

  tests: [
    {
      name: "has a header element",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/<header>/i.test(html));`,
    },
    {
      name: "has a main element",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/<main>/i.test(html));`,
    },
    {
      name: "has a footer element",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/<footer>/i.test(html));`,
    },
    {
      name: "has a section element",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/<section>/i.test(html));`,
    },
  ],
};
