import type { Lesson } from "../../types";

export const articleSection: Lesson = {
  id: "article-section",
  title: "Article & Section",
  chapterId: "semantic",
  content: `## Article and Section

**\`<article>\`** — a self-contained piece of content that makes sense on its own:
- Blog posts
- News articles
- Forum posts
- Product cards

**\`<section>\`** — a thematic grouping within a page or article:
- Chapters of a document
- Introduction, body, conclusion
- Feature sections on a landing page

\`\`\`html
<article>
  <h2>My Blog Post</h2>
  <section>
    <h3>Introduction</h3>
    <p>This post covers...</p>
  </section>
  <section>
    <h3>Main Content</h3>
    <p>The key ideas are...</p>
  </section>
</article>
\`\`\`

The difference: \`<article>\` is for content that could stand alone (be syndicated). \`<section>\` is for organizing content within a page.

### Your Task

Create an \`<article>\` with a title and at least 2 \`<section>\` elements, each with a heading and paragraph.`,

  starterCode: `<!-- Create an article with sections -->
`,

  solution: `<main>
  <article>
    <h2>My First Blog Post</h2>
    <p>Published on January 1, 2024</p>
    <section>
      <h3>Introduction</h3>
      <p>This is the introduction section.</p>
    </section>
    <section>
      <h3>Conclusion</h3>
      <p>Thanks for reading!</p>
    </section>
  </article>
</main>
`,

  tests: [
    {
      name: "has an article element",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/<article>/i.test(html));`,
    },
    {
      name: "has a section element",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/<section>/i.test(html));`,
    },
    {
      name: "has an h2 heading",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/<h2>/i.test(html));`,
    },
    {
      name: "has multiple sections",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log((html.match(/<section>/gi)||[]).length >= 2);`,
    },
  ],
};
