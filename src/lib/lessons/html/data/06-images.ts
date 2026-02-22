import type { Lesson } from "../../types";

export const images: Lesson = {
  id: "images",
  title: "Images",
  chapterId: "content",
  content: `## The img Element

Images use the \`<img>\` tag — a self-closing tag (no closing \`</img>\`):

\`\`\`html
<img src="photo.jpg" alt="A beautiful photo">
\`\`\`

Key attributes:
- \`src\` — the image URL or file path
- \`alt\` — alternative text (shown if image fails to load, required for accessibility)
- \`width\` and \`height\` — dimensions in pixels

\`\`\`html
<img src="https://picsum.photos/300/200" alt="Random photo" width="300" height="200">
\`\`\`

### Why alt text matters

Screen readers read the \`alt\` text aloud for visually impaired users. Search engines use it to understand images. Always include a meaningful \`alt\` attribute.

### Your Task

Add an image with:
- A \`src\` pointing to any image URL
- An \`alt\` description
- A \`width\` attribute`,

  starterCode: `<!-- Add an image with src, alt, and width attributes -->
`,

  solution: `<img src="https://picsum.photos/200/150" alt="A sample image" width="200" height="150">
<p>Image caption below the photo</p>
`,

  tests: [
    {
      name: "has an img tag",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/<img/i.test(html));`,
    },
    {
      name: "has a src attribute",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/src=/i.test(html));`,
    },
    {
      name: "has an alt attribute",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/alt=/i.test(html));`,
    },
    {
      name: "has a width attribute",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/width=/i.test(html));`,
    },
  ],
};
