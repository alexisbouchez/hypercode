import type { Lesson } from "../../types";

export const marginsPadding: Lesson = {
  id: "margins-padding",
  title: "Margins & Padding",
  chapterId: "box-model",
  content: `## Margins and Padding

Both use the same shorthand syntax:

\`\`\`css
/* All four sides */
margin: 16px;

/* Top/bottom, Left/right */
margin: 16px 24px;

/* Top, Right, Bottom, Left (clockwise) */
margin: 8px 16px 24px 32px;

/* Individual sides */
margin-top: 8px;
margin-right: 16px;
margin-bottom: 8px;
margin-left: 16px;
\`\`\`

**Center horizontally** with \`auto\`:
\`\`\`css
.container {
  width: 600px;
  margin: 0 auto;  /* top/bottom: 0, left/right: auto */
}
\`\`\`

**Negative margins** pull elements closer:
\`\`\`css
.overlap { margin-top: -20px; }
\`\`\`

Padding uses the same syntax but adds space *inside* the border.

### Your Task

Create a card with \`margin\` (including \`auto\` for centering) and \`padding\`.`,

  starterCode: `<style>
/* Use margin and padding */
</style>
<div class="card">
  <p class="spaced">Content</p>
</div>
`,

  solution: `<style>
.card { margin: 20px auto; padding: 24px; background: #f8fafc; width: 300px; }
.spaced { margin-top: 16px; margin-bottom: 16px; padding-left: 8px; padding-right: 8px; }
</style>
<div class="card">
  <p class="spaced">Content with spacing</p>
</div>
`,

  tests: [
    {
      name: "uses margin property",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/\\bmargin\\s*:/i.test(html));`,
    },
    {
      name: "uses padding property",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/\\bpadding\\s*:/i.test(html));`,
    },
    {
      name: "uses auto for centering",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/auto/i.test(html));`,
    },
    {
      name: "uses margin with auto shorthand",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/margin\\s*:.*auto/i.test(html));`,
    },
  ],
};
