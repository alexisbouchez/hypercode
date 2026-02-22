import type { Lesson } from "../../types";

export const borders: Lesson = {
  id: "borders",
  title: "Borders",
  chapterId: "box-model",
  content: `## CSS Borders

The \`border\` shorthand sets width, style, and color:

\`\`\`css
border: 2px solid #3b82f6;
\`\`\`

Border styles: \`solid\`, \`dashed\`, \`dotted\`, \`double\`, \`none\`

Individual sides:
\`\`\`css
border-top: 3px solid red;
border-right: 1px dashed gray;
border-bottom: none;
\`\`\`

**Rounded corners** with \`border-radius\`:
\`\`\`css
border-radius: 8px;         /* all corners */
border-radius: 50%;          /* circle */
border-radius: 8px 0 8px 0;  /* top-left, top-right, bottom-right, bottom-left */
\`\`\`

**Outline** is like border but outside the box model (doesn't affect layout):
\`\`\`css
outline: 2px solid blue;
outline-offset: 4px;
\`\`\`

### Your Task

Create elements with a solid border, a dashed border, and rounded corners.`,

  starterCode: `<style>
/* Create solid, dashed, and rounded borders */
</style>
<div class="solid">Solid border</div>
<div class="dashed">Dashed border</div>
<div class="rounded">Rounded corners</div>
<div class="circle"></div>
`,

  solution: `<style>
.solid { border: 2px solid #3b82f6; padding: 12px; margin: 8px 0; }
.dashed { border: 2px dashed #ef4444; padding: 12px; margin: 8px 0; }
.rounded { border: 1px solid #6b7280; border-radius: 8px; padding: 12px; margin: 8px 0; }
.circle { width: 60px; height: 60px; border-radius: 50%; background: #a78bfa; }
</style>
<div class="solid">Solid border</div>
<div class="dashed">Dashed border</div>
<div class="rounded">Rounded corners</div>
<div class="circle"></div>
`,

  tests: [
    {
      name: "uses the border property",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/\\bborder\\s*:/i.test(html));`,
    },
    {
      name: "uses border-radius",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/border-radius\\s*:/i.test(html));`,
    },
    {
      name: "uses solid border style",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/solid/i.test(html));`,
    },
    {
      name: "uses dashed border style",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/dashed/i.test(html));`,
    },
  ],
};
