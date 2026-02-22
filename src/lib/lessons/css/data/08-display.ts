import type { Lesson } from "../../types";

export const display: Lesson = {
  id: "display",
  title: "Display Types",
  chapterId: "box-model",
  content: `## CSS Display Property

The \`display\` property controls how an element participates in layout:

**\`block\`** — full width, starts on new line (\`div\`, \`p\`, \`h1\`)
\`\`\`css
display: block;
\`\`\`

**\`inline\`** — width/height ignored, flows with text (\`span\`, \`a\`, \`strong\`)
\`\`\`css
display: inline;
\`\`\`

**\`inline-block\`** — flows with text but respects width/height:
\`\`\`css
display: inline-block;
width: 100px;
height: 50px;
\`\`\`

**\`none\`** — hides the element completely (no space reserved):
\`\`\`css
display: none;
\`\`\`

**\`visibility: hidden\`** — hides but keeps space:
\`\`\`css
visibility: hidden;
\`\`\`

### Your Task

Create elements demonstrating \`block\`, \`inline\`, \`inline-block\`, and \`none\` display values.`,

  starterCode: `<style>
/* Use block, inline, inline-block, and none display values */
</style>
<div class="block-el">Block</div>
<span class="inline-el">Inline A</span>
<span class="inline-el">Inline B</span>
<div class="inline-block-el">Box 1</div>
<div class="inline-block-el">Box 2</div>
<div class="hidden">Hidden</div>
`,

  solution: `<style>
.block-el { display: block; background: #dbeafe; padding: 8px; margin: 4px 0; }
.inline-el { display: inline; background: #fef9c3; padding: 2px 4px; }
.inline-block-el { display: inline-block; background: #dcfce7; padding: 8px; margin: 4px; width: 80px; height: 40px; }
.hidden { display: none; }
</style>
<div class="block-el">Block element</div>
<span class="inline-el">Inline A</span>
<span class="inline-el">Inline B</span>
<div class="inline-block-el">Box 1</div>
<div class="inline-block-el">Box 2</div>
<div class="hidden">Hidden</div>
`,

  tests: [
    {
      name: "uses display block",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/display\\s*:\\s*block/i.test(html));`,
    },
    {
      name: "uses display inline",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/display\\s*:\\s*inline/i.test(html));`,
    },
    {
      name: "uses display none",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/display\\s*:\\s*none/i.test(html));`,
    },
    {
      name: "uses inline-block",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/inline-block/i.test(html));`,
    },
  ],
};
