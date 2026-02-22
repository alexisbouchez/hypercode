import type { Lesson } from "../../types";

export const typography: Lesson = {
  id: "typography",
  title: "Typography",
  chapterId: "selectors",
  content: `## CSS Typography

Control how text looks with these properties:

\`\`\`css
h1 {
  font-family: Georgia, serif;    /* font stack */
  font-size: 2rem;                /* size */
  font-weight: bold;              /* 100-900 or bold/normal */
  font-style: italic;
  line-height: 1.5;               /* spacing between lines */
  letter-spacing: 0.05em;        /* space between letters */
  text-transform: uppercase;     /* uppercase, lowercase, capitalize */
  text-decoration: underline;    /* underline, none, line-through */
  text-align: center;            /* left, right, center, justify */
}
\`\`\`

**Font stacks** list fallback fonts:
\`\`\`css
font-family: "Helvetica Neue", Arial, sans-serif;
\`\`\`

**Units:**
- \`rem\` — relative to root font size (16px by default)
- \`em\` — relative to parent element's font size
- \`px\` — absolute pixels

### Your Task

Style headings and paragraphs with \`font-family\`, \`font-size\`, \`font-weight\`, and \`line-height\`.`,

  starterCode: `<style>
/* Style h1 and p with typography properties */
</style>
<h1>Beautiful Typography</h1>
<p>This paragraph uses custom font settings.</p>
<p class="small">Smaller, lighter text.</p>
`,

  solution: `<style>
h1 { font-family: Georgia, serif; font-size: 2rem; font-weight: bold; }
p { font-family: Arial, sans-serif; font-size: 1rem; line-height: 1.7; letter-spacing: 0.02em; }
.small { font-size: 0.875rem; font-weight: 300; }
</style>
<h1>Beautiful Typography</h1>
<p>This paragraph uses custom font settings for better readability.</p>
<p class="small">Smaller, lighter text for captions and metadata.</p>
`,

  tests: [
    {
      name: "uses font-family",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/font-family\\s*:/i.test(html));`,
    },
    {
      name: "uses font-size",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/font-size\\s*:/i.test(html));`,
    },
    {
      name: "uses line-height",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/line-height\\s*:/i.test(html));`,
    },
    {
      name: "uses font-weight",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/font-weight\\s*:/i.test(html));`,
    },
  ],
};
