import type { Lesson } from "../../types";

export const grid: Lesson = {
  id: "grid",
  title: "CSS Grid",
  chapterId: "layout",
  content: `## CSS Grid

Grid makes two-dimensional layouts easy. Apply \`display: grid\` to the container:

\`\`\`css
.grid {
  display: grid;
  grid-template-columns: repeat(3, 1fr);  /* 3 equal columns */
  grid-template-rows: auto;
  gap: 16px;
}
\`\`\`

**\`fr\`** — "fraction" unit; \`1fr 2fr 1fr\` = 25%, 50%, 25%

**\`repeat()\`** shorthand:
\`\`\`css
grid-template-columns: repeat(4, 1fr);     /* 4 equal columns */
grid-template-columns: 200px repeat(3, 1fr); /* fixed + fluid */
\`\`\`

**Spanning** multiple cells:
\`\`\`css
.wide { grid-column: span 2; }     /* spans 2 columns */
.tall { grid-row: span 3; }        /* spans 3 rows */
.hero { grid-column: 1 / -1; }    /* full width */
\`\`\`

### Your Task

Create a 3-column grid with \`gap\` and at least one item that \`span\`s 2 columns.`,

  starterCode: `<style>
/* Create a CSS grid layout */
</style>
<div class="grid">
  <div class="grid-item">1</div>
  <div class="grid-item">2</div>
  <div class="grid-item">3</div>
  <div class="span-2">Spans 2</div>
  <div class="grid-item">5</div>
</div>
`,

  solution: `<style>
.grid { display: grid; grid-template-columns: repeat(3, 1fr); gap: 16px; padding: 16px; }
.grid-item { background: #ddd6fe; padding: 20px; border-radius: 4px; text-align: center; }
.span-2 { grid-column: span 2; background: #bbf7d0; padding: 20px; border-radius: 4px; }
</style>
<div class="grid">
  <div class="grid-item">1</div>
  <div class="grid-item">2</div>
  <div class="grid-item">3</div>
  <div class="span-2">Spans 2</div>
  <div class="grid-item">5</div>
</div>
`,

  tests: [
    {
      name: "uses display grid",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/display\\s*:\\s*grid/i.test(html));`,
    },
    {
      name: "uses grid-template-columns",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/grid-template-columns\\s*:/i.test(html));`,
    },
    {
      name: "uses gap property",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/\\bgap\\s*:/i.test(html));`,
    },
    {
      name: "uses grid-column span",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/grid-column\\s*:/i.test(html));`,
    },
  ],
};
