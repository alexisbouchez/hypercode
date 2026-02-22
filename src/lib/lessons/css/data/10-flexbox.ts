import type { Lesson } from "../../types";

export const flexbox: Lesson = {
  id: "flexbox",
  title: "Flexbox",
  chapterId: "layout",
  content: `## CSS Flexbox

Flexbox makes one-dimensional layouts easy. Apply \`display: flex\` to the **container**:

\`\`\`css
.container {
  display: flex;
  flex-direction: row;        /* row (default) | column */
  justify-content: center;    /* main axis alignment */
  align-items: center;        /* cross axis alignment */
  gap: 16px;                  /* space between items */
}
\`\`\`

**\`justify-content\`** values: \`flex-start\`, \`flex-end\`, \`center\`, \`space-between\`, \`space-around\`, \`space-evenly\`

**\`align-items\`** values: \`flex-start\`, \`flex-end\`, \`center\`, \`stretch\`, \`baseline\`

**Flex items** (the children):
\`\`\`css
.item {
  flex: 1;          /* grow to fill available space */
  flex-shrink: 0;   /* don't shrink */
}
\`\`\`

Centering anything with flexbox:
\`\`\`css
.center {
  display: flex;
  justify-content: center;
  align-items: center;
}
\`\`\`

### Your Task

Create a flex container with items aligned using \`flex-direction\`, \`align-items\`, \`justify-content\`, and \`gap\`.`,

  starterCode: `<style>
/* Create a flexbox layout */
</style>
<div class="flex-container">
  <div class="flex-item">Item 1</div>
  <div class="flex-item">Item 2</div>
  <div class="flex-item">Item 3</div>
</div>
`,

  solution: `<style>
.flex-container { display: flex; flex-direction: row; align-items: center; justify-content: space-between; gap: 16px; padding: 16px; background: #f8fafc; }
.flex-item { background: #bfdbfe; padding: 12px 20px; border-radius: 4px; }
</style>
<div class="flex-container">
  <div class="flex-item">Item 1</div>
  <div class="flex-item">Item 2</div>
  <div class="flex-item">Item 3</div>
</div>
`,

  tests: [
    {
      name: "uses display flex",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/display\\s*:\\s*flex/i.test(html));`,
    },
    {
      name: "uses flex-direction",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/flex-direction\\s*:/i.test(html));`,
    },
    {
      name: "uses align-items",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/align-items\\s*:/i.test(html));`,
    },
    {
      name: "uses justify-content",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/justify-content\\s*:/i.test(html));`,
    },
  ],
};
