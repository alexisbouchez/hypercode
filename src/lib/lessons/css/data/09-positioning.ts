import type { Lesson } from "../../types";

export const positioning: Lesson = {
  id: "positioning",
  title: "Positioning",
  chapterId: "layout",
  content: `## CSS Positioning

The \`position\` property controls how an element is placed:

**\`static\`** (default) — normal document flow.

**\`relative\`** — offset from its normal position:
\`\`\`css
.box { position: relative; top: 10px; left: 20px; }
\`\`\`

**\`absolute\`** — removed from flow, positioned relative to nearest non-static ancestor:
\`\`\`css
.parent { position: relative; }
.child  { position: absolute; top: 0; right: 0; }
\`\`\`

**\`fixed\`** — positioned relative to the viewport, stays when scrolling:
\`\`\`css
.nav { position: fixed; top: 0; width: 100%; }
\`\`\`

**\`sticky\`** — relative until a scroll threshold, then fixed:
\`\`\`css
.header { position: sticky; top: 0; }
\`\`\`

**\`z-index\`** — stacking order (higher = on top):
\`\`\`css
.modal { z-index: 100; }
\`\`\`

### Your Task

Create a container with \`position: relative\`, an absolutely-positioned child, and a sticky element using \`z-index\`.`,

  starterCode: `<style>
/* Use relative, absolute, sticky positioning and z-index */
</style>
<div class="sticky-nav">Sticky Nav</div>
<div class="container">
  <p>Container</p>
  <div class="absolute-box">Absolute</div>
</div>
`,

  solution: `<style>
.container { position: relative; width: 300px; height: 200px; background: #f1f5f9; }
.absolute-box { position: absolute; top: 20px; right: 20px; background: #fbbf24; padding: 8px; }
.sticky-nav { position: sticky; top: 0; background: #1e40af; color: white; padding: 8px; z-index: 10; }
</style>
<div class="sticky-nav">Sticky Navigation</div>
<div class="container">
  <p>Relative container</p>
  <div class="absolute-box">Absolute</div>
</div>
`,

  tests: [
    {
      name: "uses position relative",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/position\\s*:\\s*relative/i.test(html));`,
    },
    {
      name: "uses position absolute",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/position\\s*:\\s*absolute/i.test(html));`,
    },
    {
      name: "uses position sticky",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/position\\s*:\\s*sticky/i.test(html));`,
    },
    {
      name: "uses z-index",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/z-index\\s*:/i.test(html));`,
    },
  ],
};
