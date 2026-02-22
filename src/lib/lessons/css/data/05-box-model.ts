import type { Lesson } from "../../types";

export const boxModel: Lesson = {
  id: "box-model",
  title: "The Box Model",
  chapterId: "box-model",
  content: `## The CSS Box Model

Every HTML element is a rectangular box with four layers:

\`\`\`
┌─────────────────────────────┐
│           margin            │
│  ┌───────────────────────┐  │
│  │        border         │  │
│  │  ┌─────────────────┐  │  │
│  │  │     padding     │  │  │
│  │  │  ┌───────────┐  │  │  │
│  │  │  │  content  │  │  │  │
│  │  │  └───────────┘  │  │  │
│  │  └─────────────────┘  │  │
│  └───────────────────────┘  │
└─────────────────────────────┘
\`\`\`

By default, \`width\` and \`height\` only size the **content** box — padding and border add to the total size. Fix this with:

\`\`\`css
* {
  box-sizing: border-box;
}
\`\`\`

With \`border-box\`, \`width\` includes padding and border — much easier to reason about.

\`\`\`css
.box {
  width: 200px;      /* total width including padding & border */
  height: 100px;
  padding: 16px;
  border: 2px solid black;
  box-sizing: border-box;
}
\`\`\`

### Your Task

Set \`box-sizing: border-box\` globally and create two boxes with explicit \`width\` and \`height\`.`,

  starterCode: `<style>
/* Apply box-sizing globally and create two boxes */
</style>
<div class="box">Box 1</div>
<div class="tall">Box 2</div>
`,

  solution: `<style>
* { box-sizing: border-box; }
.box { width: 200px; height: 100px; background: #bfdbfe; padding: 8px; }
.tall { width: 150px; height: 200px; background: #bbf7d0; padding: 8px; }
</style>
<div class="box">200×100 box</div>
<div class="tall">150×200 box</div>
`,

  tests: [
    {
      name: "uses box-sizing",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/box-sizing\\s*:/i.test(html));`,
    },
    {
      name: "uses width property",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/\\bwidth\\s*:/i.test(html));`,
    },
    {
      name: "uses height property",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/\\bheight\\s*:/i.test(html));`,
    },
    {
      name: "uses border-box value",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/border-box/i.test(html));`,
    },
  ],
};
