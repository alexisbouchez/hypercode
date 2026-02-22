import type { Lesson } from "../../types";

export const transitions: Lesson = {
  id: "transitions",
  title: "Transitions",
  chapterId: "styling",
  content: `## CSS Transitions

Transitions animate property changes smoothly:

\`\`\`css
.btn {
  background: blue;
  transition: background 0.3s ease;
}
.btn:hover {
  background: darkblue;
}
\`\`\`

**Transition shorthand:** \`property duration timing-function delay\`

\`\`\`css
transition: background 0.3s ease;
transition: all 0.2s ease-in-out;
transition: color 0.2s, transform 0.3s;  /* multiple */
\`\`\`

**Timing functions:**
- \`ease\` — slow → fast → slow (default)
- \`ease-in\` — starts slow
- \`ease-out\` — ends slow
- \`ease-in-out\` — starts and ends slow
- \`linear\` — constant speed
- \`cubic-bezier(0.4, 0, 0.2, 1)\` — custom

**Transform** for smooth animations:
\`\`\`css
.card:hover {
  transform: scale(1.05);       /* scale up 5% */
  transform: translateY(-4px);  /* lift up */
  transform: rotate(10deg);     /* rotate */
}
\`\`\`

### Your Task

Create a button and a box, each with a transition on hover. Use both \`transition\` and \`transform\`.`,

  starterCode: `<style>
/* Add transitions and hover transforms */
</style>
<button class="btn">Hover Me</button>
<div class="box"></div>
`,

  solution: `<style>
.btn { background: #3b82f6; color: white; padding: 10px 20px; border: none; border-radius: 4px; cursor: pointer; transition: background 0.3s ease, transform 0.2s ease; }
.btn:hover { background: #1d4ed8; transform: scale(1.05); }
.box { width: 100px; height: 100px; background: #f472b6; margin-top: 16px; transition: all 0.4s ease-in-out; }
.box:hover { width: 150px; background: #be185d; }
</style>
<button class="btn">Hover Me</button>
<div class="box"></div>
`,

  tests: [
    {
      name: "uses the transition property",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/\\btransition\\s*:/i.test(html));`,
    },
    {
      name: "uses an easing function",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/ease/i.test(html));`,
    },
    {
      name: "uses the transform property",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/\\btransform\\s*:/i.test(html));`,
    },
    {
      name: "has a :hover state",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/:hover\\s*\\{/i.test(html));`,
    },
  ],
};
