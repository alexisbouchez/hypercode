import type { Lesson } from "../../types";

export const animations: Lesson = {
  id: "animations",
  title: "Animations",
  chapterId: "styling",
  content: `## CSS Animations

Animations use \`@keyframes\` to define steps, then the \`animation\` property to apply them:

\`\`\`css
@keyframes slide-in {
  from { transform: translateX(-100%); }
  to   { transform: translateX(0); }
}

.element {
  animation: slide-in 0.5s ease forwards;
}
\`\`\`

**\`animation\` shorthand:** \`name duration timing-function delay iteration-count direction fill-mode\`

\`\`\`css
animation: bounce 1s ease-in-out infinite;
animation: spin 2s linear infinite;
\`\`\`

**Iteration count:** \`1\`, \`3\`, \`infinite\`

**Fill mode:** \`forwards\` (keep end state), \`backwards\` (apply start state during delay)

**Multi-step keyframes:**
\`\`\`css
@keyframes pulse {
  0%   { transform: scale(1); }
  50%  { transform: scale(1.1); }
  100% { transform: scale(1); }
}
\`\`\`

### Your Task

Create two animations using \`@keyframes\`: a bouncing effect and a spinning loader.`,

  starterCode: `<style>
/* Create @keyframes animations */
</style>
<div class="bouncing"></div>
<div class="spinning"></div>
`,

  solution: `<style>
@keyframes bounce { 0%, 100% { transform: translateY(0); } 50% { transform: translateY(-20px); } }
@keyframes spin { from { transform: rotate(0deg); } to { transform: rotate(360deg); } }
.bouncing { width: 50px; height: 50px; background: #f59e0b; border-radius: 50%; animation: bounce 1s ease-in-out infinite; margin-bottom: 16px; }
.spinning { width: 50px; height: 50px; border: 4px solid #6366f1; border-top-color: transparent; border-radius: 50%; animation: spin 1s linear infinite; }
</style>
<div class="bouncing"></div>
<div class="spinning"></div>
`,

  tests: [
    {
      name: "uses @keyframes",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/@keyframes/i.test(html));`,
    },
    {
      name: "uses animation property",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/\\banimation\\s*:/i.test(html));`,
    },
    {
      name: "uses infinite iteration",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/infinite/i.test(html));`,
    },
    {
      name: "uses transform in keyframes",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/transform\\s*:/i.test(html));`,
    },
  ],
};
