import type { Lesson } from "../../types";

export const selectors: Lesson = {
  id: "selectors",
  title: "CSS Selectors",
  chapterId: "selectors",
  content: `## Types of CSS Selectors

**Element selector** — targets all elements of that type:
\`\`\`css
p { color: gray; }
\`\`\`

**Class selector** — targets elements with that class (starts with \`.\`):
\`\`\`css
.card { background: white; padding: 1em; }
\`\`\`

**ID selector** — targets a unique element (starts with \`#\`):
\`\`\`css
#title { font-size: 2em; }
\`\`\`

**Descendant selector** — targets elements inside another:
\`\`\`css
.card p { color: gray; }
\`\`\`

**Pseudo-class** — targets elements in a state:
\`\`\`css
a:hover { color: red; }
button:focus { outline: 2px solid blue; }
li:first-child { font-weight: bold; }
\`\`\`

### Specificity

When multiple rules target the same element, the browser uses **specificity** to decide which wins. From lowest to highest priority:

1. Element selectors (\`p\`, \`h1\`) — specificity **0-0-1**
2. Class selectors (\`.card\`), pseudo-classes (\`:hover\`) — specificity **0-1-0**
3. ID selectors (\`#title\`) — specificity **1-0-0**

If two rules have equal specificity, the one that appears **last** in the stylesheet wins. Inline \`style\` attributes override all of the above, and \`!important\` overrides everything (but avoid it when possible).

### Your Task

Write CSS using a class selector (\`.card\`), an ID selector (\`#title\`), a descendant selector, and a pseudo-class.`,

  starterCode: `<style>
/* Use class, id, descendant, and pseudo-class selectors */
</style>
<div class="card">
  <h2 id="title">Card Title</h2>
  <p>Card content goes here.</p>
  <a href="#">Read more</a>
</div>
`,

  solution: `<style>
.card { background: #f9fafb; padding: 1em; border-radius: 8px; }
#title { color: #1d4ed8; }
.card p { color: #6b7280; }
a:hover { color: #dc2626; }
</style>
<div class="card">
  <h2 id="title">Card Title</h2>
  <p>Card content goes here.</p>
  <a href="#">Read more</a>
</div>
`,

  tests: [
    {
      name: "has a class selector",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/\\.[a-zA-Z]/i.test(html));`,
    },
    {
      name: "has an id selector",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/#[a-zA-Z]/i.test(html));`,
    },
    {
      name: "has a pseudo-class",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/:[a-z]+\\s*\\{/i.test(html));`,
    },
    {
      name: "has a descendant selector",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/\\.\\w+\\s+[a-z]/i.test(html));`,
    },
  ],
};
