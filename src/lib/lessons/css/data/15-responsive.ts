import type { Lesson } from "../../types";

export const responsive: Lesson = {
  id: "responsive",
  title: "Responsive Design",
  chapterId: "styling",
  content: `## Responsive Design with Media Queries

Media queries apply CSS only when certain conditions are met:

\`\`\`css
@media (min-width: 768px) {
  .grid { grid-template-columns: repeat(2, 1fr); }
}

@media (max-width: 640px) {
  .nav { display: none; }
}
\`\`\`

**Mobile-first approach** — start with mobile styles, then add larger screens:
\`\`\`css
/* Mobile (default) */
.grid { grid-template-columns: 1fr; }

/* Tablet */
@media (min-width: 768px) {
  .grid { grid-template-columns: repeat(2, 1fr); }
}

/* Desktop */
@media (min-width: 1024px) {
  .grid { grid-template-columns: repeat(3, 1fr); }
}
\`\`\`

**Common breakpoints:**
- 640px — small phones
- 768px — tablets
- 1024px — laptops
- 1280px — desktops

**Relative units:**
- \`em\` / \`rem\` — scales with font size
- \`%\` — relative to parent
- \`vw\` / \`vh\` — viewport width/height

### Your Task

Create a responsive grid layout that shows 1 column on mobile, 2 on tablet (\`768px+\`), and 3 on desktop (\`1024px+\`).`,

  starterCode: `<style>
/* Create a responsive grid with media queries */
.container { max-width: 1200px; margin: 0 auto; padding: 0 1rem; }
.grid { display: grid; gap: 1rem; }
/* Add media queries here */
</style>
<div class="container">
  <div class="grid">
    <div class="card">Card 1</div>
    <div class="card">Card 2</div>
    <div class="card">Card 3</div>
  </div>
</div>
`,

  solution: `<style>
.container { max-width: 1200px; margin: 0 auto; padding: 0 1rem; }
.grid { display: grid; grid-template-columns: 1fr; gap: 1rem; }
@media (min-width: 768px) { .grid { grid-template-columns: repeat(2, 1fr); } }
@media (min-width: 1024px) { .grid { grid-template-columns: repeat(3, 1fr); } }
.card { background: #f8fafc; padding: 1.5rem; border-radius: 8px; border: 1px solid #e2e8f0; }
</style>
<div class="container">
  <div class="grid">
    <div class="card">Card 1</div>
    <div class="card">Card 2</div>
    <div class="card">Card 3</div>
  </div>
</div>
`,

  tests: [
    {
      name: "uses @media queries",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/@media/i.test(html));`,
    },
    {
      name: "uses min-width breakpoints",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/min-width/i.test(html));`,
    },
    {
      name: "uses max-width constraint",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/max-width\\s*:/i.test(html));`,
    },
    {
      name: "has multiple media queries",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log((html.match(/@media/gi)||[]).length >= 2);`,
    },
  ],
};
