import type { Lesson } from "../../types";

export const backgrounds: Lesson = {
  id: "backgrounds",
  title: "Backgrounds",
  chapterId: "styling",
  content: `## CSS Backgrounds

**Solid color:**
\`\`\`css
background-color: #dbeafe;
/* or */
background: #dbeafe;
\`\`\`

**Linear gradient:**
\`\`\`css
background: linear-gradient(135deg, #667eea, #764ba2);
background: linear-gradient(to right, red, orange, yellow);
\`\`\`

**Radial gradient:**
\`\`\`css
background: radial-gradient(circle, #f9a8d4, #c084fc);
\`\`\`

**Background image:**
\`\`\`css
background-image: url("photo.jpg");
background-size: cover;    /* cover | contain | 100px | 50% */
background-position: center;
background-repeat: no-repeat;
\`\`\`

**Shorthand:**
\`\`\`css
background: url("photo.jpg") center/cover no-repeat;
\`\`\`

### Your Task

Create three elements with different background types: solid color, linear gradient, and radial gradient.`,

  starterCode: `<style>
/* Use solid, linear-gradient, and radial-gradient backgrounds */
</style>
<div class="solid-bg">Solid background</div>
<div class="gradient-bg">Linear gradient</div>
<div class="radial-bg">Radial gradient</div>
`,

  solution: `<style>
.solid-bg { background-color: #dbeafe; padding: 20px; margin: 8px 0; }
.gradient-bg { background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white; padding: 20px; margin: 8px 0; }
.radial-bg { background: radial-gradient(circle, #f9a8d4, #c084fc); padding: 20px; margin: 8px 0; }
</style>
<div class="solid-bg">Solid background</div>
<div class="gradient-bg">Linear gradient</div>
<div class="radial-bg">Radial gradient</div>
`,

  tests: [
    {
      name: "uses background-color property",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/background-color\\s*:/i.test(html));`,
    },
    {
      name: "uses linear-gradient",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/linear-gradient/i.test(html));`,
    },
    {
      name: "uses radial-gradient",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/radial-gradient/i.test(html));`,
    },
    {
      name: "uses background property",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/background\\s*:/i.test(html));`,
    },
  ],
};
