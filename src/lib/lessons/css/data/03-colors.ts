import type { Lesson } from "../../types";

export const colors: Lesson = {
  id: "colors",
  title: "Colors",
  chapterId: "selectors",
  content: `## CSS Color Formats

CSS supports multiple color formats:

**Named colors** — human-readable:
\`\`\`css
color: tomato;
background: lightblue;
\`\`\`

**Hex** — \`#RRGGBB\` (red, green, blue in hex):
\`\`\`css
color: #e11d48;       /* red */
color: #3b82f6;       /* blue */
color: #fff;          /* white shorthand */
\`\`\`

**RGB** — values 0–255:
\`\`\`css
color: rgb(34, 197, 94);
background: rgba(0, 0, 0, 0.5);  /* with alpha */
\`\`\`

**HSL** — hue (0–360°), saturation (%), lightness (%):
\`\`\`css
color: hsl(271, 81%, 56%);
background: hsl(220, 100%, 97%);
\`\`\`

HSL is great for creating color palettes — keep the hue, vary the lightness.

### Your Task

Style 4 paragraphs using named, hex, RGB, and HSL color formats.`,

  starterCode: `<style>
/* Style each paragraph with a different color format */
</style>
<p class="named">Named color</p>
<p class="hex">Hex color</p>
<p class="rgb">RGB color</p>
<p class="hsl">HSL color</p>
`,

  solution: `<style>
.named { color: tomato; background: lightblue; padding: 8px; }
.hex { color: #e11d48; background: #dbeafe; padding: 8px; }
.rgb { color: rgb(34, 197, 94); background: rgb(240, 253, 244); padding: 8px; }
.hsl { color: hsl(271, 81%, 56%); background: hsl(270, 100%, 98%); padding: 8px; }
</style>
<p class="named">Named colors</p>
<p class="hex">Hex colors</p>
<p class="rgb">RGB colors</p>
<p class="hsl">HSL colors</p>
`,

  tests: [
    {
      name: "uses hex color notation",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/#[0-9a-fA-F]{3,6}/.test(html));`,
    },
    {
      name: "uses rgb() color notation",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/rgb\\(/i.test(html));`,
    },
    {
      name: "uses hsl() color notation",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/hsl\\(/i.test(html));`,
    },
    {
      name: "uses a named color",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/color\\s*:\\s*[a-z]+/i.test(html));`,
    },
  ],
};
