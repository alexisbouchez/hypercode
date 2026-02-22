import type { Lesson } from "../../types";

export const inputTypes: Lesson = {
  id: "input-types",
  title: "Input Types",
  chapterId: "forms",
  content: `## Input Types

The \`<input>\` tag supports many types via the \`type\` attribute:

\`\`\`html
<input type="text">       <!-- single-line text -->
<input type="email">      <!-- email address -->
<input type="password">   <!-- masked text -->
<input type="number" min="0" max="100">
<input type="checkbox">   <!-- tick box -->
<input type="radio" name="choice" value="a">
<input type="date">       <!-- date picker -->
<input type="color">      <!-- color picker -->
<input type="range" min="0" max="100">
\`\`\`

Radio buttons with the same \`name\` form a group — only one can be selected:
\`\`\`html
<input type="radio" name="size" value="s"> Small
<input type="radio" name="size" value="m"> Medium
<input type="radio" name="size" value="l"> Large
\`\`\`

### Your Task

Create a form with: email input, number input, checkbox, and radio button inputs.`,

  starterCode: `<!-- Create inputs of different types -->
<form>

</form>
`,

  solution: `<form>
  <input type="email" placeholder="Email address">
  <input type="number" min="0" max="100">
  <input type="checkbox" id="agree">
  <label for="agree">I agree</label>
  <input type="radio" name="choice" value="yes"> Yes
  <input type="date">
</form>
`,

  tests: [
    {
      name: "has an email input",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/type="email"/i.test(html));`,
    },
    {
      name: "has a number input",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/type="number"/i.test(html));`,
    },
    {
      name: "has a checkbox",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/type="checkbox"/i.test(html));`,
    },
    {
      name: "has a radio button",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/type="radio"/i.test(html));`,
    },
  ],
};
