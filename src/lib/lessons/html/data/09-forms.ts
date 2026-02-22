import type { Lesson } from "../../types";

export const forms: Lesson = {
  id: "forms",
  title: "Forms",
  chapterId: "forms",
  content: `## HTML Forms

Forms collect user input. The \`<form>\` tag wraps all form controls:

\`\`\`html
<form action="/submit" method="post">
  <label for="name">Your Name:</label>
  <input type="text" id="name" name="name">
  <button type="submit">Submit</button>
</form>
\`\`\`

Key elements:
- \`<form>\` — the container, with \`action\` (where to send data) and \`method\` (GET or POST)
- \`<input>\` — a form field (self-closing)
- \`<label>\` — a text label for an input; \`for\` links it to an \`input\`'s \`id\`
- \`<button type="submit">\` — submits the form

The \`placeholder\` attribute shows hint text inside an input:
\`\`\`html
<input type="text" placeholder="Enter your name">
\`\`\`

### Your Task

Create a form with:
- A \`<label>\` and a \`<input type="text">\`
- A submit button`,

  starterCode: `<!-- Create a form with a label, input, and submit button -->
`,

  solution: `<form action="#" method="post">
  <label for="name">Your Name:</label>
  <input type="text" id="name" name="name" placeholder="Enter your name">
  <button type="submit">Submit</button>
</form>
`,

  tests: [
    {
      name: "has a form element",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/<form/i.test(html));`,
    },
    {
      name: "has an input element",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/<input/i.test(html));`,
    },
    {
      name: "has a label element",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/<label/i.test(html));`,
    },
    {
      name: "has a submit button",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/type="submit"/i.test(html));`,
    },
  ],
};
