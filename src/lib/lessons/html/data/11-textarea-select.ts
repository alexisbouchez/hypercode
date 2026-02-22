import type { Lesson } from "../../types";

export const textareaSelect: Lesson = {
  id: "textarea-select",
  title: "Textarea & Select",
  chapterId: "forms",
  content: `## Textarea and Select

**\`<textarea>\`** — multi-line text input:
\`\`\`html
<textarea rows="4" cols="50" placeholder="Your message"></textarea>
\`\`\`

Unlike \`<input>\`, \`<textarea>\` has a closing tag and its default content goes between the tags.

**\`<select>\`** — dropdown menu:
\`\`\`html
<select name="country">
  <option value="us">United States</option>
  <option value="uk">United Kingdom</option>
  <option value="ca" selected>Canada</option>
</select>
\`\`\`

The \`selected\` attribute pre-selects an option. \`<optgroup>\` groups options:
\`\`\`html
<select>
  <optgroup label="Europe">
    <option>France</option>
    <option>Germany</option>
  </optgroup>
</select>
\`\`\`

### Your Task

Create a form with a \`<textarea>\` and a \`<select>\` dropdown with at least 2 options.`,

  starterCode: `<!-- Create a form with textarea and select -->
<form>

</form>
`,

  solution: `<form>
  <label for="message">Message:</label>
  <textarea id="message" name="message" rows="4" placeholder="Your message"></textarea>
  <label for="country">Country:</label>
  <select id="country" name="country">
    <option value="us">United States</option>
    <option value="uk">United Kingdom</option>
    <option value="ca">Canada</option>
  </select>
</form>
`,

  tests: [
    {
      name: "has a textarea element",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/<textarea/i.test(html));`,
    },
    {
      name: "has a select element",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/<select/i.test(html));`,
    },
    {
      name: "has at least 2 options",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log((html.match(/<option/gi)||[]).length >= 2);`,
    },
    {
      name: "has a rows attribute on textarea",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/rows=/i.test(html));`,
    },
  ],
};
