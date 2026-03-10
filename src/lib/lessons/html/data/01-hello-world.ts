import type { Lesson } from "../../types";

export const helloWorld: Lesson = {
  id: "hello-world",
  title: "Hello, World!",
  chapterId: "structure",
  content: `## Your First HTML Element

HTML (HyperText Markup Language) is the standard language for creating web pages. HTML uses **tags** to mark up content. Tags are written with angle brackets:

\`\`\`html
<h1>Hello, World!</h1>
\`\`\`

- \`<h1>\` is the **opening tag**
- \`</h1>\` is the **closing tag** (note the \`/\`)
- Everything between the tags is the **content**

\`h1\` stands for "heading level 1" — the most important heading on the page.

> **Note:** Every HTML page should start with \`<!DOCTYPE html>\` followed by an \`<html>\` element. This tells the browser to use HTML5 standards mode. A minimal boilerplate looks like:
> \`\`\`html
> <!DOCTYPE html>
> <html lang="en">
>   <head><title>Page Title</title></head>
>   <body><!-- content here --></body>
> </html>
> \`\`\`
> We will skip the boilerplate in early lessons for simplicity, but you will use the full structure in later lessons.

### Your Task

Write an \`<h1>\` element containing the text \`Hello, World!\`.`,

  starterCode: `<!-- Write an h1 heading here -->
`,

  solution: `<h1>Hello, World!</h1>
`,

  tests: [
    {
      name: "uses an h1 element",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/<h1>/i.test(html));`,
    },
    {
      name: "contains Hello, World!",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(html.includes("Hello, World!"));`,
    },
    {
      name: "has a closing h1 tag",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/<\\/h1>/i.test(html));`,
    },
    {
      name: "uses a heading element",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/<h[1-6]/i.test(html));`,
    },
  ],
};
