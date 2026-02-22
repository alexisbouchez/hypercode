import type { Lesson } from "../../types";

export const tables: Lesson = {
  id: "tables",
  title: "Tables",
  chapterId: "content",
  content: `## HTML Tables

Tables organize data in rows and columns. The structure:

\`\`\`html
<table>
  <thead>
    <tr>
      <th>Name</th>
      <th>Score</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>Alice</td>
      <td>95</td>
    </tr>
    <tr>
      <td>Bob</td>
      <td>87</td>
    </tr>
  </tbody>
</table>
\`\`\`

Tags used:
- \`<table>\` — the container
- \`<thead>\` — header section
- \`<tbody>\` — body section
- \`<tr>\` — table row
- \`<th>\` — header cell (bold by default)
- \`<td>\` — data cell

### Your Task

Create a table with:
- A \`<thead>\` with at least 3 column headers (\`<th>\`)
- A \`<tbody>\` with at least 2 rows of data`,

  starterCode: `<!-- Create a table with thead and tbody -->
`,

  solution: `<table>
  <thead>
    <tr>
      <th>Name</th>
      <th>Age</th>
      <th>City</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>Alice</td>
      <td>30</td>
      <td>New York</td>
    </tr>
    <tr>
      <td>Bob</td>
      <td>25</td>
      <td>London</td>
    </tr>
  </tbody>
</table>
`,

  tests: [
    {
      name: "has a table element",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/<table>/i.test(html));`,
    },
    {
      name: "has a thead element",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/<thead>/i.test(html));`,
    },
    {
      name: "has a tbody element",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/<tbody>/i.test(html));`,
    },
    {
      name: "has th header cells",
      expected: "true\n",
      code: `const html = {{HTML}}; console.log(/<th>/i.test(html));`,
    },
  ],
};
