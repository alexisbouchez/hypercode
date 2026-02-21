import type { Lesson } from "../../types";

export const selectBasics: Lesson = {
  id: "select-basics",
  title: "SELECT Basics",
  chapterId: "getting-started",
  content: `## The SELECT Statement

The \`SELECT\` statement is the most used SQL command. It retrieves data from one or more tables.

### Selecting Specific Columns

Instead of \`SELECT *\` (all columns), you can name exactly which columns you want:

\`\`\`sql
SELECT name, price FROM products;
\`\`\`

This is better practice than \`SELECT *\` because:
- You get only the data you need
- Queries are faster — less data to transfer
- Your code doesn't break if new columns are added

### Column Aliases with AS

Use \`AS\` to rename a column in the result:

\`\`\`sql
SELECT name AS product_name, price AS cost
FROM products;
\`\`\`

Aliases are useful for:
- Making output more readable
- Renaming calculated expressions
- Shortening long column names

### DISTINCT — Removing Duplicates

\`SELECT DISTINCT\` returns unique values only:

\`\`\`sql
SELECT DISTINCT category FROM products;
\`\`\`

Without \`DISTINCT\`, every row in the table is returned even if it has the same category as another row. With \`DISTINCT\`, each unique category appears once.

### Expressions and Calculations

You can compute values directly in SELECT:

\`\`\`sql
SELECT
  name,
  price,
  price * 1.08 AS price_with_tax
FROM products;
\`\`\`

MySQL supports all arithmetic operators: \`+\`, \`-\`, \`*\`, \`/\`, \`%\` (modulo).

### String Literals

You can include constant text in results:

\`\`\`sql
SELECT name, 'in stock' AS availability
FROM products;
\`\`\`

### The Order of Clauses

In a full SELECT statement, clauses must appear in this order:

\`\`\`sql
SELECT   column1, column2
FROM     table_name
WHERE    condition
GROUP BY column
HAVING   group_condition
ORDER BY column
LIMIT    n;
\`\`\`

You will learn each clause in upcoming lessons. For now, focus on \`SELECT\` and \`FROM\`.

### Your Task

Use \`SELECT DISTINCT\` to find all unique product categories. Return only the \`category\` column.`,

  starterCode: `-- Find all unique categories
SELECT DISTINCT`,

  solution: `SELECT DISTINCT category FROM products;`,

  tests: [
    {
      name: "returns 4 distinct categories",
      expected: '{"type":"rowCount","value":4}',
    },
    {
      name: "returns category column",
      expected: '{"type":"contains","columns":["category"]}',
    },
    {
      name: "includes Electronics",
      expected: '{"type":"contains","value":"Electronics"}',
    },
    {
      name: "includes Office",
      expected: '{"type":"contains","value":"Office"}',
    },
  ],
};
