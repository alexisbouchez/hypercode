import type { Lesson } from "../../types";

export const yourFirstSelect: Lesson = {
  id: "your-first-select",
  title: "Your First SELECT",
  chapterId: "sql-basics",
  content: `## The SELECT Statement

The \`SELECT\` statement is the most fundamental SQL command. It retrieves data from one or more tables. Every query you write will start with \`SELECT\`.

### Selecting All Columns

The asterisk (\`*\`) is a wildcard that means "all columns":

\`\`\`sql
SELECT * FROM products;
\`\`\`

This returns every column and every row from the \`products\` table. It is useful for quick exploration, but in production code you should almost always list the specific columns you need.

> **Tip:** Avoid \`SELECT *\` in application code. It fetches more data than necessary, makes your queries fragile when columns are added or reordered, and can hurt performance on large tables.

### Selecting Specific Columns

List the columns you want, separated by commas:

\`\`\`sql
SELECT name, price FROM products;
\`\`\`

This returns only the \`name\` and \`price\` columns. The order of columns in the result matches the order you list them, not the order they were defined in the table.

\`\`\`sql
SELECT price, name FROM products;
\`\`\`

This returns \`price\` first, then \`name\`.

### Column Aliases with AS

You can rename columns in the output using \`AS\`. This does not change the underlying table, only the column name in the result set:

\`\`\`sql
SELECT name AS product_name, price AS cost FROM products;
\`\`\`

Aliases are useful when column names are ambiguous, when joining multiple tables, or when you want more readable output.

If your alias contains spaces or special characters, wrap it in double quotes:

\`\`\`sql
SELECT name AS "Product Name", price AS "Unit Price" FROM products;
\`\`\`

### Expressions in SELECT

You can use expressions and calculations in your \`SELECT\` list:

\`\`\`sql
SELECT name, price, price * 1.1 AS price_with_tax FROM products;
\`\`\`

This adds a computed column that shows the price with a 10% tax applied. The original \`price\` column is unchanged.

### Removing Duplicates with DISTINCT

If a column contains repeated values, use \`DISTINCT\` to return only unique values:

\`\`\`sql
SELECT DISTINCT category FROM products;
\`\`\`

This returns each category exactly once, no matter how many products belong to it.

### Your Task

Select only the \`name\` and \`price\` columns from the \`products\` table.`,

  starterCode: `-- Select the name and price columns from products
SELECT`,

  solution: `SELECT name, price FROM products;`,

  tests: [
    {
      name: "returns name and price columns",
      expected: '{"type":"contains","columns":["name","price"]}',
    },
  ],
};
