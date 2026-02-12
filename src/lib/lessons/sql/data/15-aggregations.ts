import type { Lesson } from "../../types";

export const aggregations: Lesson = {
  id: "aggregations",
  title: "Aggregate Functions & GROUP BY",
  chapterId: "aggregations",
  content: `## Summarizing Data

Aggregate functions compute a single result from a set of rows. They are the foundation of data analysis in SQL, letting you answer questions like "how many?", "what is the total?", and "what is the average?".

### Core Aggregate Functions

PostgreSQL provides several built-in aggregate functions:

| Function | Description | Example |
|----------|-------------|---------|
| \`COUNT(*)\` | Number of rows | \`SELECT COUNT(*) FROM products;\` |
| \`COUNT(column)\` | Number of non-NULL values in a column | \`SELECT COUNT(category) FROM products;\` |
| \`SUM(column)\` | Sum of all values | \`SELECT SUM(price) FROM products;\` |
| \`AVG(column)\` | Average of all values | \`SELECT AVG(price) FROM products;\` |
| \`MIN(column)\` | Smallest value | \`SELECT MIN(price) FROM products;\` |
| \`MAX(column)\` | Largest value | \`SELECT MAX(price) FROM products;\` |

A simple aggregate query without \`GROUP BY\` collapses the entire table into a single row:

\`\`\`sql
SELECT COUNT(*) AS total_products,
       AVG(price) AS average_price,
       MIN(price) AS cheapest,
       MAX(price) AS most_expensive
FROM products;
\`\`\`

### COUNT(*) vs COUNT(column)

There is an important distinction:

- \`COUNT(*)\` counts all rows, including those with \`NULL\` values
- \`COUNT(column)\` counts only rows where \`column\` is not \`NULL\`

\`\`\`sql
-- Count all rows
SELECT COUNT(*) FROM orders;  -- 3

-- Count only rows with a non-NULL total
SELECT COUNT(total) FROM orders;  -- 3
\`\`\`

You can also count distinct values:

\`\`\`sql
SELECT COUNT(DISTINCT category) FROM products;  -- 4 unique categories
\`\`\`

### GROUP BY

\`GROUP BY\` divides rows into groups that share the same value in one or more columns. Aggregate functions then operate on each group independently:

\`\`\`sql
SELECT category, COUNT(*) AS product_count
FROM products
GROUP BY category;
\`\`\`

Result:

| category     | product_count |
|-------------|---------------|
| Electronics | 2             |
| Kitchen     | 2             |
| Office      | 3             |
| Accessories | 1             |

Each category becomes its own group, and \`COUNT(*)\` counts the rows within each group.

### The Non-Aggregated Column Rule

When using \`GROUP BY\`, every column in the \`SELECT\` list must either:

1. Appear in the \`GROUP BY\` clause, or
2. Be inside an aggregate function

This query is **valid**:

\`\`\`sql
SELECT category, AVG(price) FROM products GROUP BY category;
\`\`\`

This query will **fail**:

\`\`\`sql
SELECT category, name, AVG(price) FROM products GROUP BY category;
\`\`\`

The database cannot know which \`name\` to show for a group of multiple products. PostgreSQL enforces this rule strictly and will raise an error.

> **Tip:** If you need to include non-aggregated columns, either add them to \`GROUP BY\` (which creates more granular groups) or wrap them in an aggregate function like \`MIN(name)\` or \`ARRAY_AGG(name)\`.

### Grouping by Multiple Columns

You can group by more than one column to create finer-grained groups:

\`\`\`sql
SELECT category, price > 50 AS premium, COUNT(*)
FROM products
GROUP BY category, price > 50;
\`\`\`

### Combining Aggregates

Multiple aggregate functions can appear in the same query:

\`\`\`sql
SELECT category,
       COUNT(*) AS num_products,
       ROUND(AVG(price), 2) AS avg_price,
       MIN(price) AS min_price,
       MAX(price) AS max_price,
       SUM(price) AS total_value
FROM products
GROUP BY category
ORDER BY total_value DESC;
\`\`\`

> **Tip:** Use \`ROUND()\` with \`AVG()\` to control decimal places. Without it, PostgreSQL may return many decimal digits that clutter the output.

### Your Task

Find the number of products and the average price for each category. Your result should include the \`category\` column.`,

  starterCode: `-- Find product count and average price per category
SELECT`,

  solution: `SELECT category, COUNT(*) AS product_count, AVG(price) AS average_price
FROM products
GROUP BY category;`,

  tests: [
    {
      name: "returns results grouped by category",
      expected: '{"type":"contains","columns":["category"]}',
    },
    {
      name: "returns all 4 categories",
      expected: '{"type":"rowCount","value":4}',
    },
  ],
};
