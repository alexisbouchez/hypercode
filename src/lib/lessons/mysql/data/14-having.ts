import type { Lesson } from "../../types";

export const having: Lesson = {
  id: "having",
  title: "GROUP BY and HAVING",
  chapterId: "aggregations",
  content: `## HAVING — Filtering Groups

\`WHERE\` filters rows before grouping. \`HAVING\` filters **after** grouping — it filters the aggregated results.

### The Difference

\`\`\`sql
-- WHERE filters individual rows BEFORE grouping
SELECT category, COUNT(*) AS cnt
FROM products
WHERE price > 20
GROUP BY category;

-- HAVING filters groups AFTER aggregation
SELECT category, COUNT(*) AS cnt
FROM products
GROUP BY category
HAVING COUNT(*) >= 3;
\`\`\`

You cannot use aggregate functions in \`WHERE\`. Use \`HAVING\` for that.

### HAVING Syntax

\`\`\`sql
SELECT category, COUNT(*) AS product_count
FROM products
GROUP BY category
HAVING COUNT(*) >= 3;
\`\`\`

This returns only categories that contain 3 or more products.

### Using Aliases in HAVING

MySQL allows referencing SELECT aliases in \`HAVING\`:

\`\`\`sql
SELECT category, COUNT(*) AS cnt
FROM products
GROUP BY category
HAVING cnt >= 3;
\`\`\`

This is a MySQL extension — it does not work in all databases (e.g., PostgreSQL requires repeating the expression).

### Combining WHERE and HAVING

Use both together to filter rows first, then filter groups:

\`\`\`sql
-- Categories with 2+ products priced over $30
SELECT category, COUNT(*) AS cnt, AVG(price) AS avg_price
FROM products
WHERE price > 30
GROUP BY category
HAVING COUNT(*) >= 2;
\`\`\`

Step 1: \`WHERE price > 30\` eliminates cheap products.
Step 2: Remaining rows are grouped by category.
Step 3: \`HAVING COUNT(*) >= 2\` keeps only groups with 2+ rows.

### HAVING with SUM and AVG

\`\`\`sql
-- Customers who have spent more than $500 total
SELECT customer_id, SUM(total) AS total_spent
FROM orders
GROUP BY customer_id
HAVING SUM(total) > 500;

-- Categories where average price exceeds $100
SELECT category, ROUND(AVG(price), 2) AS avg_price
FROM products
GROUP BY category
HAVING AVG(price) > 100;
\`\`\`

### Full Clause Order

The complete order of SQL clauses is:

\`\`\`sql
SELECT   columns
FROM     table
WHERE    row_filter
GROUP BY grouping_columns
HAVING   group_filter
ORDER BY sort_columns
LIMIT    n;
\`\`\`

This is the order they must be written, and it roughly matches how they are evaluated (except SELECT aliases are resolved last in most databases).

### Your Task

Find product categories that have **3 or more** products. Return the \`category\` and \`product_count\`.`,

  starterCode: `-- Find categories with 3 or more products
SELECT category, COUNT(*) AS product_count
FROM products
GROUP BY category
HAVING`,

  solution: `SELECT category, COUNT(*) AS product_count
FROM products
GROUP BY category
HAVING COUNT(*) >= 3;`,

  tests: [
    {
      name: "returns 2 categories with 3+ products",
      expected: '{"type":"rowCount","value":2}',
    },
    {
      name: "returns category and product_count columns",
      expected: '{"type":"contains","columns":["category","product_count"]}',
    },
    {
      name: "Electronics is included (has 3 products)",
      expected: '{"type":"contains","value":"Electronics"}',
    },
    {
      name: "Office is included (has 3 products)",
      expected: '{"type":"contains","value":"Office"}',
    },
  ],
};
