import type { Lesson } from "../../types";

export const sortingAndLimiting: Lesson = {
  id: "sorting-and-limiting",
  title: "Sorting and Limiting",
  chapterId: "querying",
  content: `## Ordering Results with ORDER BY

By default, MySQL returns rows in an unspecified order. Use \`ORDER BY\` to sort results:

\`\`\`sql
SELECT name, price FROM products ORDER BY price;
\`\`\`

This sorts ascending (lowest first) by default.

### Ascending and Descending

\`\`\`sql
-- Ascending (A→Z, 0→9) — the default
SELECT name, price FROM products ORDER BY price ASC;

-- Descending (Z→A, 9→0)
SELECT name, price FROM products ORDER BY price DESC;
\`\`\`

### Sorting by Multiple Columns

You can sort by multiple columns. The first column is primary, the second breaks ties:

\`\`\`sql
SELECT name, category, price
FROM products
ORDER BY category ASC, price DESC;
\`\`\`

This sorts by category alphabetically, and within each category, by price from highest to lowest.

### Sorting by Column Position

You can also reference columns by their position in the SELECT list (1-indexed):

\`\`\`sql
SELECT name, price FROM products ORDER BY 2 DESC;
-- Equivalent to: ORDER BY price DESC
\`\`\`

This is common in quick queries but should be avoided in production code (fragile when column order changes).

### Limiting Results with LIMIT

\`LIMIT\` restricts how many rows are returned:

\`\`\`sql
SELECT name, price FROM products ORDER BY price DESC LIMIT 3;
\`\`\`

Returns the 3 most expensive products.

### LIMIT with OFFSET — Pagination

\`OFFSET\` skips rows before applying \`LIMIT\`. Together, they enable pagination:

\`\`\`sql
-- Page 1: rows 1-5
SELECT name, price FROM products ORDER BY price LIMIT 5 OFFSET 0;

-- Page 2: rows 6-10
SELECT name, price FROM products ORDER BY price LIMIT 5 OFFSET 5;

-- MySQL also supports: LIMIT offset, count
SELECT name, price FROM products ORDER BY price LIMIT 5, 5;
\`\`\`

### Top-N Queries

A very common pattern: find the top N records by some metric:

\`\`\`sql
-- Most expensive products
SELECT name, price FROM products ORDER BY price DESC LIMIT 3;

-- Cheapest products
SELECT name, price FROM products ORDER BY price ASC LIMIT 3;

-- Most recent orders
SELECT id, total, order_date FROM orders ORDER BY order_date DESC LIMIT 5;
\`\`\`

### Your Task

Select the \`name\` and \`price\` of the **5 most expensive products**, sorted from highest to lowest price.`,

  starterCode: `-- Get the 5 most expensive products
SELECT name, price
FROM products
ORDER BY`,

  solution: `SELECT name, price
FROM products
ORDER BY price DESC
LIMIT 5;`,

  tests: [
    {
      name: "returns exactly 5 products",
      expected: '{"type":"rowCount","value":5}',
    },
    {
      name: "returns name and price columns",
      expected: '{"type":"contains","columns":["name","price"]}',
    },
    {
      name: "Laptop is first (highest price)",
      expected: '{"type":"exact","value":"Laptop"}',
    },
    {
      name: "includes Coffee Maker (5th most expensive)",
      expected: '{"type":"contains","value":"Coffee Maker"}',
    },
  ],
};
