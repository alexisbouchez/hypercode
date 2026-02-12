import type { Lesson } from "../../types";

export const sortingAndLimiting: Lesson = {
  id: "sorting-and-limiting",
  title: "Sorting & Limiting Results",
  chapterId: "sql-basics",
  content: `## ORDER BY

By default, SQL does not guarantee the order of rows in a result set. If you need a specific order, use \`ORDER BY\`:

\`\`\`sql
SELECT * FROM products ORDER BY price;
\`\`\`

This sorts products by price in ascending order (lowest first). Ascending is the default.

### ASC and DESC

You can explicitly specify the sort direction:

\`\`\`sql
-- Cheapest first (ascending, the default)
SELECT * FROM products ORDER BY price ASC;

-- Most expensive first (descending)
SELECT * FROM products ORDER BY price DESC;
\`\`\`

### Sorting by Multiple Columns

You can sort by more than one column. Rows are sorted by the first column, and ties are broken by the second column, and so on:

\`\`\`sql
SELECT * FROM products ORDER BY category ASC, price DESC;
\`\`\`

This groups products by category alphabetically, and within each category, sorts by price from highest to lowest.

### Sorting by Column Position

You can also sort by column position (1-based) rather than column name:

\`\`\`sql
SELECT name, price FROM products ORDER BY 2 DESC;
\`\`\`

Here \`2\` refers to the second column in the \`SELECT\` list, which is \`price\`. This is sometimes convenient for complex expressions, but naming the column explicitly is usually more readable.

## LIMIT

\`LIMIT\` restricts the number of rows returned:

\`\`\`sql
SELECT * FROM products ORDER BY price DESC LIMIT 5;
\`\`\`

This returns only the 5 most expensive products.

> **Warning:** Using \`LIMIT\` without \`ORDER BY\` returns an arbitrary set of rows. The database makes no guarantee about which rows you will get. Always combine \`LIMIT\` with \`ORDER BY\` when you want deterministic results.

## OFFSET

\`OFFSET\` skips a number of rows before starting to return results. Combined with \`LIMIT\`, it enables pagination:

\`\`\`sql
-- Page 1: first 3 products
SELECT * FROM products ORDER BY id LIMIT 3 OFFSET 0;

-- Page 2: next 3 products
SELECT * FROM products ORDER BY id LIMIT 3 OFFSET 3;

-- Page 3: next 3 products
SELECT * FROM products ORDER BY id LIMIT 3 OFFSET 6;
\`\`\`

The general formula for pagination is:

\`\`\`
OFFSET = (page_number - 1) * page_size
LIMIT  = page_size
\`\`\`

> **Tip:** \`OFFSET\`-based pagination becomes slow on large tables because the database must scan and discard all the skipped rows. For better performance on large datasets, consider keyset pagination (also called "cursor-based pagination"), where you filter by the last seen value instead of using \`OFFSET\`.

### Combining ORDER BY, LIMIT, and OFFSET

These clauses always appear in this order:

\`\`\`sql
SELECT columns
FROM table
WHERE conditions
ORDER BY columns
LIMIT count
OFFSET skip;
\`\`\`

### Your Task

Get the 3 cheapest products. Return all columns, sorted by price in ascending order.`,

  starterCode: `-- Get the 3 cheapest products
SELECT`,

  solution: `SELECT * FROM products ORDER BY price ASC LIMIT 3;`,

  tests: [
    {
      name: "returns exactly 3 rows",
      expected: '{"type":"rowCount","value":3}',
    },
    {
      name: "returns the 3 cheapest (includes 4.99 and 19.99)",
      expected: '{"type":"contains","value":"4.99"}',
    },
    {
      name: "result includes third-cheapest price 19.99",
      expected: '{"type":"contains","value":"19.99"}',
    },
  ],
};
