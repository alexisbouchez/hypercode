import type { Lesson } from "../../types";

export const sortingLimiting: Lesson = {
  id: "sorting-limiting",
  title: "Sorting and Limiting",
  chapterId: "getting-started",
  content: `## ORDER BY

The \`ORDER BY\` clause sorts query results. Without it, the order of rows is undefined — SQLite may return them in any order.

\`\`\`sql
SELECT name, price FROM products ORDER BY price;
\`\`\`

This returns products sorted by price from lowest to highest (ascending order by default).

### ASC and DESC

Use \`ASC\` (ascending, default) or \`DESC\` (descending) to control sort direction:

\`\`\`sql
-- Cheapest first (ascending, default)
SELECT name, price FROM products ORDER BY price ASC;

-- Most expensive first (descending)
SELECT name, price FROM products ORDER BY price DESC;
\`\`\`

### Sorting by Multiple Columns

List multiple columns to sort by, separated by commas. The second column is used to break ties in the first:

\`\`\`sql
SELECT name, category, price
FROM products
ORDER BY category ASC, price DESC;
\`\`\`

This sorts by category alphabetically, and within each category, by price from highest to lowest.

### LIMIT

\`LIMIT\` restricts the number of rows returned:

\`\`\`sql
-- Return only the first 3 rows
SELECT name, price FROM products ORDER BY price DESC LIMIT 3;
\`\`\`

Always combine \`LIMIT\` with \`ORDER BY\` in practice — otherwise you get an arbitrary subset of rows, which is rarely useful.

### OFFSET

\`OFFSET\` skips a number of rows before returning results. Use it with \`LIMIT\` for pagination:

\`\`\`sql
-- Page 1: rows 1-3
SELECT name, price FROM products ORDER BY id LIMIT 3 OFFSET 0;

-- Page 2: rows 4-6
SELECT name, price FROM products ORDER BY id LIMIT 3 OFFSET 3;

-- Page 3: rows 7-8
SELECT name, price FROM products ORDER BY id LIMIT 3 OFFSET 6;
\`\`\`

SQLite also supports a shorthand: \`LIMIT offset, count\`. For example, \`LIMIT 3, 3\` skips 3 rows and returns 3. This is non-standard — prefer the \`LIMIT ... OFFSET ...\` form for clarity.

### NULL Ordering

By default, \`NULL\` values sort last in ascending order and first in descending order. SQLite follows standard behavior here.

### Your Task

Select the \`name\` and \`price\` of the **3 most expensive** products, sorted by price descending.`,

  starterCode: `-- Select the 3 most expensive products
SELECT name, price FROM products`,

  solution: `SELECT name, price FROM products ORDER BY price DESC LIMIT 3;`,

  tests: [
    {
      name: "returns name and price columns",
      expected: '{"type":"contains","columns":["name","price"]}',
    },
    {
      name: "returns exactly 3 rows",
      expected: '{"type":"rowCount","value":3}',
    },
  ],
};
