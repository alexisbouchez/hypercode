import type { Lesson } from "../../types";

export const ctes: Lesson = {
  id: "ctes",
  title: "Common Table Expressions",
  chapterId: "subqueries-ctes",
  content: `## The WITH Clause

A Common Table Expression (CTE) is a named temporary result set defined using the \`WITH\` keyword. It exists only for the duration of the query that follows it. Think of it as giving a name to a subquery so you can reference it like a table.

\`\`\`sql
WITH expensive_products AS (
  SELECT name, price, category
  FROM products
  WHERE price > 50
)
SELECT * FROM expensive_products ORDER BY price DESC;
\`\`\`

The CTE \`expensive_products\` is defined first, then the main query selects from it.

### Benefits of CTEs

CTEs offer several advantages over nested subqueries:

- **Readability**: Complex queries are broken into named, logical steps instead of deeply nested subqueries.
- **Reuse**: You can reference the same CTE multiple times in the main query without repeating the logic.
- **Maintainability**: Each CTE can be understood and tested independently.

Compare a subquery approach:

\`\`\`sql
SELECT sub.category, sub.avg_price
FROM (
  SELECT category, AVG(price) AS avg_price
  FROM products
  GROUP BY category
) AS sub
WHERE sub.avg_price > 30;
\`\`\`

With the equivalent CTE:

\`\`\`sql
WITH category_stats AS (
  SELECT category, AVG(price) AS avg_price
  FROM products
  GROUP BY category
)
SELECT category, avg_price
FROM category_stats
WHERE avg_price > 30;
\`\`\`

The CTE version reads top-to-bottom in a natural order.

### Multiple CTEs

You can define multiple CTEs separated by commas:

\`\`\`sql
WITH category_stats AS (
  SELECT category, AVG(price) AS avg_price, COUNT(*) AS product_count
  FROM products
  GROUP BY category
),
expensive_categories AS (
  SELECT category, avg_price
  FROM category_stats
  WHERE avg_price > 30
)
SELECT * FROM expensive_categories;
\`\`\`

Later CTEs can reference earlier ones. This lets you build complex logic step by step.

> **Tip:** Think of multiple CTEs as a pipeline. Each one transforms the data a little further, and the final \`SELECT\` pulls together the result. This approach makes even very complex analytical queries manageable.

### Recursive CTEs

A recursive CTE references itself. It has two parts separated by \`UNION ALL\`:

1. **Anchor member**: The base case that provides the initial rows.
2. **Recursive member**: References the CTE itself and builds on the previous iteration.

**Generating a number sequence:**

\`\`\`sql
WITH RECURSIVE numbers AS (
  -- Anchor: start with 1
  SELECT 1 AS n

  UNION ALL

  -- Recursive: add 1 each iteration
  SELECT n + 1
  FROM numbers
  WHERE n < 10
)
SELECT n FROM numbers;
\`\`\`

This generates the numbers 1 through 10. The anchor produces \`1\`, then the recursive member keeps adding \`1\` until the condition \`n < 10\` stops it.

**Traversing a hierarchy:**

Recursive CTEs are perfect for hierarchical data such as organizational charts, category trees, or threaded comments:

\`\`\`sql
-- Assuming an employees table with id, name, and manager_id columns
WITH RECURSIVE org_chart AS (
  -- Anchor: start with top-level managers (no manager)
  SELECT id, name, manager_id, 1 AS level
  FROM employees
  WHERE manager_id IS NULL

  UNION ALL

  -- Recursive: find employees who report to the current level
  SELECT e.id, e.name, e.manager_id, oc.level + 1
  FROM employees e
  JOIN org_chart oc ON e.manager_id = oc.id
)
SELECT * FROM org_chart ORDER BY level, name;
\`\`\`

> **Warning:** A recursive CTE without a proper termination condition will run indefinitely. Always include a \`WHERE\` clause in the recursive member that limits the depth or row count. PostgreSQL will eventually raise an error, but it is better to explicitly prevent infinite loops.

### When to Use CTEs vs Subqueries

| Scenario | Prefer |
|----------|--------|
| Simple, one-off filter | Subquery |
| Referenced multiple times | CTE |
| Step-by-step transformations | CTE |
| Recursive traversal | CTE (required) |
| Performance-critical and optimizer needs to inline | Subquery |

In modern PostgreSQL (12+), the optimizer can inline non-recursive CTEs, so the performance difference is minimal in most cases.

### Your Task

Use a CTE to find products priced above the average.`,

  starterCode: `-- Use a CTE to find products priced above the average
WITH avg_price AS (
  SELECT AVG(price) AS value FROM products
)
`,

  solution: `WITH avg_price AS (
  SELECT AVG(price) AS value FROM products
)
SELECT name, price
FROM products, avg_price
WHERE price > avg_price.value;`,

  tests: [
    {
      name: "returns name and price columns",
      expected: '{"type":"contains","columns":["name","price"]}',
    },
    {
      name: "returns exactly one product above average (Laptop)",
      expected: '{"type":"rowCount","value":1}',
    },
  ],
};
