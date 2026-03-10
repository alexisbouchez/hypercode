import type { Lesson } from "../../types";

export const views: Lesson = {
  id: "views",
  title: "Views & Materialized Views",
  chapterId: "advanced-sql",
  content: `## Views & Materialized Views

Just as the Enterprise's main viewscreen can display different sensor readings on demand, a **view** is a named query stored in the database. It acts like a virtual table — every time you select from it, PostgreSQL runs the underlying query and returns fresh results.

### CREATE VIEW

\`\`\`sql
CREATE VIEW expensive_products AS
SELECT name, price, category
FROM products
WHERE price > 50;
\`\`\`

Now you can query it like a regular table:

\`\`\`sql
SELECT * FROM expensive_products;
\`\`\`

Views are useful for — much like how Starfleet uses different bridge station displays for different officers:

- **Simplifying complex queries** — Give a readable name to a long JOIN or aggregation, like a tactical overlay.
- **Access control** — Expose only certain columns or rows to specific users, just as ensigns don't need access to classified mission data.
- **Consistency** — Define the logic once, reuse it everywhere.

### Updating Through Views

Simple views (single table, no aggregations, no DISTINCT) are **automatically updatable** in PostgreSQL:

\`\`\`sql
UPDATE expensive_products SET price = 899.99 WHERE name = 'Laptop';
\`\`\`

This updates the underlying \`products\` table. If the updated row no longer matches the view's \`WHERE\` clause, it disappears from the view.

### CREATE OR REPLACE VIEW

You can redefine a view without dropping it first:

\`\`\`sql
CREATE OR REPLACE VIEW expensive_products AS
SELECT name, price, category
FROM products
WHERE price > 100;
\`\`\`

### Materialized Views

A **materialized view** stores the query result physically on disk. It does **not** re-run the query on every access — you get a snapshot of the data at the time it was last refreshed.

\`\`\`sql
CREATE MATERIALIZED VIEW category_stats AS
SELECT category, COUNT(*) AS product_count, AVG(price) AS avg_price
FROM products
GROUP BY category;
\`\`\`

Querying a materialized view is fast because it reads pre-computed data:

\`\`\`sql
SELECT * FROM category_stats;
\`\`\`

### REFRESH MATERIALIZED VIEW

When the underlying data changes, the materialized view becomes stale. You must explicitly refresh it:

\`\`\`sql
REFRESH MATERIALIZED VIEW category_stats;
\`\`\`

This re-executes the original query and replaces the stored data. During a normal refresh, the view is locked for reads. To allow concurrent reads, add \`CONCURRENTLY\` (requires a unique index on the view).

### DISTINCT ON

PostgreSQL's \`DISTINCT ON\` lets you pick **one row per group** based on an ordering. This is extremely handy and not available in most other databases.

\`\`\`sql
SELECT DISTINCT ON (category) category, name, price
FROM products
ORDER BY category, price DESC;
\`\`\`

This returns **one row per category** — specifically the row with the highest price in each category. The \`ORDER BY\` must start with the \`DISTINCT ON\` columns, followed by the column that controls which row is picked.

### Your Task

Create a view called \`office_products\` that selects the \`name\` and \`price\` columns from the \`products\` table where the \`category\` is \`'Office'\`.

Then write a query using \`DISTINCT ON\` to select one product per category — the **most expensive** product in each category. Return the columns \`category\`, \`name\`, and \`price\`, ordered by \`category\` and then \`price DESC\`.`,

  starterCode: `-- 1. Create a view called office_products
--    that selects name and price from products
--    where category = 'Office'


-- 2. Select one product per category (the most expensive)
--    using DISTINCT ON
--    Return: category, name, price
--    Order by: category, price DESC
`,

  solution: `CREATE VIEW office_products AS
SELECT name, price
FROM products
WHERE category = 'Office';

SELECT DISTINCT ON (category) category, name, price
FROM products
ORDER BY category, price DESC;`,

  tests: [
    {
      name: "office_products view returns 3 Office products",
      code: `{{USER_SQL}}\n---VALIDATE---\nSELECT COUNT(*)::text AS cnt FROM office_products;`,
      expected: '{"type":"exact","value":"3"}',
    },
    {
      name: "office_products view contains Notebook",
      code: `{{USER_SQL}}\n---VALIDATE---\nSELECT name FROM office_products WHERE name = 'Notebook';`,
      expected: '{"type":"exact","value":"Notebook"}',
    },
    {
      name: "DISTINCT ON returns one row per category (4 categories)",
      code: `{{USER_SQL}}\n---VALIDATE---\nSELECT COUNT(*)::text AS cnt FROM (SELECT DISTINCT ON (category) category, name, price FROM products ORDER BY category, price DESC) t;`,
      expected: '{"type":"exact","value":"4"}',
    },
  ],
};
