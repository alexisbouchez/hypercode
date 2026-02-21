import type { Lesson } from "../../types";

export const updatingData: Lesson = {
  id: "updating-data",
  title: "Updating Data",
  chapterId: "schema",
  content: `## UPDATE

The \`UPDATE\` statement modifies existing rows.

### Basic Syntax

\`\`\`sql
UPDATE products
SET price = 899.99
WHERE id = 1;
\`\`\`

Always include a \`WHERE\` clause. Without it, **every row** in the table is updated.

### Updating Multiple Columns

Separate column assignments with commas:

\`\`\`sql
UPDATE products
SET price = 79.99, stock = 150
WHERE name = 'Headphones';
\`\`\`

### Expressions in SET

You can use expressions referencing the current value:

\`\`\`sql
-- Add 10 to current stock
UPDATE products SET stock = stock + 10 WHERE category = 'Kitchen';

-- Apply percentage change
UPDATE products SET price = ROUND(price * 1.05, 2) WHERE category = 'Electronics';

-- Concatenate to existing string
UPDATE customers SET city = CONCAT(city, ', USA') WHERE city IS NOT NULL;
\`\`\`

### Conditional Updates with CASE

\`CASE\` inside \`SET\` lets you apply different values per row:

\`\`\`sql
UPDATE orders
SET status = CASE
  WHEN total > 500 THEN 'priority'
  WHEN total > 100 THEN 'standard'
  ELSE 'economy'
END;
\`\`\`

### Updating Based on Other Tables (Subquery)

\`\`\`sql
-- Discount products that have never been ordered
UPDATE products
SET price = price * 0.8
WHERE id NOT IN (SELECT DISTINCT product_id FROM order_items);
\`\`\`

### Safe UPDATE Mode

MySQL Workbench and some clients enforce **safe update mode**: you must include a \`WHERE\` clause that references a primary key or use \`LIMIT\`. To disable this for a session:

\`\`\`sql
SET SQL_SAFE_UPDATES = 0;
\`\`\`

### Limiting Updates

\`LIMIT\` restricts how many rows are updated. Useful as a safety net:

\`\`\`sql
-- Update at most 1 row
UPDATE products SET stock = 0 WHERE category = 'Office' LIMIT 1;
\`\`\`

### Your Task

Increase the \`stock\` of all **Kitchen** products by \`10\`.`,

  starterCode: `-- Increase stock for Kitchen products by 10
UPDATE products
SET stock = stock + 10
WHERE`,

  solution: `UPDATE products
SET stock = stock + 10
WHERE category = 'Kitchen';`,

  tests: [
    {
      name: "Coffee Maker stock is now 85",
      code: `{{USER_SQL}}
---VALIDATE---
SELECT stock FROM products WHERE name='Coffee Maker'`,
      expected: '{"type":"exact","value":"85"}',
    },
    {
      name: "Blender stock is now 70",
      code: `{{USER_SQL}}
---VALIDATE---
SELECT stock FROM products WHERE name='Blender'`,
      expected: '{"type":"exact","value":"70"}',
    },
    {
      name: "only Kitchen products have updated stock (Coffee Maker > 80)",
      code: `{{USER_SQL}}
---VALIDATE---
SELECT COUNT(*) FROM products WHERE category='Kitchen' AND stock > 80`,
      expected: '{"type":"exact","value":"1"}',
    },
    {
      name: "non-Kitchen products are unchanged (Laptop stock still 50)",
      code: `{{USER_SQL}}
---VALIDATE---
SELECT stock FROM products WHERE name='Laptop'`,
      expected: '{"type":"exact","value":"50"}',
    },
  ],
};
