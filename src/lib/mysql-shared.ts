export const EXERCISE_SCHEMA = `
CREATE TABLE products (
  id INTEGER PRIMARY KEY,
  name VARCHAR(255) NOT NULL,
  price DECIMAL(10,2) NOT NULL,
  category VARCHAR(100) NOT NULL,
  stock INT NOT NULL DEFAULT 0
);

CREATE TABLE customers (
  id INTEGER PRIMARY KEY,
  name VARCHAR(255) NOT NULL,
  email VARCHAR(255) NOT NULL UNIQUE,
  city VARCHAR(100)
);

CREATE TABLE orders (
  id INTEGER PRIMARY KEY,
  customer_id INT NOT NULL,
  total DECIMAL(10,2) NOT NULL,
  status VARCHAR(50) NOT NULL DEFAULT 'pending',
  order_date DATE DEFAULT (date('now')),
  FOREIGN KEY (customer_id) REFERENCES customers(id)
);

CREATE TABLE order_items (
  id INTEGER PRIMARY KEY,
  order_id INT NOT NULL,
  product_id INT NOT NULL,
  quantity INT NOT NULL DEFAULT 1,
  unit_price DECIMAL(10,2) NOT NULL,
  FOREIGN KEY (order_id) REFERENCES orders(id),
  FOREIGN KEY (product_id) REFERENCES products(id)
);
`;

/** Human-readable summary of exercise tables for display in the lesson UI */
export const EXERCISE_SCHEMA_SUMMARY = [
  { table: "products", columns: ["id", "name", "price", "category", "stock"] },
  { table: "customers", columns: ["id", "name", "email", "city"] },
  { table: "orders", columns: ["id", "customer_id", "total", "status", "order_date"] },
  { table: "order_items", columns: ["id", "order_id", "product_id", "quantity", "unit_price"] },
] as const;

export const EXERCISE_DATA = `
INSERT INTO products VALUES
  (1, 'Laptop', 999.99, 'Electronics', 50),
  (2, 'Headphones', 79.99, 'Electronics', 200),
  (3, 'Smartphone', 699.99, 'Electronics', 100),
  (4, 'Coffee Maker', 49.99, 'Kitchen', 75),
  (5, 'Blender', 39.99, 'Kitchen', 60),
  (6, 'Notebook', 4.99, 'Office', 500),
  (7, 'Pen Set', 12.99, 'Office', 300),
  (8, 'Desk Lamp', 34.99, 'Office', 150),
  (9, 'Backpack', 59.99, 'Accessories', 80),
  (10, 'Water Bottle', 19.99, 'Accessories', 200);

INSERT INTO customers VALUES
  (1, 'Alice Johnson', 'alice@example.com', 'New York'),
  (2, 'Bob Smith', 'bob@example.com', 'Los Angeles'),
  (3, 'Carol Davis', 'carol@example.com', 'Chicago'),
  (4, 'Dave Wilson', 'dave@example.com', 'Houston'),
  (5, 'Eve Martinez', 'eve@example.com', NULL);

INSERT INTO orders VALUES
  (1, 1, 1079.98, 'completed', '2024-01-15'),
  (2, 2, 49.99, 'completed', '2024-02-20'),
  (3, 1, 34.99, 'shipped', '2024-03-10'),
  (4, 3, 779.98, 'pending', '2024-03-15'),
  (5, 4, 17.98, 'cancelled', '2024-04-01'),
  (6, 2, 699.99, 'pending', '2024-04-10');

INSERT INTO order_items VALUES
  (1, 1, 1, 1, 999.99),
  (2, 1, 2, 1, 79.99),
  (3, 2, 4, 1, 49.99),
  (4, 3, 8, 1, 34.99),
  (5, 4, 3, 1, 699.99),
  (6, 4, 2, 1, 79.99),
  (7, 5, 6, 1, 4.99),
  (8, 5, 7, 1, 12.99),
  (9, 6, 3, 1, 699.99);
`;

export interface ValidationSpec {
  type: "rowCount" | "contains" | "exact" | "custom";
  value?: string | number;
  columns?: string[];
}

export function parseValidation(expected: string): ValidationSpec {
  try {
    return JSON.parse(expected) as ValidationSpec;
  } catch {
    return { type: "custom" };
  }
}

export function evaluateValidation(
  spec: ValidationSpec,
  columns: string[],
  rows: Record<string, unknown>[],
): { passed: boolean; actual: string; expected: string } {
  switch (spec.type) {
    case "rowCount": {
      const expectedCount = Number(spec.value);
      return {
        passed: rows.length === expectedCount,
        actual: `${rows.length} rows`,
        expected: `${expectedCount} rows`,
      };
    }
    case "contains": {
      if (spec.columns) {
        const lower = columns.map((c) => c.toLowerCase());
        const missing = spec.columns.filter(
          (c) => !lower.includes(c.toLowerCase()),
        );
        return {
          passed: missing.length === 0,
          actual: `columns: [${columns.join(", ")}]`,
          expected: `columns include: [${spec.columns.join(", ")}]`,
        };
      }
      const val = String(spec.value ?? "");
      const found = rows.some((row) =>
        Object.values(row).some((v) =>
          String(v).toLowerCase().includes(val.toLowerCase()),
        ),
      );
      return {
        passed: found,
        actual: found ? `found "${val}"` : `"${val}" not found`,
        expected: `contains "${val}"`,
      };
    }
    case "exact": {
      const val = String(spec.value ?? "");
      const firstVal =
        rows.length > 0
          ? String(Object.values(rows[0])[0] ?? "")
          : "";
      return {
        passed: firstVal === val,
        actual: firstVal,
        expected: val,
      };
    }
    case "custom":
    default: {
      return {
        passed: rows.length > 0,
        actual: rows.length > 0 ? `${rows.length} rows returned` : "0 rows",
        expected: "at least 1 row",
      };
    }
  }
}
