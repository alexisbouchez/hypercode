export const EXERCISE_SCHEMA = `
CREATE TABLE products (
  id SERIAL PRIMARY KEY,
  name TEXT NOT NULL,
  price DECIMAL(10,2) NOT NULL,
  category TEXT NOT NULL
);

CREATE TABLE users (
  id SERIAL PRIMARY KEY,
  name TEXT NOT NULL,
  email TEXT NOT NULL UNIQUE
);

CREATE TABLE customers (
  id SERIAL PRIMARY KEY,
  name TEXT NOT NULL,
  email TEXT NOT NULL UNIQUE
);

CREATE TABLE orders (
  id SERIAL PRIMARY KEY,
  customer_id INTEGER REFERENCES customers(id),
  product_id INTEGER REFERENCES products(id),
  quantity INTEGER NOT NULL DEFAULT 1,
  total DECIMAL(10,2) NOT NULL
);
`;

/** Human-readable summary of exercise tables for display in the lesson UI */
export const EXERCISE_SCHEMA_SUMMARY = [
  { table: "products", columns: ["id", "name", "price", "category"] },
  { table: "users", columns: ["id", "name", "email"] },
  { table: "customers", columns: ["id", "name", "email"] },
  { table: "orders", columns: ["id", "customer_id", "product_id", "quantity", "total"] },
] as const;

export const EXERCISE_DATA = `
INSERT INTO products (name, price, category) VALUES
  ('Laptop', 999.99, 'Electronics'),
  ('Headphones', 79.99, 'Electronics'),
  ('Coffee Maker', 49.99, 'Kitchen'),
  ('Notebook', 4.99, 'Office'),
  ('Pen Set', 12.99, 'Office'),
  ('Desk Lamp', 34.99, 'Office'),
  ('Water Bottle', 19.99, 'Kitchen'),
  ('Backpack', 59.99, 'Accessories');

INSERT INTO users (name, email) VALUES
  ('Alice', 'alice@example.com'),
  ('Bob', 'bob@example.com'),
  ('Charlie', 'charlie@example.com');

INSERT INTO customers (name, email) VALUES
  ('Alice', 'alice@example.com'),
  ('Bob', 'bob@example.com'),
  ('Charlie', 'charlie@example.com');

INSERT INTO orders (customer_id, product_id, quantity, total) VALUES
  (1, 1, 1, 999.99),
  (1, 2, 2, 159.98),
  (2, 3, 1, 49.99);
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
