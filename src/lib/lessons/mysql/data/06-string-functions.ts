import type { Lesson } from "../../types";

export const stringFunctions: Lesson = {
  id: "string-functions",
  title: "String Functions",
  chapterId: "querying",
  content: `## String Functions in MySQL

MySQL provides a rich set of built-in functions for manipulating text. These are applied per-row in SELECT, WHERE, and other clauses.

### Case Conversion

\`\`\`sql
SELECT UPPER('hello');   -- 'HELLO'
SELECT LOWER('WORLD');   -- 'world'

SELECT UPPER(name) AS name_upper FROM products;
\`\`\`

### String Length

\`LENGTH(str)\` returns the number of bytes. For ASCII text this equals the number of characters. For multibyte UTF-8 characters, use \`CHAR_LENGTH(str)\`:

\`\`\`sql
SELECT LENGTH('Laptop');      -- 6
SELECT CHAR_LENGTH('Laptop'); -- 6 (same for ASCII)
SELECT LENGTH('café');        -- 5 (é is 2 bytes in UTF-8)
SELECT CHAR_LENGTH('café');   -- 4 (4 characters)
\`\`\`

### Substring Extraction

\`SUBSTRING(str, pos, len)\` extracts part of a string. Position starts at 1:

\`\`\`sql
SELECT SUBSTRING('Laptop', 1, 3);   -- 'Lap'
SELECT SUBSTRING('Headphones', 5);  -- 'phones' (from pos 5 to end)
\`\`\`

\`LEFT(str, n)\` and \`RIGHT(str, n)\` are shortcuts:

\`\`\`sql
SELECT LEFT('Laptop', 3);   -- 'Lap'
SELECT RIGHT('Laptop', 3);  -- 'top'
\`\`\`

### String Replacement

\`REPLACE(str, from_str, to_str)\` replaces all occurrences:

\`\`\`sql
SELECT REPLACE('Pen Set', 'Set', 'Pack');  -- 'Pen Pack'
SELECT REPLACE(name, ' ', '_') AS slug FROM products;
\`\`\`

### Concatenation

MySQL uses the \`CONCAT()\` function to join strings:

\`\`\`sql
SELECT CONCAT(name, ' - $', price) AS label FROM products;
-- 'Laptop - $999.99'
\`\`\`

\`CONCAT_WS(separator, str1, str2, ...)\` joins with a separator:

\`\`\`sql
SELECT CONCAT_WS(', ', name, category) FROM products;
-- 'Laptop, Electronics'
\`\`\`

### Trimming Whitespace

\`\`\`sql
SELECT TRIM('  hello  ');         -- 'hello'
SELECT LTRIM('  hello  ');        -- 'hello  '
SELECT RTRIM('  hello  ');        -- '  hello'
SELECT TRIM(LEADING 'x' FROM 'xxxHello');   -- 'Hello'
SELECT TRIM(TRAILING 'x' FROM 'Helloxxx'); -- 'Hello'
\`\`\`

### String Search

\`INSTR(str, substr)\` returns the position of the first occurrence (0 if not found):

\`\`\`sql
SELECT INSTR('Laptop', 'top');  -- 4
SELECT INSTR('Laptop', 'xyz');  -- 0
\`\`\`

\`LOCATE(substr, str)\` is equivalent:

\`\`\`sql
SELECT LOCATE('top', 'Laptop');  -- 4
\`\`\`

### Padding

\`LPAD\` and \`RPAD\` pad a string to a given length:

\`\`\`sql
SELECT LPAD('42', 5, '0');   -- '00042'
SELECT RPAD('hello', 8, '!');  -- 'hello!!!'
\`\`\`

### Your Task

Select the \`name\` converted to uppercase as \`name_upper\`, and the character length of the name as \`name_length\`, from \`products\` where the name is **longer than 8 characters**.`,

  starterCode: `-- Find products with long names
SELECT UPPER(name) AS name_upper, LENGTH(name) AS name_length
FROM products
WHERE`,

  solution: `SELECT UPPER(name) AS name_upper, LENGTH(name) AS name_length
FROM products
WHERE LENGTH(name) > 8;`,

  tests: [
    {
      name: "returns 5 products with long names",
      expected: '{"type":"rowCount","value":5}',
    },
    {
      name: "returns name_upper and name_length columns",
      expected: '{"type":"contains","columns":["name_upper","name_length"]}',
    },
    {
      name: "UPPER converts to uppercase (HEADPHONES)",
      expected: '{"type":"contains","value":"HEADPHONES"}',
    },
    {
      name: "includes WATER BOTTLE (12 chars)",
      expected: '{"type":"contains","value":"WATER BOTTLE"}',
    },
  ],
};
