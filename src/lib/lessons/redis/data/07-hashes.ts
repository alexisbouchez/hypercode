import type { Lesson } from "../../types";

export const hashes: Lesson = {
  id: "hashes",
  title: "Hashes",
  chapterId: "collections",
  content: `## Redis Hashes

A Redis hash is a map between string field names and string values. Hashes are perfect for representing objects — a user profile, product details, session data — while keeping everything in a single key.

### HSET — Set Fields

\`\`\`
HSET user:1 name "Alice" email "alice@example.com" age "30"
\`\`\`

Returns the number of **new** fields added (not updated). Existing fields are updated silently.

### HGET — Get a Single Field

\`\`\`
HGET user:1 name
HGET user:1 email
\`\`\`

Output:
\`\`\`
"Alice"
"alice@example.com"
\`\`\`

### HGETALL — Get All Fields and Values

\`\`\`
HGETALL user:1
\`\`\`

Output (interleaved field/value pairs):
\`\`\`
1) "name"
2) "Alice"
3) "email"
4) "alice@example.com"
5) "age"
6) "30"
\`\`\`

### HKEYS, HVALS, HLEN

\`\`\`
HKEYS user:1     -- all field names
HVALS user:1     -- all field values
HLEN user:1      -- number of fields
\`\`\`

### HEXISTS — Check if Field Exists

\`\`\`
HEXISTS user:1 name    -- 1 (exists)
HEXISTS user:1 phone   -- 0 (not found)
\`\`\`

### HDEL — Delete Fields

\`\`\`
HDEL user:1 age
\`\`\`

### HMGET — Get Multiple Fields

\`\`\`
HMGET user:1 name email phone
\`\`\`

Output:
\`\`\`
1) "Alice"
2) "alice@example.com"
3) (nil)
\`\`\`

### HINCRBY — Increment a Hash Field

\`\`\`
HSET product:42 stock "100"
HINCRBY product:42 stock -3
HGET product:42 stock    -- "97"
\`\`\`

### Why Hashes?

Compared to storing a JSON string:
- Access individual fields without parsing the whole value
- Modify one field without rewriting the entire object
- Check if a field exists without loading all data

### Your Task

Store a product in a hash \`product:1\` with fields: \`name\` → \`"Laptop"\`, \`price\` → \`"999"\`, \`stock\` → \`"50"\`. Then use HGETALL to verify.`,

  starterCode: `# Store product:1 fields
HSET product:1 name "Laptop" price "999" stock "50"

# Get all fields
HGETALL product:1`,

  solution: `HSET product:1 name "Laptop" price "999" stock "50"
HGETALL product:1`,

  tests: [
    {
      name: "HSET added 3 new fields",
      expected: '{"type":"contains","value":"(integer) 3"}',
    },
    {
      name: "HGETALL contains Laptop",
      expected: '{"type":"contains","value":"\\"Laptop\\""}',
    },
    {
      name: "HGETALL contains price field",
      expected: '{"type":"contains","value":"\\"price\\""}',
    },
  ],
};
