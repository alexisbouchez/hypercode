import type { Lesson } from "../../types";

export const keyManagement: Lesson = {
  id: "key-management",
  title: "Key Management",
  chapterId: "key-ops",
  content: `## Managing Keys

Redis provides several commands for inspecting and managing keys themselves — independent of their data type.

### KEYS — Find Keys by Pattern

\`\`\`
KEYS *              -- all keys
KEYS user:*         -- keys starting with "user:"
KEYS *:session      -- keys ending with ":session"
KEYS user:?         -- keys matching single character after "user:"
\`\`\`

Pattern wildcards:
- \`*\` — matches any number of characters
- \`?\` — matches exactly one character
- \`[abc]\` — matches one character from the set

> **Warning:** \`KEYS\` blocks the server while scanning all keys. Use \`SCAN\` in production.

### TYPE — Get Data Type

\`\`\`
SET mystr "hello"
RPUSH mylist "a" "b"
SADD myset "x" "y"
HSET myhash f1 v1

TYPE mystr     -- string
TYPE mylist    -- list
TYPE myset     -- set
TYPE myhash    -- hash
\`\`\`

### RENAME — Rename a Key

\`\`\`
SET old_name "value"
RENAME old_name new_name
GET new_name    -- "value"
EXISTS old_name -- 0
\`\`\`

If the new key already exists, it is overwritten.

### DEL — Delete Multiple Keys

\`\`\`
DEL key1 key2 key3    -- returns count of deleted keys
\`\`\`

### EXISTS — Check Multiple Keys

\`\`\`
SET a "1"
SET b "2"
EXISTS a b c    -- 2 (a and b exist, c does not)
\`\`\`

Note: \`EXISTS\` counts occurrences — if you pass the same key twice, it counts twice.

### COPY — Copy a Key

\`\`\`
SET source "original"
COPY source destination
GET destination    -- "original"
\`\`\`

Returns 1 if copied, 0 if destination already exists.

### OBJECT ENCODING — Internal Representation

\`\`\`
SET num 42
OBJECT ENCODING num    -- "int"

SET short "hello"
OBJECT ENCODING short  -- "embstr"
\`\`\`

### Your Task

Set three keys: \`user:1\` → \`"Alice"\`, \`user:2\` → \`"Bob"\`, \`product:1\` → \`"Laptop"\`. Then use \`KEYS user:*\` to find only user keys.`,

  starterCode: `# Set some keys
SET user:1 "Alice"
SET user:2 "Bob"
SET product:1 "Laptop"

# Find only user keys
KEYS user:*`,

  solution: `SET user:1 "Alice"
SET user:2 "Bob"
SET product:1 "Laptop"
KEYS user:*`,

  tests: [
    {
      name: "KEYS user:* returns user:1",
      expected: '{"type":"contains","value":"\\"user:1\\""}',
    },
    {
      name: "KEYS user:* returns user:2",
      expected: '{"type":"contains","value":"\\"user:2\\""}',
    },
    {
      name: "KEYS result has 2 items (not product:1)",
      expected: '{"type":"lineCount","value":5}',
    },
  ],
};
