import type { Lesson } from "../../types";

export const stringOperations: Lesson = {
  id: "string-operations",
  title: "String Operations",
  chapterId: "strings",
  content: `## String Operations

Redis strings are binary-safe — they can contain any sequence of bytes. The maximum size is 512 MB.

### MSET and MGET — Multiple Keys

Setting and getting multiple keys in a single round-trip:

\`\`\`
MSET key1 val1 key2 val2 key3 val3
MGET key1 key2 key3
\`\`\`

Output:
\`\`\`
OK
1) "val1"
2) "val2"
3) "val3"
\`\`\`

\`MGET\` returns \`(nil)\` for keys that don't exist.

### APPEND — Concatenate

Append a value to an existing string. Returns the new length:

\`\`\`
SET log "2024-01-01: "
APPEND log "server started"
GET log
\`\`\`

Output:
\`\`\`
OK
(integer) 26
"2024-01-01: server started"
\`\`\`

If the key doesn't exist, APPEND creates it (like SET).

### STRLEN — String Length

\`\`\`
SET message "Hello"
STRLEN message
\`\`\`

Output:
\`\`\`
OK
(integer) 5
\`\`\`

### GETSET — Atomic Get-and-Set

Get the old value while setting a new one — useful for swapping:

\`\`\`
SET counter "old"
GETSET counter "new"
GET counter
\`\`\`

Output:
\`\`\`
OK
"old"
"new"
\`\`\`

### SETNX — Set if Not Exists

Set a key only if it does not already exist. Returns 1 if set, 0 if skipped:

\`\`\`
SETNX lock "process-1"
SETNX lock "process-2"
GET lock
\`\`\`

Output:
\`\`\`
(integer) 1
(integer) 0
"process-1"
\`\`\`

\`SETNX\` is the basis for distributed locks in Redis.

### Your Task

Use \`MSET\` to set three keys at once: \`name\` → \`"Redis"\`, \`version\` → \`"7"\`, \`language\` → \`"C"\`. Then use \`MGET\` to retrieve all three.`,

  starterCode: `# Set three keys with MSET
MSET name "Redis" version "7" language "C"

# Get all three with MGET
MGET name version language`,

  solution: `MSET name "Redis" version "7" language "C"
MGET name version language`,

  tests: [
    {
      name: "MGET returns all three values",
      expected: '{"type":"contains","value":"\\"Redis\\""}',
    },
    {
      name: "output has 4 lines (OK + 3 values)",
      expected: '{"type":"lineCount","value":4}',
    },
  ],
};
