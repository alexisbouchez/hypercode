import type { Lesson } from "../../types";

export const transactions: Lesson = {
  id: "transactions",
  title: "Transactions",
  chapterId: "patterns",
  content: `## MULTI/EXEC — Transactions

Redis transactions let you queue a series of commands and execute them atomically. No other client can interleave commands between the queued operations.

### MULTI and EXEC

\`\`\`
MULTI
SET account:1 1000
SET account:2 500
EXEC
\`\`\`

Output:
\`\`\`
OK
QUEUED
QUEUED
1) OK
2) OK
\`\`\`

- \`MULTI\` — begin the transaction
- Each command responds with \`QUEUED\` (not yet executed)
- \`EXEC\` — execute all queued commands atomically; returns an array of results

### DISCARD — Cancel the Transaction

\`\`\`
MULTI
SET temp "value"
DISCARD
GET temp
\`\`\`

Output:
\`\`\`
OK
QUEUED
OK
(nil)
\`\`\`

\`DISCARD\` cancels the transaction and discards all queued commands.

### Atomicity in Redis

Redis transactions guarantee that:
1. All commands in a \`MULTI/EXEC\` block run sequentially
2. No other client's commands can interleave
3. Either all commands execute or none do (after \`EXEC\`)

> **Note:** Redis does not roll back on command errors within a transaction. If a command fails (e.g., INCR on a string), other commands still execute. Use \`DISCARD\` to explicitly cancel.

### Transfer Example

Atomically transfer 100 from account A to account B:

\`\`\`
SET balance:alice 1000
SET balance:bob 500

MULTI
DECRBY balance:alice 100
INCRBY balance:bob 100
EXEC

GET balance:alice
GET balance:bob
\`\`\`

Output:
\`\`\`
OK
OK
OK
QUEUED
QUEUED
1) (integer) 900
2) (integer) 600
"900"
"600"
\`\`\`

### WATCH — Optimistic Locking

\`WATCH key [key ...]\` monitors keys for changes. If a watched key is modified before \`EXEC\`, the entire transaction is aborted (EXEC returns nil):

\`\`\`
WATCH balance:alice
MULTI
DECRBY balance:alice 100
EXEC
\`\`\`

This is useful for compare-and-swap operations without locking.

### Your Task

Use MULTI/EXEC to atomically:
1. SET \`inventory\` to \`100\`
2. DECRBY \`inventory\` by \`10\`

Then GET the final value.`,

  starterCode: `# Begin transaction
MULTI
SET inventory 100
DECRBY inventory 10
EXEC

# Check the result
GET inventory`,

  solution: `MULTI
SET inventory 100
DECRBY inventory 10
EXEC
GET inventory`,

  tests: [
    {
      name: "EXEC output contains QUEUED results",
      expected: '{"type":"contains","value":"QUEUED"}',
    },
    {
      name: "final GET returns 90",
      expected: '{"type":"lastLine","value":"\\"90\\""}',
    },
  ],
};
