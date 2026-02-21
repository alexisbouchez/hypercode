import type { Lesson } from "../../types";

export const expiry: Lesson = {
  id: "expiry",
  title: "Expiry and TTL",
  chapterId: "strings",
  content: `## Key Expiry

One of Redis's killer features is built-in key expiration. You can set any key to automatically disappear after a given time. No cron jobs, no cleanup code — Redis handles it automatically.

### SET with EX (seconds)

Set a key and its expiry in one command:

\`\`\`
SET session "abc123" EX 3600
\`\`\`

This key will expire in 3600 seconds (1 hour). Redis's lazy expiration removes expired keys when they are accessed.

### EXPIRE — Set Expiry After the Fact

Add expiry to an existing key:

\`\`\`
SET cache "some data"
EXPIRE cache 300
\`\`\`

Returns \`(integer) 1\` if the expiry was set, \`(integer) 0\` if the key doesn't exist.

### TTL — Time To Live

Check how many seconds remain before a key expires:

\`\`\`
SET token "xyz" EX 60
TTL token
\`\`\`

Output:
\`\`\`
OK
(integer) 60
\`\`\`

Return values:
- Positive integer: seconds until expiry
- \`-1\`: key exists but has no expiry
- \`-2\`: key does not exist

### PEXPIRE and PTTL — Millisecond Precision

\`\`\`
PEXPIRE key 5000    -- expire in 5000 milliseconds
PTTL key            -- remaining time in milliseconds
\`\`\`

### PERSIST — Remove Expiry

Turn a key with expiry into a permanent key:

\`\`\`
SET temp "value" EX 60
PERSIST temp
TTL temp
\`\`\`

Output:
\`\`\`
OK
(integer) 1
(integer) -1
\`\`\`

### Common Patterns

**Session tokens:**
\`\`\`
SET session:user123 "token_data" EX 86400
\`\`\`

**Cache with TTL:**
\`\`\`
SET cache:query:12345 "result_data" EX 300
\`\`\`

**One-time codes:**
\`\`\`
SET otp:user@example.com "482910" EX 300
\`\`\`

### Your Task

Set a key \`api_key\` to \`"secret123"\` with a 60-second expiry. Then check its TTL.`,

  starterCode: `# Set api_key with 60-second expiry
SET api_key "secret123" EX 60

# Check the TTL
TTL api_key`,

  solution: `SET api_key "secret123" EX 60
TTL api_key`,

  tests: [
    {
      name: "SET returns OK",
      expected: '{"type":"contains","value":"OK"}',
    },
    {
      name: "TTL returns 60",
      expected: '{"type":"lastLine","value":"(integer) 60"}',
    },
  ],
};
