import type { Lesson } from "../../types";

export const caching: Lesson = {
  id: "caching",
  title: "Caching Patterns",
  chapterId: "patterns",
  content: `## Redis as a Cache

Caching is Redis's most common use case. By storing frequently-accessed data in Redis, you avoid repeated expensive database queries or API calls.

### Cache-Aside Pattern

The most common caching pattern:

1. Check Redis for the data
2. If found (cache hit), return it
3. If not found (cache miss), fetch from the database, store in Redis, return it

\`\`\`
# Cache hit scenario:
SET cache:user:1 '{"name":"Alice","email":"a@b.com"}' EX 300
GET cache:user:1    -- returns cached JSON

# Cache miss scenario:
GET cache:user:99   -- (nil) → fetch from DB, then cache
\`\`\`

### SET with NX and XX Flags

\`SET key value NX\` — only set if key does **not** exist (like SETNX):
\`\`\`
SET lock "worker-1" NX EX 30    -- acquire lock, expire in 30s
SET lock "worker-2" NX EX 30    -- fails if lock exists
GET lock                         -- "worker-1"
\`\`\`

\`SET key value XX\` — only set if key **exists** (update only):
\`\`\`
SET user:1 "Alice"
SET user:1 "Alice Updated" XX    -- updates existing key
SET user:99 "Bob" XX             -- (nil) — key doesn't exist
\`\`\`

### GETDEL — Get and Delete Atomically

Perfect for one-time use tokens:
\`\`\`
SET otp:user123 "482910" EX 300
GETDEL otp:user123    -- "482910" (and deletes the key)
GETDEL otp:user123    -- (nil)
\`\`\`

### Distributed Lock Pattern

\`\`\`
# Acquire: SET key value NX EX timeout
SET lock:resource "process-1" NX EX 30

# If successful ((integer) 1 returned or "OK"), we have the lock
# Do work...

# Release: delete the key (check it's still ours first!)
GET lock:resource    -- verify it's "process-1"
DEL lock:resource
\`\`\`

### Cache Warming

Pre-load frequently accessed data on startup:
\`\`\`
MSET
  cache:config:maxitems "100"
  cache:config:timeout "30"
  cache:config:version "2"
\`\`\`

### Write-Through Cache

Update both the cache and database simultaneously:
\`\`\`
SET user:1 "updated data" EX 3600
# Also write to database (in application code)
\`\`\`

### Your Task

Implement a simple lock using SET NX EX:
1. Set \`lock:task\` to \`"worker-1"\` with NX and EX 60 (acquires lock)
2. Try to set \`lock:task\` to \`"worker-2"\` with NX (should fail)
3. GET \`lock:task\` to confirm worker-1 holds the lock`,

  starterCode: `# Acquire lock as worker-1
SET lock:task "worker-1" NX EX 60

# Try to acquire as worker-2 (should fail)
SET lock:task "worker-2" NX EX 60

# Confirm who holds the lock
GET lock:task`,

  solution: `SET lock:task "worker-1" NX EX 60
SET lock:task "worker-2" NX EX 60
GET lock:task`,

  tests: [
    {
      name: "first SET returns OK (lock acquired)",
      expected: '{"type":"contains","value":"OK"}',
    },
    {
      name: "GET shows worker-1 holds the lock",
      expected: '{"type":"lastLine","value":"\\"worker-1\\""}',
    },
  ],
};
