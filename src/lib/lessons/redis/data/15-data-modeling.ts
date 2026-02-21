import type { Lesson } from "../../types";

export const dataModeling: Lesson = {
  id: "data-modeling",
  title: "Data Modeling",
  chapterId: "advanced",
  content: `## Data Modeling in Redis

Redis is not relational. Data is stored as flat key-value pairs, and there are no JOINs. This requires thinking differently about your data model — choosing the right data structure and key naming strategy.

### Key Naming Conventions

Use colons to separate namespaces in key names:

\`\`\`
user:1              -- a single user (string or hash)
user:1:posts        -- a set or list of post IDs for user 1
post:42             -- a single post hash
post:42:comments    -- a list of comment IDs
index:posts:by_tag:redis  -- sorted set of posts tagged "redis"
\`\`\`

This creates a consistent, readable key structure.

### Storing Objects

**Option 1: Hash** (recommended for objects with many fields)
\`\`\`
HSET user:1 name "Alice" email "alice@example.com" role "admin"
HGET user:1 name
HGETALL user:1
\`\`\`

**Option 2: JSON string** (when you always need the whole object)
\`\`\`
SET user:2 '{"name":"Bob","email":"bob@example.com"}'
GET user:2
\`\`\`

### One-to-Many Relationships

User → Posts: store post IDs in a set or sorted set:
\`\`\`
HSET post:1 title "Hello Redis" author "alice" views "0"
HSET post:2 title "Redis Patterns" author "alice" views "0"
SADD user:1:posts "post:1" "post:2"

# Get Alice's posts
SMEMBERS user:1:posts
\`\`\`

### Tags and Inverted Indexes

\`\`\`
# Tag posts by topic
SADD tag:redis "post:1" "post:2"
SADD tag:database "post:2" "post:3"

# Find posts tagged with both redis AND database
SINTER tag:redis tag:database
\`\`\`

### Rate Limiting Pattern

\`\`\`
# Key = rate:userId:windowStart (minute-based)
SET rate:user123:api 0
INCR rate:user123:api
EXPIRE rate:user123:api 60

# Check before processing:
# If GET rate:user123:api > 100, reject
\`\`\`

### Session Storage

\`\`\`
HSET session:abc123 user_id "42" role "admin" csrf_token "xyz"
EXPIRE session:abc123 3600
HGET session:abc123 user_id
\`\`\`

### Analytics with Sorted Sets

\`\`\`
# Track product popularity (view count as score)
ZINCRBY trending:products 1 "product:laptop"
ZINCRBY trending:products 1 "product:laptop"
ZINCRBY trending:products 1 "product:phone"

# Get most popular
ZREVRANGE trending:products 0 4 WITHSCORES
\`\`\`

### Your Task

Model a simple blog:
1. HSET \`post:1\` with fields: \`title\` → \`"Redis Guide"\`, \`author\` → \`"Alice"\`, \`views\` → \`"0"\`
2. SADD \`tag:redis\` with member \`"post:1"\`
3. HINCRBY \`post:1\` \`views\` by 5
4. HGET \`post:1\` \`views\` to see the updated view count`,

  starterCode: `# Create blog post
HSET post:1 title "Redis Guide" author "Alice" views "0"

# Tag it
SADD tag:redis "post:1"

# Simulate 5 views
HINCRBY post:1 views 5

# Check view count
HGET post:1 views`,

  solution: `HSET post:1 title "Redis Guide" author "Alice" views "0"
SADD tag:redis "post:1"
HINCRBY post:1 views 5
HGET post:1 views`,

  tests: [
    {
      name: "HGET views returns 5",
      expected: '{"type":"lastLine","value":"\\"5\\""}',
    },
    {
      name: "SADD returns 1",
      expected: '{"type":"contains","value":"(integer) 1"}',
    },
  ],
};
