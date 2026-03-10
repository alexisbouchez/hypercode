import type { Lesson } from "../../types";

export const streams: Lesson = {
  id: "streams",
  title: "Streams",
  chapterId: "advanced",
  content: `## Redis Streams

Streams are an append-only log data structure, introduced in Redis 5.0. They are similar to Apache Kafka topics but built into Redis. Each entry in a stream has a unique ID and consists of field-value pairs.

### Why Streams?

- **Event sourcing**: Record every event (order placed, user clicked, sensor reading)
- **Message queues**: Producers add messages, consumers read them
- **Activity feeds**: Append events and read recent history
- **Audit logs**: Immutable, ordered record of changes

### XADD — Append an Entry

\`\`\`
XADD mystream * name "Alice" action "login"
XADD mystream * name "Bob" action "purchase" item "book"
\`\`\`

The \`*\` tells Redis to auto-generate a unique ID (timestamp-based). Each entry has field-value pairs, like a mini hash.

### XLEN — Count Entries

\`\`\`
XLEN mystream    -- (integer) 2
\`\`\`

### XRANGE — Read Entries (Oldest First)

\`\`\`
XRANGE mystream - +
\`\`\`

Use \`-\` for the minimum ID and \`+\` for the maximum. This returns all entries in chronological order.

### XREVRANGE — Read Entries (Newest First)

\`\`\`
XREVRANGE mystream + -
\`\`\`

### XREAD — Read From One or More Streams

\`\`\`
XREAD COUNT 2 STREAMS mystream 0
\`\`\`

\`XREAD\` reads entries after the given ID. Use \`0\` to start from the beginning. The \`COUNT\` option limits how many entries are returned.

### Stream Entry IDs

Every stream entry has an ID in the format \`<millisecondsTime>-<sequenceNumber>\`, e.g. \`1526919030474-0\`. When you use \`*\`, Redis generates this automatically. You can also specify your own IDs if they are greater than the last entry.

### XTRIM — Limit Stream Length

\`\`\`
XTRIM mystream MAXLEN 1000
\`\`\`

Trims the stream to the most recent 1000 entries, removing the oldest.

### Real-World: Event Log

\`\`\`
# Log user events
XADD events:user:42 * type "page_view" url "/products"
XADD events:user:42 * type "add_to_cart" product "laptop"
XADD events:user:42 * type "checkout" total "999"

# Read all events for this user
XRANGE events:user:42 - +
XLEN events:user:42
\`\`\`

### Pub/Sub vs Streams

Redis also has a **Pub/Sub** system (\`PUBLISH\`/\`SUBSCRIBE\`) for real-time messaging. The key differences:

| Feature | Pub/Sub | Streams |
|---------|---------|---------|
| Persistence | No (fire-and-forget) | Yes (stored on disk) |
| History | No (miss messages if offline) | Yes (read past entries) |
| Consumer groups | No | Yes |
| Delivery guarantee | At-most-once | At-least-once |

Pub/Sub is ideal for real-time notifications where missing a message is acceptable. Streams are better when you need reliability, persistence, and the ability to replay history.

### Your Task

Build an event log for a web application:
1. XADD three events to \`events\`:
   - \`type "signup" user "alice"\`
   - \`type "login" user "bob"\`
   - \`type "purchase" user "alice" amount "50"\`
2. Use XLEN to count the entries
3. Use XRANGE to read all entries`,

  starterCode: `# Add events to the stream
XADD events * type "signup" user "alice"
XADD events * type "login" user "bob"
XADD events * type "purchase" user "alice" amount "50"

# Count events
XLEN events

# Read all events
XRANGE events - +`,

  solution: `XADD events * type "signup" user "alice"
XADD events * type "login" user "bob"
XADD events * type "purchase" user "alice" amount "50"
XLEN events
XRANGE events - +`,

  tests: [
    {
      name: "stream has 3 entries",
      expected: '{"type":"contains","value":"(integer) 3"}',
    },
    {
      name: "signup event recorded",
      expected: '{"type":"contains","value":"\\"signup\\""}',
    },
    {
      name: "purchase event has amount",
      expected: '{"type":"contains","value":"\\"50\\""}',
    },
  ],
};
