import type { Lesson } from "../../types";

export const rateLimiter: Lesson = {
	id: "rate-limiter",
	title: "Rate Limiter (Token Bucket)",
	chapterId: "fault-tolerance",
	content: `## Rate Limiting: Token Bucket

Rate limiting controls how many requests a client can make in a given period. It protects services from being overwhelmed by traffic spikes, abuse, or runaway clients.

### The Token Bucket Algorithm

Imagine a bucket that holds tokens. Tokens are added at a fixed **refill rate**. Each request consumes one token. If the bucket is empty, the request is rejected.

- **Burst tolerance:** the bucket can hold up to \`capacity\` tokens, so short bursts above the refill rate are allowed.
- **Sustained rate:** over time, the average rate cannot exceed the refill rate.

\`\`\`js
class TokenBucket {
  constructor(capacity, refillRate) {
    this.capacity = capacity;   // max tokens
    this.tokens = capacity;     // start full
    this.refillRate = refillRate; // tokens per second
    this.lastRefill = Date.now();
  }

  refill() {
    const now = Date.now();
    const elapsed = (now - this.lastRefill) / 1000; // seconds
    this.tokens = Math.min(this.capacity, this.tokens + elapsed * this.refillRate);
    this.lastRefill = now;
  }

  consume(count = 1) {
    this.refill();
    if (this.tokens >= count) {
      this.tokens -= count;
      return true;  // allowed
    }
    return false;   // rejected
  }
}
\`\`\`

Since we are testing without real time, we will use a simulated version with an explicit \`addTokens\` method:

\`\`\`js
class TokenBucket {
  constructor(capacity) {
    this.capacity = capacity;
    this.tokens = capacity;
  }

  addTokens(n) {
    this.tokens = Math.min(this.capacity, this.tokens + n);
  }

  consume() {
    if (this.tokens >= 1) {
      this.tokens--;
      return true;
    }
    return false;
  }
}
\`\`\`

### Other Rate Limiting Algorithms

- **Fixed Window Counter** — count requests in fixed time windows (e.g., 100 req/min).
- **Sliding Window Log** — track exact timestamps of requests in a rolling window.
- **Leaky Bucket** — requests drain at a constant rate; excess is queued or dropped.

### Used By

- **Stripe, Twilio, GitHub APIs** — rate limit by API key.
- **Nginx** — \`limit_req\` directive implements a leaky bucket.
- **AWS API Gateway** — token bucket per endpoint.
- **Redis** — often used as the backing store for distributed rate limiters.

### Your Task

Implement a \`TokenBucket\` class with a \`capacity\`, \`addTokens(n)\`, and \`consume()\` method.`,

	starterCode: `class TokenBucket {
	constructor(capacity) {
		this.capacity = capacity;
		this.tokens = capacity; // start full
	}

	addTokens(n) {
		// Add n tokens, but do not exceed capacity
	}

	consume() {
		// If tokens >= 1, decrement and return true
		// Otherwise return false
	}
}

const bucket = new TokenBucket(3);
console.log(bucket.consume()); // true
console.log(bucket.consume()); // true
console.log(bucket.consume()); // true
console.log(bucket.consume()); // false (empty)
bucket.addTokens(2);
console.log(bucket.consume()); // true
console.log(bucket.consume()); // true
console.log(bucket.consume()); // false (empty again)
`,

	solution: `class TokenBucket {
	constructor(capacity) {
		this.capacity = capacity;
		this.tokens = capacity;
	}

	addTokens(n) {
		this.tokens = Math.min(this.capacity, this.tokens + n);
	}

	consume() {
		if (this.tokens >= 1) {
			this.tokens--;
			return true;
		}
		return false;
	}
}

const bucket = new TokenBucket(3);
console.log(bucket.consume());
console.log(bucket.consume());
console.log(bucket.consume());
console.log(bucket.consume());
bucket.addTokens(2);
console.log(bucket.consume());
console.log(bucket.consume());
console.log(bucket.consume());
`,

	tests: [
		{
			name: "3 consumes succeed, 4th fails, refill 2, 2 succeed, 3rd fails",
			expected: "true\ntrue\ntrue\nfalse\ntrue\ntrue\nfalse\n",
		},
		{
			name: "addTokens respects capacity",
			code: `{{FUNC}}
const b = new TokenBucket(5);
b.consume(); b.consume(); b.consume(); // tokens = 2
b.addTokens(10); // capped at 5
console.log(b.tokens);`,
			expected: "5\n",
		},
		{
			name: "empty bucket rejects",
			code: `{{FUNC}}
const b = new TokenBucket(1);
b.consume();
console.log(b.consume());`,
			expected: "false\n",
		},
		{
			name: "addTokens then consume",
			code: `{{FUNC}}
const b = new TokenBucket(10);
for (let i = 0; i < 10; i++) b.consume(); // drain
b.addTokens(3);
console.log(b.consume());
console.log(b.consume());
console.log(b.consume());
console.log(b.consume());`,
			expected: "true\ntrue\ntrue\nfalse\n",
		},
	],
};
