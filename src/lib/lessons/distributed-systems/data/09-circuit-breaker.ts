import type { Lesson } from "../../types";

export const circuitBreaker: Lesson = {
	id: "circuit-breaker",
	title: "Circuit Breaker",
	chapterId: "fault-tolerance",
	content: `## Circuit Breaker

When a downstream service is failing, continuously retrying just makes things worse — it wastes resources and prevents recovery. The **Circuit Breaker** pattern stops sending requests to a failing service and lets it recover.

### The Three States

\`\`\`
  success     too many failures    timeout elapsed
CLOSED  ──────────────────────►  OPEN  ──────────►  HALF-OPEN
  ◄─────────────────────────────────────────────────
               success                    │  failure
                                          ▼
                                         OPEN
\`\`\`

- **CLOSED** — normal operation. Requests pass through. Track failures.
- **OPEN** — service is down. Reject all requests immediately. Start a timeout.
- **HALF-OPEN** — tentatively try one request. If it succeeds, go CLOSED. If it fails, go back OPEN.

\`\`\`js
class CircuitBreaker {
  constructor(threshold, timeout) {
    this.threshold = threshold;  // failures before opening
    this.timeout = timeout;      // ms before trying again
    this.failures = 0;
    this.state = "CLOSED";
    this.openedAt = null;
  }

  call(fn) {
    if (this.state === "OPEN") {
      if (Date.now() - this.openedAt >= this.timeout) {
        this.state = "HALF-OPEN";
      } else {
        throw new Error("Circuit OPEN");
      }
    }
    try {
      const result = fn();
      this.onSuccess();
      return result;
    } catch (err) {
      this.onFailure();
      throw err;
    }
  }

  onSuccess() {
    this.failures = 0;
    this.state = "CLOSED";
  }

  onFailure() {
    this.failures++;
    if (this.failures >= this.threshold) {
      this.state = "OPEN";
      this.openedAt = Date.now();
    }
  }
}
\`\`\`

### Used By

- **Netflix Hystrix** — the original circuit breaker library for microservices.
- **Resilience4j** — modern Java circuit breaker.
- **AWS SDK** — built-in retries with exponential backoff and jitter.
- **Envoy/Istio** — service mesh with circuit breaking at the proxy level.

### Your Task

Implement a \`CircuitBreaker\` class. For testing purposes, use a \`callsAttempted\` counter instead of real time — the circuit opens after \`threshold\` failures and allows a retry after \`timeout\` additional calls.`,

	starterCode: `class CircuitBreaker {
	constructor(threshold, timeout) {
		this.threshold = threshold;   // failures before opening
		this.timeout = timeout;       // calls to wait before trying again
		this.failures = 0;
		this.state = "CLOSED";
		this.openedAtCall = 0;
		this.totalCalls = 0;
	}

	call(fn) {
		this.totalCalls++;

		if (this.state === "OPEN") {
			if (this.totalCalls - this.openedAtCall >= this.timeout) {
				this.state = "HALF-OPEN";
			} else {
				return "REJECTED";
			}
		}

		try {
			const result = fn();
			// On success: reset failures, close circuit
			this.failures = 0;
			this.state = "CLOSED";
			return result;
		} catch (e) {
			// On failure: increment failures, open if threshold reached
			this.failures++;
			if (this.failures >= this.threshold) {
				this.state = "OPEN";
				this.openedAtCall = this.totalCalls;
			}
			return "FAILED";
		}
	}
}

const cb = new CircuitBreaker(3, 5);

// Simulate 3 failures → circuit opens
console.log(cb.call(() => { throw new Error(); })); // FAILED
console.log(cb.call(() => { throw new Error(); })); // FAILED
console.log(cb.call(() => { throw new Error(); })); // FAILED (opens circuit)
console.log(cb.state);                              // OPEN

// Next calls rejected while open
console.log(cb.call(() => "ok"));   // REJECTED
console.log(cb.call(() => "ok"));   // REJECTED
console.log(cb.call(() => "ok"));   // REJECTED
console.log(cb.call(() => "ok"));   // REJECTED
console.log(cb.call(() => "ok"));   // REJECTED (timeout=5, now HALF-OPEN, succeeds)
console.log(cb.state);              // CLOSED
`,

	solution: `class CircuitBreaker {
	constructor(threshold, timeout) {
		this.threshold = threshold;
		this.timeout = timeout;
		this.failures = 0;
		this.state = "CLOSED";
		this.openedAtCall = 0;
		this.totalCalls = 0;
	}

	call(fn) {
		this.totalCalls++;

		if (this.state === "OPEN") {
			if (this.totalCalls - this.openedAtCall >= this.timeout) {
				this.state = "HALF-OPEN";
			} else {
				return "REJECTED";
			}
		}

		try {
			const result = fn();
			this.failures = 0;
			this.state = "CLOSED";
			return result;
		} catch (e) {
			this.failures++;
			if (this.failures >= this.threshold) {
				this.state = "OPEN";
				this.openedAtCall = this.totalCalls;
			}
			return "FAILED";
		}
	}
}

const cb = new CircuitBreaker(3, 5);

console.log(cb.call(() => { throw new Error(); }));
console.log(cb.call(() => { throw new Error(); }));
console.log(cb.call(() => { throw new Error(); }));
console.log(cb.state);

console.log(cb.call(() => "ok"));
console.log(cb.call(() => "ok"));
console.log(cb.call(() => "ok"));
console.log(cb.call(() => "ok"));
console.log(cb.call(() => "ok"));
console.log(cb.state);
`,

	tests: [
		{
			name: "opens after 3 failures, rejects, recovers after 5 calls",
			expected: "FAILED\nFAILED\nFAILED\nOPEN\nREJECTED\nREJECTED\nREJECTED\nREJECTED\nok\nCLOSED\n",
		},
		{
			name: "stays closed on success",
			code: `{{FUNC}}
const cb = new CircuitBreaker(3, 5);
console.log(cb.call(() => "hello"));
console.log(cb.state);`,
			expected: "hello\nCLOSED\n",
		},
		{
			name: "resets failure count on success",
			code: `{{FUNC}}
const cb = new CircuitBreaker(3, 5);
cb.call(() => { throw new Error(); }); // 1 failure
cb.call(() => { throw new Error(); }); // 2 failures
cb.call(() => "ok");                   // success, resets to 0
cb.call(() => { throw new Error(); }); // 1 failure
cb.call(() => { throw new Error(); }); // 2 failures
console.log(cb.state);                 // still CLOSED`,
			expected: "CLOSED\n",
		},
	],
};
