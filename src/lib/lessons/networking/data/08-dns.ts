import type { Lesson } from "../../types";

export const dns: Lesson = {
	id: "dns",
	title: "DNS Resolution",
	chapterId: "protocols",
	content: `## DNS Resolution

The **Domain Name System (DNS)** translates human-readable domain names into IP addresses. When you visit \`example.com\`, DNS resolves it to an IP like \`93.184.216.34\`.

### DNS Hierarchy

\`\`\`
                  . (root)
                 / \\
              .com  .org
              /       \\
         example     mozilla
           |           |
          www         www
\`\`\`

### Record Types

| Type | Purpose | Example |
|------|---------|---------|
| A | IPv4 address | \`example.com -> 93.184.216.34\` |
| AAAA | IPv6 address | \`example.com -> 2606:2800:...\` |
| CNAME | Alias | \`www.example.com -> example.com\` |
| MX | Mail server | \`example.com -> mail.example.com\` |
| NS | Name server | \`example.com -> ns1.example.com\` |

### Resolution Process

1. Client checks local cache
2. Query recursive resolver
3. Resolver queries root server -> TLD server -> authoritative server
4. Response cached and returned

### Your Task

Implement a \`DNSResolver\` class that simulates DNS resolution:
- \`addRecord(domain, type, value)\` — adds a DNS record
- \`resolve(domain, type)\` — resolves a domain, following CNAME chains
- \`addToCache(domain, type, value, ttl)\` — adds to cache with TTL
- \`resolveWithCache(domain, type, currentTime)\` — resolves using cache (returns null if expired)`,

	starterCode: `class DNSResolver {
	constructor() {
		this.records = {};
		this.cache = {};
	}

	addRecord(domain, type, value) {
		// Your implementation here
	}

	resolve(domain, type) {
		// Your implementation here (follow CNAME chains)
	}

	addToCache(domain, type, value, ttl) {
		// Your implementation here
	}

	resolveWithCache(domain, type, currentTime) {
		// Your implementation here
	}
}

const dns = new DNSResolver();
dns.addRecord("example.com", "A", "93.184.216.34");
dns.addRecord("www.example.com", "CNAME", "example.com");
dns.addRecord("example.com", "MX", "mail.example.com");

console.log(dns.resolve("example.com", "A"));
console.log(dns.resolve("www.example.com", "A"));
console.log(dns.resolve("example.com", "MX"));
`,

	solution: `class DNSResolver {
	constructor() {
		this.records = {};
		this.cache = {};
	}

	addRecord(domain, type, value) {
		const key = domain + ":" + type;
		this.records[key] = value;
	}

	resolve(domain, type) {
		const key = domain + ":" + type;
		if (this.records[key]) return this.records[key];

		const cnameKey = domain + ":CNAME";
		if (this.records[cnameKey]) {
			return this.resolve(this.records[cnameKey], type);
		}

		return null;
	}

	addToCache(domain, type, value, ttl) {
		const key = domain + ":" + type;
		this.cache[key] = { value, expiry: ttl };
	}

	resolveWithCache(domain, type, currentTime) {
		const key = domain + ":" + type;
		const entry = this.cache[key];
		if (entry && currentTime < entry.expiry) return entry.value;
		return this.resolve(domain, type);
	}
}

const dns = new DNSResolver();
dns.addRecord("example.com", "A", "93.184.216.34");
dns.addRecord("www.example.com", "CNAME", "example.com");
dns.addRecord("example.com", "MX", "mail.example.com");

console.log(dns.resolve("example.com", "A"));
console.log(dns.resolve("www.example.com", "A"));
console.log(dns.resolve("example.com", "MX"));
`,

	tests: [
		{
			name: "resolves direct and CNAME records",
			expected: "93.184.216.34\n93.184.216.34\nmail.example.com\n",
		},
		{
			name: "returns null for unknown domain",
			code: `{{FUNC}}
const dns = new DNSResolver();
console.log(dns.resolve("unknown.com", "A"));`,
			expected: "null\n",
		},
		{
			name: "cache returns value before expiry",
			code: `{{FUNC}}
const dns = new DNSResolver();
dns.addToCache("cached.com", "A", "1.2.3.4", 100);
console.log(dns.resolveWithCache("cached.com", "A", 50));
console.log(dns.resolveWithCache("cached.com", "A", 100));`,
			expected: "1.2.3.4\nnull\n",
		},
		{
			name: "follows multi-level CNAME chain",
			code: `{{FUNC}}
const dns = new DNSResolver();
dns.addRecord("a.com", "CNAME", "b.com");
dns.addRecord("b.com", "CNAME", "c.com");
dns.addRecord("c.com", "A", "10.0.0.1");
console.log(dns.resolve("a.com", "A"));`,
			expected: "10.0.0.1\n",
		},
	],
};
