import type { Lesson } from "../../types";

export const ipAddresses: Lesson = {
	id: "ip-addresses",
	title: "IP Addresses",
	chapterId: "basics",
	content: `## IP Addresses

An **IP address** (Internet Protocol address) is a unique numerical label assigned to each device on a network. IPv4 addresses consist of four **octets** — numbers from 0 to 255 — separated by dots.

### Structure

\`\`\`
192.168.1.1
 │    │  │ │
 │    │  │ └─ Host (octet 4)
 │    │  └─── Host (octet 3)
 │    └────── Network (octet 2)
 └─────────── Network (octet 1)
\`\`\`

Each octet is an 8-bit number (0–255), so an IPv4 address is 32 bits total.

### Special Addresses

| Address | Purpose |
|---------|---------|
| \`127.0.0.1\` | Loopback (localhost) |
| \`0.0.0.0\` | Unspecified / default route |
| \`255.255.255.255\` | Broadcast |
| \`10.x.x.x\` | Private (Class A) |
| \`192.168.x.x\` | Private (Class C) |

### Validation

A valid IPv4 address must:
1. Have exactly 4 octets separated by dots
2. Each octet must be a number between 0 and 255
3. No leading zeros (e.g., \`01.02.03.04\` is invalid)

### Your Task

Implement \`parseIPv4(str)\` that takes a string and returns an object with:
- \`valid\`: boolean indicating if the IP is valid
- \`octets\`: array of 4 numbers (or empty array if invalid)
- \`type\`: one of \`"loopback"\`, \`"private"\`, \`"broadcast"\`, or \`"public"\` (or \`"invalid"\` if not valid)`,

	starterCode: `function parseIPv4(str) {
	// Your implementation here
}

const r1 = parseIPv4("192.168.1.1");
console.log(r1.valid + " " + r1.octets.join(".") + " " + r1.type);

const r2 = parseIPv4("127.0.0.1");
console.log(r2.valid + " " + r2.octets.join(".") + " " + r2.type);

const r3 = parseIPv4("256.1.1.1");
console.log(r3.valid + " " + r3.type);
`,

	solution: `function parseIPv4(str) {
	const parts = str.split(".");
	if (parts.length !== 4) return { valid: false, octets: [], type: "invalid" };

	const octets = [];
	for (const p of parts) {
		if (!/^\\d+$/.test(p)) return { valid: false, octets: [], type: "invalid" };
		if (p.length > 1 && p[0] === "0") return { valid: false, octets: [], type: "invalid" };
		const n = Number(p);
		if (n < 0 || n > 255) return { valid: false, octets: [], type: "invalid" };
		octets.push(n);
	}

	let type = "public";
	if (octets[0] === 127) type = "loopback";
	else if (octets[0] === 10) type = "private";
	else if (octets[0] === 172 && octets[1] >= 16 && octets[1] <= 31) type = "private";
	else if (octets[0] === 192 && octets[1] === 168) type = "private";
	else if (octets.every(o => o === 255)) type = "broadcast";

	return { valid: true, octets, type };
}

const r1 = parseIPv4("192.168.1.1");
console.log(r1.valid + " " + r1.octets.join(".") + " " + r1.type);

const r2 = parseIPv4("127.0.0.1");
console.log(r2.valid + " " + r2.octets.join(".") + " " + r2.type);

const r3 = parseIPv4("256.1.1.1");
console.log(r3.valid + " " + r3.type);
`,

	tests: [
		{
			name: "parses valid IPs and detects types",
			expected: "true 192.168.1.1 private\ntrue 127.0.0.1 loopback\nfalse invalid\n",
		},
		{
			name: "detects broadcast address",
			code: `{{FUNC}}
const r = parseIPv4("255.255.255.255");
console.log(r.valid + " " + r.type);`,
			expected: "true broadcast\n",
		},
		{
			name: "rejects leading zeros",
			code: `{{FUNC}}
const r = parseIPv4("01.02.03.04");
console.log(r.valid + " " + r.type);`,
			expected: "false invalid\n",
		},
		{
			name: "detects public address",
			code: `{{FUNC}}
const r = parseIPv4("8.8.8.8");
console.log(r.valid + " " + r.type);`,
			expected: "true public\n",
		},
	],
};
