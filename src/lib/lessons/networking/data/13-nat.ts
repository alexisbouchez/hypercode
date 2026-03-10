import type { Lesson } from "../../types";

export const nat: Lesson = {
	id: "nat",
	title: "Network Address Translation",
	chapterId: "advanced",
	content: `## Network Address Translation (NAT)

**NAT** allows multiple devices on a private network to share a single public IP address. A NAT router translates between private and public addresses.

### How NAT Works

\`\`\`
Private Network          NAT Router           Internet
                    ┌──────────────────┐
192.168.1.10:5000 ──>│ 203.0.113.1:40001│──> Server
192.168.1.11:5001 ──>│ 203.0.113.1:40002│──> Server
192.168.1.12:5000 ──>│ 203.0.113.1:40003│──> Server
                    └──────────────────┘
\`\`\`

### NAT Translation Table

The router maintains a translation table mapping internal (private IP:port) to external (public IP:port):

| Internal | External | Destination |
|----------|----------|-------------|
| 192.168.1.10:5000 | 203.0.113.1:40001 | 8.8.8.8:53 |
| 192.168.1.11:5001 | 203.0.113.1:40002 | 8.8.8.8:53 |

### Types of NAT

- **Static NAT**: One-to-one mapping (fixed)
- **Dynamic NAT**: Pool of public IPs assigned on demand
- **PAT (Port Address Translation)**: Many-to-one using different ports

### Your Task

Implement a \`NATRouter\` class that simulates PAT:
- \`constructor(publicIP)\` — initialize with a public IP
- \`translateOutbound(privateIP, privatePort, destIP, destPort)\` — creates a mapping, returns the external address
- \`translateInbound(publicPort)\` — looks up the internal address from a public port
- \`getTable()\` — returns all current mappings as an array`,

	starterCode: `class NATRouter {
	constructor(publicIP) {
		this.publicIP = publicIP;
		this.table = {};
		this.nextPort = 40001;
	}

	translateOutbound(privateIP, privatePort, destIP, destPort) {
		// Your implementation here
	}

	translateInbound(publicPort) {
		// Your implementation here
	}

	getTable() {
		// Your implementation here
	}
}

const nat = new NATRouter("203.0.113.1");

const ext1 = nat.translateOutbound("192.168.1.10", 5000, "8.8.8.8", 53);
console.log("Out: " + ext1.ip + ":" + ext1.port);

const ext2 = nat.translateOutbound("192.168.1.11", 6000, "8.8.8.8", 53);
console.log("Out: " + ext2.ip + ":" + ext2.port);

const internal = nat.translateInbound(40001);
console.log("In: " + internal.ip + ":" + internal.port);

console.log("Entries: " + nat.getTable().length);
`,

	solution: `class NATRouter {
	constructor(publicIP) {
		this.publicIP = publicIP;
		this.table = {};
		this.nextPort = 40001;
	}

	translateOutbound(privateIP, privatePort, destIP, destPort) {
		const internalKey = privateIP + ":" + privatePort;
		if (this.table[internalKey]) {
			return { ip: this.publicIP, port: this.table[internalKey].externalPort };
		}
		const externalPort = this.nextPort++;
		this.table[internalKey] = {
			privateIP, privatePort, externalPort, destIP, destPort
		};
		return { ip: this.publicIP, port: externalPort };
	}

	translateInbound(publicPort) {
		for (const entry of Object.values(this.table)) {
			if (entry.externalPort === publicPort) {
				return { ip: entry.privateIP, port: entry.privatePort };
			}
		}
		return null;
	}

	getTable() {
		return Object.values(this.table).map(e => ({
			internal: e.privateIP + ":" + e.privatePort,
			external: this.publicIP + ":" + e.externalPort,
			destination: e.destIP + ":" + e.destPort,
		}));
	}
}

const nat = new NATRouter("203.0.113.1");

const ext1 = nat.translateOutbound("192.168.1.10", 5000, "8.8.8.8", 53);
console.log("Out: " + ext1.ip + ":" + ext1.port);

const ext2 = nat.translateOutbound("192.168.1.11", 6000, "8.8.8.8", 53);
console.log("Out: " + ext2.ip + ":" + ext2.port);

const internal = nat.translateInbound(40001);
console.log("In: " + internal.ip + ":" + internal.port);

console.log("Entries: " + nat.getTable().length);
`,

	tests: [
		{
			name: "translates outbound and inbound",
			expected: "Out: 203.0.113.1:40001\nOut: 203.0.113.1:40002\nIn: 192.168.1.10:5000\nEntries: 2\n",
		},
		{
			name: "reuses existing mapping for same source",
			code: `{{FUNC}}
const nat = new NATRouter("1.2.3.4");
const e1 = nat.translateOutbound("10.0.0.1", 80, "8.8.8.8", 53);
const e2 = nat.translateOutbound("10.0.0.1", 80, "8.8.8.8", 53);
console.log(e1.port === e2.port);`,
			expected: "true\n",
		},
		{
			name: "returns null for unknown inbound port",
			code: `{{FUNC}}
const nat = new NATRouter("1.2.3.4");
console.log(nat.translateInbound(99999));`,
			expected: "null\n",
		},
		{
			name: "table contains correct mappings",
			code: `{{FUNC}}
const nat = new NATRouter("5.5.5.5");
nat.translateOutbound("10.0.0.1", 100, "8.8.8.8", 53);
const t = nat.getTable();
console.log(t[0].internal);
console.log(t[0].destination);`,
			expected: "10.0.0.1:100\n8.8.8.8:53\n",
		},
	],
};
