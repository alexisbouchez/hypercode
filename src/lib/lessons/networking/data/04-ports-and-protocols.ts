import type { Lesson } from "../../types";

export const portsAndProtocols: Lesson = {
	id: "ports-and-protocols",
	title: "Ports & Protocols",
	chapterId: "basics",
	content: `## Ports & Protocols

A **port** is a 16-bit number (0–65535) that identifies a specific process or service on a host. Combined with an IP address, a port forms a **socket address** — the endpoint for network communication.

### Port Ranges

| Range | Name | Description |
|-------|------|-------------|
| 0–1023 | Well-known | Assigned to common services (HTTP, SSH, DNS) |
| 1024–49151 | Registered | Used by applications (MySQL, PostgreSQL) |
| 49152–65535 | Dynamic/Ephemeral | Temporary client-side ports |

### Common Ports

| Port | Protocol | Service |
|------|----------|---------|
| 22 | TCP | SSH |
| 53 | UDP/TCP | DNS |
| 80 | TCP | HTTP |
| 443 | TCP | HTTPS |
| 3306 | TCP | MySQL |
| 5432 | TCP | PostgreSQL |
| 6379 | TCP | Redis |
| 8080 | TCP | HTTP Alternate |

### Your Task

Implement two functions:
- \`portInfo(port)\` — returns an object with \`range\` (\`"well-known"\`, \`"registered"\`, or \`"dynamic"\`) and \`service\` (known service name or \`"unknown"\`)
- \`parseSocketAddress(addr)\` — parses a string like \`"192.168.1.1:8080"\` into \`{ ip, port, service, range }\``,

	starterCode: `function portInfo(port) {
	// Your implementation here
}

function parseSocketAddress(addr) {
	// Your implementation here
}

const p1 = portInfo(80);
console.log(p1.range + " " + p1.service);

const p2 = portInfo(5432);
console.log(p2.range + " " + p2.service);

const s = parseSocketAddress("192.168.1.1:443");
console.log(s.ip + ":" + s.port + " " + s.service);
`,

	solution: `function portInfo(port) {
	const services = {
		22: "SSH", 53: "DNS", 80: "HTTP", 443: "HTTPS",
		3306: "MySQL", 5432: "PostgreSQL", 6379: "Redis", 8080: "HTTP-Alt"
	};

	let range;
	if (port >= 0 && port <= 1023) range = "well-known";
	else if (port <= 49151) range = "registered";
	else range = "dynamic";

	return { range, service: services[port] || "unknown" };
}

function parseSocketAddress(addr) {
	const [ip, portStr] = addr.split(":");
	const port = Number(portStr);
	const info = portInfo(port);
	return { ip, port, service: info.service, range: info.range };
}

const p1 = portInfo(80);
console.log(p1.range + " " + p1.service);

const p2 = portInfo(5432);
console.log(p2.range + " " + p2.service);

const s = parseSocketAddress("192.168.1.1:443");
console.log(s.ip + ":" + s.port + " " + s.service);
`,

	tests: [
		{
			name: "identifies well-known and registered ports",
			expected: "well-known HTTP\nregistered PostgreSQL\n192.168.1.1:443 HTTPS\n",
		},
		{
			name: "identifies dynamic port range",
			code: `{{FUNC}}
const p = portInfo(50000);
console.log(p.range + " " + p.service);`,
			expected: "dynamic unknown\n",
		},
		{
			name: "parses socket address with SSH",
			code: `{{FUNC}}
const s = parseSocketAddress("10.0.0.1:22");
console.log(s.ip + " " + s.port + " " + s.service + " " + s.range);`,
			expected: "10.0.0.1 22 SSH well-known\n",
		},
		{
			name: "identifies Redis port",
			code: `{{FUNC}}
const p = portInfo(6379);
console.log(p.range + " " + p.service);`,
			expected: "registered Redis\n",
		},
	],
};
