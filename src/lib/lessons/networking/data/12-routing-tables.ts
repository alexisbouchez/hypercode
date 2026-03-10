import type { Lesson } from "../../types";

export const routingTables: Lesson = {
	id: "routing-tables",
	title: "Routing Tables",
	chapterId: "advanced",
	content: `## Routing Tables

A **routing table** determines where network packets should be forwarded. Each entry maps a destination network to a next hop (gateway) and interface.

### Routing Table Structure

\`\`\`
Destination      Netmask           Gateway         Interface
192.168.1.0      255.255.255.0     0.0.0.0         eth0
10.0.0.0         255.0.0.0         192.168.1.1     eth0
0.0.0.0          0.0.0.0           192.168.1.1     eth0    (default)
\`\`\`

### Longest Prefix Match

When multiple routes match a destination, the router uses the **longest prefix match** — the route with the most specific (longest) subnet mask wins.

For example, if a packet is destined for \`10.1.2.3\`:
- \`10.0.0.0/8\` matches (prefix length 8)
- \`10.1.0.0/16\` matches (prefix length 16) -- this wins
- \`0.0.0.0/0\` matches (prefix length 0)

### Your Task

Implement a \`RoutingTable\` class:
- \`addRoute(network, prefix, gateway, iface)\` — adds a route (e.g., \`"192.168.1.0"\`, \`24\`, \`"0.0.0.0"\`, \`"eth0"\`)
- \`lookup(destIP)\` — finds the best matching route using longest prefix match, returns \`{ gateway, iface }\` or null`,

	starterCode: `class RoutingTable {
	constructor() {
		this.routes = [];
	}

	addRoute(network, prefix, gateway, iface) {
		// Your implementation here
	}

	lookup(destIP) {
		// Your implementation here (longest prefix match)
	}
}

const rt = new RoutingTable();
rt.addRoute("192.168.1.0", 24, "0.0.0.0", "eth0");
rt.addRoute("10.0.0.0", 8, "192.168.1.1", "eth0");
rt.addRoute("10.1.0.0", 16, "192.168.1.2", "eth1");
rt.addRoute("0.0.0.0", 0, "192.168.1.1", "eth0");

const r1 = rt.lookup("192.168.1.50");
console.log(r1.gateway + " " + r1.iface);

const r2 = rt.lookup("10.1.2.3");
console.log(r2.gateway + " " + r2.iface);

const r3 = rt.lookup("8.8.8.8");
console.log(r3.gateway + " " + r3.iface);
`,

	solution: `class RoutingTable {
	constructor() {
		this.routes = [];
	}

	addRoute(network, prefix, gateway, iface) {
		const octets = network.split(".").map(Number);
		const networkInt = (octets[0] << 24 | octets[1] << 16 | octets[2] << 8 | octets[3]) >>> 0;
		const mask = prefix === 0 ? 0 : (0xFFFFFFFF << (32 - prefix)) >>> 0;
		this.routes.push({ networkInt, mask, prefix, gateway, iface });
	}

	lookup(destIP) {
		const octets = destIP.split(".").map(Number);
		const destInt = (octets[0] << 24 | octets[1] << 16 | octets[2] << 8 | octets[3]) >>> 0;

		let bestMatch = null;
		let bestPrefix = -1;

		for (const route of this.routes) {
			if ((destInt & route.mask) >>> 0 === route.networkInt) {
				if (route.prefix > bestPrefix) {
					bestPrefix = route.prefix;
					bestMatch = { gateway: route.gateway, iface: route.iface };
				}
			}
		}

		return bestMatch;
	}
}

const rt = new RoutingTable();
rt.addRoute("192.168.1.0", 24, "0.0.0.0", "eth0");
rt.addRoute("10.0.0.0", 8, "192.168.1.1", "eth0");
rt.addRoute("10.1.0.0", 16, "192.168.1.2", "eth1");
rt.addRoute("0.0.0.0", 0, "192.168.1.1", "eth0");

const r1 = rt.lookup("192.168.1.50");
console.log(r1.gateway + " " + r1.iface);

const r2 = rt.lookup("10.1.2.3");
console.log(r2.gateway + " " + r2.iface);

const r3 = rt.lookup("8.8.8.8");
console.log(r3.gateway + " " + r3.iface);
`,

	tests: [
		{
			name: "performs longest prefix match routing",
			expected: "0.0.0.0 eth0\n192.168.1.2 eth1\n192.168.1.1 eth0\n",
		},
		{
			name: "matches /8 route over default",
			code: `{{FUNC}}
const rt = new RoutingTable();
rt.addRoute("10.0.0.0", 8, "gw1", "eth0");
rt.addRoute("0.0.0.0", 0, "gw0", "eth0");
const r = rt.lookup("10.255.255.255");
console.log(r.gateway);`,
			expected: "gw1\n",
		},
		{
			name: "returns null for no match",
			code: `{{FUNC}}
const rt = new RoutingTable();
rt.addRoute("192.168.1.0", 24, "gw", "eth0");
console.log(rt.lookup("10.0.0.1"));`,
			expected: "null\n",
		},
		{
			name: "more specific route wins",
			code: `{{FUNC}}
const rt = new RoutingTable();
rt.addRoute("10.0.0.0", 8, "gw8", "eth0");
rt.addRoute("10.1.0.0", 16, "gw16", "eth1");
rt.addRoute("10.1.1.0", 24, "gw24", "eth2");
const r = rt.lookup("10.1.1.5");
console.log(r.gateway + " " + r.iface);`,
			expected: "gw24 eth2\n",
		},
	],
};
