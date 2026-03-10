import type { Lesson } from "../../types";

export const subnets: Lesson = {
	id: "subnets",
	title: "Subnet Masks & CIDR",
	chapterId: "basics",
	content: `## Subnet Masks & CIDR

A **subnet mask** determines which portion of an IP address identifies the network and which identifies the host. **CIDR** (Classless Inter-Domain Routing) notation uses a slash followed by the number of network bits.

### Subnet Mask Basics

\`\`\`
IP:      192.168.1.100
Mask:    255.255.255.0   (/24)
Network: 192.168.1.0
Host:    0.0.0.100
\`\`\`

The network address is obtained by ANDing the IP with the mask. The broadcast address has all host bits set to 1.

### CIDR Notation

| CIDR | Subnet Mask | Hosts |
|------|-------------|-------|
| /8   | 255.0.0.0 | 16,777,214 |
| /16  | 255.255.0.0 | 65,534 |
| /24  | 255.255.255.0 | 254 |
| /28  | 255.255.255.240 | 14 |
| /32  | 255.255.255.255 | 1 |

Number of usable hosts = 2^(32 - prefix) - 2 (excluding network and broadcast addresses).

### Your Task

Implement \`subnetInfo(cidr)\` that takes a CIDR string (e.g., \`"192.168.1.100/24"\`) and returns an object with:
- \`networkAddress\`: the network address
- \`broadcastAddress\`: the broadcast address
- \`subnetMask\`: dotted-decimal mask
- \`usableHosts\`: number of usable host addresses`,

	starterCode: `function subnetInfo(cidr) {
	// Your implementation here
}

const info = subnetInfo("192.168.1.100/24");
console.log("Network: " + info.networkAddress);
console.log("Broadcast: " + info.broadcastAddress);
console.log("Mask: " + info.subnetMask);
console.log("Hosts: " + info.usableHosts);
`,

	solution: `function subnetInfo(cidr) {
	const [ipStr, prefixStr] = cidr.split("/");
	const prefix = Number(prefixStr);
	const octets = ipStr.split(".").map(Number);

	const ip = (octets[0] << 24 | octets[1] << 16 | octets[2] << 8 | octets[3]) >>> 0;
	const mask = prefix === 0 ? 0 : (0xFFFFFFFF << (32 - prefix)) >>> 0;
	const network = (ip & mask) >>> 0;
	const broadcast = (network | (~mask >>> 0)) >>> 0;
	const usableHosts = prefix >= 31 ? (prefix === 32 ? 1 : 2) : Math.pow(2, 32 - prefix) - 2;

	const toIp = (n) => [(n >>> 24) & 255, (n >>> 16) & 255, (n >>> 8) & 255, n & 255].join(".");

	return {
		networkAddress: toIp(network),
		broadcastAddress: toIp(broadcast),
		subnetMask: toIp(mask),
		usableHosts,
	};
}

const info = subnetInfo("192.168.1.100/24");
console.log("Network: " + info.networkAddress);
console.log("Broadcast: " + info.broadcastAddress);
console.log("Mask: " + info.subnetMask);
console.log("Hosts: " + info.usableHosts);
`,

	tests: [
		{
			name: "computes /24 subnet info",
			expected: "Network: 192.168.1.0\nBroadcast: 192.168.1.255\nMask: 255.255.255.0\nHosts: 254\n",
		},
		{
			name: "computes /28 subnet info",
			code: `{{FUNC}}
const info = subnetInfo("10.0.0.50/28");
console.log(info.networkAddress);
console.log(info.broadcastAddress);
console.log(info.usableHosts);`,
			expected: "10.0.0.48\n10.0.0.63\n14\n",
		},
		{
			name: "computes /16 subnet info",
			code: `{{FUNC}}
const info = subnetInfo("172.16.5.130/16");
console.log(info.networkAddress);
console.log(info.subnetMask);
console.log(info.usableHosts);`,
			expected: "172.16.0.0\n255.255.0.0\n65534\n",
		},
		{
			name: "computes /32 single host",
			code: `{{FUNC}}
const info = subnetInfo("8.8.8.8/32");
console.log(info.networkAddress);
console.log(info.broadcastAddress);
console.log(info.usableHosts);`,
			expected: "8.8.8.8\n8.8.8.8\n1\n",
		},
	],
};
