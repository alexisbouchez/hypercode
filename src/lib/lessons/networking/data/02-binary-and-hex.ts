import type { Lesson } from "../../types";

export const binaryAndHex: Lesson = {
	id: "binary-and-hex",
	title: "Binary & Hexadecimal",
	chapterId: "basics",
	content: `## Binary & Hexadecimal in Networking

Network addresses and data are fundamentally binary. Understanding binary and hexadecimal notation is essential for working with IP addresses, subnet masks, and MAC addresses.

### Binary Representation

Each IPv4 octet is an 8-bit binary number:

\`\`\`
192 = 11000000
168 = 10101000
  1 = 00000001
\`\`\`

### Hexadecimal

Hexadecimal (base 16) uses digits 0-9 and letters A-F. Each hex digit represents 4 bits:

\`\`\`
0x0 = 0000    0x8 = 1000
0x1 = 0001    0x9 = 1001
0x2 = 0010    0xA = 1010
0x3 = 0011    0xB = 1011
0x4 = 0100    0xC = 1100
0x5 = 0101    0xD = 1101
0x6 = 0110    0xE = 1110
0x7 = 0111    0xF = 1111
\`\`\`

### MAC Addresses

MAC addresses are 48-bit hardware addresses written in hexadecimal:
\`AA:BB:CC:DD:EE:FF\`

### Your Task

Implement three functions:
- \`ipToBinary(ip)\` — converts an IPv4 string to its 32-bit binary representation (dotted, 8 bits per octet)
- \`ipToHex(ip)\` — converts an IPv4 string to hexadecimal (e.g., \`"C0A80101"\`)
- \`binaryToIp(bin)\` — converts a dotted binary string back to an IPv4 address`,

	starterCode: `function ipToBinary(ip) {
	// Your implementation here
}

function ipToHex(ip) {
	// Your implementation here
}

function binaryToIp(bin) {
	// Your implementation here
}

console.log(ipToBinary("192.168.1.1"));
console.log(ipToHex("192.168.1.1"));
console.log(binaryToIp("11000000.10101000.00000001.00000001"));
`,

	solution: `function ipToBinary(ip) {
	return ip.split(".").map(o => Number(o).toString(2).padStart(8, "0")).join(".");
}

function ipToHex(ip) {
	return ip.split(".").map(o => Number(o).toString(16).toUpperCase().padStart(2, "0")).join("");
}

function binaryToIp(bin) {
	return bin.split(".").map(b => parseInt(b, 2)).join(".");
}

console.log(ipToBinary("192.168.1.1"));
console.log(ipToHex("192.168.1.1"));
console.log(binaryToIp("11000000.10101000.00000001.00000001"));
`,

	tests: [
		{
			name: "converts IP to binary and hex, and back",
			expected: "11000000.10101000.00000001.00000001\nC0A80101\n192.168.1.1\n",
		},
		{
			name: "converts 10.0.0.1 to binary",
			code: `{{FUNC}}
console.log(ipToBinary("10.0.0.1"));`,
			expected: "00001010.00000000.00000000.00000001\n",
		},
		{
			name: "converts 255.255.255.0 to hex",
			code: `{{FUNC}}
console.log(ipToHex("255.255.255.0"));`,
			expected: "FFFFFF00\n",
		},
		{
			name: "round-trip conversion",
			code: `{{FUNC}}
const ip = "172.16.254.1";
const result = binaryToIp(ipToBinary(ip));
console.log(result === ip);`,
			expected: "true\n",
		},
	],
};
