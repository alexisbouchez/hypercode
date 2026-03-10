import type { Lesson } from "../../types";

export const udp: Lesson = {
	id: "udp",
	title: "UDP: User Datagram Protocol",
	chapterId: "protocols",
	content: `## UDP: User Datagram Protocol

**UDP** is a connectionless, lightweight transport protocol. Unlike TCP, it does not guarantee delivery, ordering, or duplicate protection — but it is significantly faster.

### UDP vs TCP

| Feature | TCP | UDP |
|---------|-----|-----|
| Connection | Connection-oriented | Connectionless |
| Reliability | Guaranteed delivery | Best-effort |
| Ordering | Ordered | Unordered |
| Overhead | High (headers, handshake) | Low (8-byte header) |
| Use cases | Web, email, file transfer | Gaming, DNS, video streaming |

### UDP Datagram Header

\`\`\`
 0      15 16     31
+--------+--------+
| Src Port|Dst Port|
+--------+--------+
| Length  |Checksum|
+--------+--------+
|      Data       |
+-----------------+
\`\`\`

The header is only 8 bytes: source port (2), destination port (2), length (2), and checksum (2).

### Checksum

UDP uses a simple checksum to detect corruption. A basic checksum sums all bytes in the data and takes the result modulo 256.

### Your Task

Implement \`createUDPDatagram(srcPort, dstPort, data)\` that creates a simulated UDP datagram object with:
- \`srcPort\`, \`dstPort\`, \`length\` (header 8 bytes + data length), \`checksum\` (sum of char codes mod 256), \`data\`

Also implement \`verifyChecksum(datagram)\` that returns true if the checksum is correct.`,

	starterCode: `function createUDPDatagram(srcPort, dstPort, data) {
	// Your implementation here
}

function verifyChecksum(datagram) {
	// Your implementation here
}

const dg = createUDPDatagram(12345, 53, "hello");
console.log("src=" + dg.srcPort + " dst=" + dg.dstPort);
console.log("length=" + dg.length + " checksum=" + dg.checksum);
console.log("valid=" + verifyChecksum(dg));
`,

	solution: `function createUDPDatagram(srcPort, dstPort, data) {
	const checksum = [...data].reduce((sum, ch) => sum + ch.charCodeAt(0), 0) % 256;
	return {
		srcPort,
		dstPort,
		length: 8 + data.length,
		checksum,
		data,
	};
}

function verifyChecksum(datagram) {
	const expected = [...datagram.data].reduce((sum, ch) => sum + ch.charCodeAt(0), 0) % 256;
	return expected === datagram.checksum;
}

const dg = createUDPDatagram(12345, 53, "hello");
console.log("src=" + dg.srcPort + " dst=" + dg.dstPort);
console.log("length=" + dg.length + " checksum=" + dg.checksum);
console.log("valid=" + verifyChecksum(dg));
`,

	tests: [
		{
			name: "creates datagram and verifies checksum",
			expected: "src=12345 dst=53\nlength=13 checksum=20\nvalid=true\n",
		},
		{
			name: "calculates correct length",
			code: `{{FUNC}}
const dg = createUDPDatagram(80, 80, "abcdef");
console.log(dg.length);`,
			expected: "14\n",
		},
		{
			name: "detects corrupted checksum",
			code: `{{FUNC}}
const dg = createUDPDatagram(1000, 2000, "test");
dg.checksum = 0;
console.log(verifyChecksum(dg));`,
			expected: "false\n",
		},
		{
			name: "handles empty data",
			code: `{{FUNC}}
const dg = createUDPDatagram(5000, 6000, "");
console.log(dg.length + " " + dg.checksum + " " + verifyChecksum(dg));`,
			expected: "8 0 true\n",
		},
	],
};
