import type { Lesson } from "../../types";

export const tcpHandshake: Lesson = {
	id: "tcp-handshake",
	title: "TCP Three-Way Handshake",
	chapterId: "protocols",
	content: `## TCP Three-Way Handshake

TCP (Transmission Control Protocol) establishes a reliable connection using a **three-way handshake** before any data is transferred.

### The Handshake

\`\`\`
Client                    Server
  │                         │
  │──── SYN (seq=x) ──────>│  Step 1: Client sends SYN
  │                         │
  │<── SYN-ACK (seq=y, ─── │  Step 2: Server sends SYN-ACK
  │     ack=x+1)            │
  │                         │
  │──── ACK (seq=x+1, ────>│  Step 3: Client sends ACK
  │     ack=y+1)            │
  │                         │
  │    CONNECTION OPEN      │
\`\`\`

### Sequence Numbers

- **SYN**: The client picks an initial sequence number \`x\`
- **SYN-ACK**: The server picks its own sequence number \`y\` and acknowledges \`x+1\`
- **ACK**: The client acknowledges \`y+1\`

### TCP States

During the handshake, the connection transitions through states:
1. \`CLOSED\` -> \`SYN_SENT\` (client sends SYN)
2. \`LISTEN\` -> \`SYN_RECEIVED\` (server receives SYN, sends SYN-ACK)
3. \`SYN_SENT\` -> \`ESTABLISHED\` (client receives SYN-ACK, sends ACK)
4. \`SYN_RECEIVED\` -> \`ESTABLISHED\` (server receives ACK)

### Your Task

Implement \`tcpHandshake(clientSeq, serverSeq)\` that simulates the three-way handshake. It should return an array of 3 message objects, each with: \`from\`, \`to\`, \`flags\` (array), \`seq\`, \`ack\`.`,

	starterCode: `function tcpHandshake(clientSeq, serverSeq) {
	// Your implementation here
	// Return array of 3 message objects
}

const messages = tcpHandshake(1000, 5000);
for (const m of messages) {
	console.log(m.from + "->" + m.to + " [" + m.flags.join(",") + "] seq=" + m.seq + " ack=" + m.ack);
}
`,

	solution: `function tcpHandshake(clientSeq, serverSeq) {
	return [
		{ from: "client", to: "server", flags: ["SYN"], seq: clientSeq, ack: 0 },
		{ from: "server", to: "client", flags: ["SYN", "ACK"], seq: serverSeq, ack: clientSeq + 1 },
		{ from: "client", to: "server", flags: ["ACK"], seq: clientSeq + 1, ack: serverSeq + 1 },
	];
}

const messages = tcpHandshake(1000, 5000);
for (const m of messages) {
	console.log(m.from + "->" + m.to + " [" + m.flags.join(",") + "] seq=" + m.seq + " ack=" + m.ack);
}
`,

	tests: [
		{
			name: "simulates three-way handshake with seq 1000/5000",
			expected: "client->server [SYN] seq=1000 ack=0\nserver->client [SYN,ACK] seq=5000 ack=1001\nclient->server [ACK] seq=1001 ack=5001\n",
		},
		{
			name: "handshake with seq 0/0",
			code: `{{FUNC}}
const msgs = tcpHandshake(0, 0);
for (const m of msgs) {
	console.log("[" + m.flags.join(",") + "] seq=" + m.seq + " ack=" + m.ack);
}`,
			expected: "[SYN] seq=0 ack=0\n[SYN,ACK] seq=0 ack=1\n[ACK] seq=1 ack=1\n",
		},
		{
			name: "returns exactly 3 messages",
			code: `{{FUNC}}
const msgs = tcpHandshake(100, 200);
console.log(msgs.length);
console.log(msgs[0].flags[0]);
console.log(msgs[1].flags.length);`,
			expected: "3\nSYN\n2\n",
		},
	],
};
