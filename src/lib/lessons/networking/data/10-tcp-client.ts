import type { Lesson } from "../../types";

export const tcpClient: Lesson = {
	id: "tcp-client",
	title: "TCP Client Simulation",
	chapterId: "sockets",
	content: `## TCP Client Simulation

A **TCP client** initiates a connection to a server, sends requests, and processes responses. This lesson simulates the client-side behavior of TCP communication including connection state management and data exchange.

### Client Lifecycle

\`\`\`
1. connect(host, port)  — Initiate connection (three-way handshake)
2. send(data)           — Send data to server
3. receive()            — Read response from server
4. close()              — Terminate connection (four-way teardown)
\`\`\`

### TCP Connection States (Client)

\`\`\`
CLOSED -> SYN_SENT -> ESTABLISHED -> FIN_WAIT -> CLOSED
\`\`\`

### Simulated Network

We will simulate a network where the client connects to a server and exchanges messages. The server echoes back messages with a prefix.

### Your Task

Implement a \`TCPClient\` class:
- \`connect(host, port)\` — transitions to ESTABLISHED, returns handshake log
- \`send(data)\` — queues data for sending, returns bytes queued
- \`receive(response)\` — stores a server response in the receive buffer
- \`getReceived()\` — returns all received data as an array
- \`close()\` — transitions to CLOSED, returns teardown log
- \`getState()\` — returns current connection state

Also implement \`simulateEcho(client, messages)\` that sends messages and simulates the server echoing them back with "ECHO:" prefix.`,

	starterCode: `class TCPClient {
	constructor() {
		this.state = "CLOSED";
		this.host = null;
		this.port = null;
		this.sendQueue = [];
		this.receiveBuffer = [];
	}

	connect(host, port) {
		// Your implementation here
	}

	send(data) {
		// Your implementation here
	}

	receive(response) {
		// Your implementation here
	}

	getReceived() {
		// Your implementation here
	}

	close() {
		// Your implementation here
	}

	getState() {
		// Your implementation here
	}
}

function simulateEcho(client, messages) {
	// Your implementation here
}

const client = new TCPClient();
console.log(client.connect("192.168.1.1", 8080));
console.log(client.getState());
simulateEcho(client, ["hello", "world"]);
console.log(client.getReceived().join(", "));
console.log(client.close());
console.log(client.getState());
`,

	solution: `class TCPClient {
	constructor() {
		this.state = "CLOSED";
		this.host = null;
		this.port = null;
		this.sendQueue = [];
		this.receiveBuffer = [];
	}

	connect(host, port) {
		if (this.state !== "CLOSED") return "ERROR: already connected";
		this.host = host;
		this.port = port;
		this.state = "ESTABLISHED";
		return "SYN -> SYN-ACK -> ACK [" + host + ":" + port + "]";
	}

	send(data) {
		if (this.state !== "ESTABLISHED") return -1;
		this.sendQueue.push(data);
		return data.length;
	}

	receive(response) {
		if (this.state !== "ESTABLISHED") return false;
		this.receiveBuffer.push(response);
		return true;
	}

	getReceived() {
		return [...this.receiveBuffer];
	}

	close() {
		if (this.state !== "ESTABLISHED") return "ERROR: not connected";
		this.state = "CLOSED";
		return "FIN -> ACK -> FIN -> ACK [closed]";
	}

	getState() {
		return this.state;
	}
}

function simulateEcho(client, messages) {
	for (const msg of messages) {
		client.send(msg);
		client.receive("ECHO:" + msg);
	}
}

const client = new TCPClient();
console.log(client.connect("192.168.1.1", 8080));
console.log(client.getState());
simulateEcho(client, ["hello", "world"]);
console.log(client.getReceived().join(", "));
console.log(client.close());
console.log(client.getState());
`,

	tests: [
		{
			name: "full client lifecycle with echo",
			expected: "SYN -> SYN-ACK -> ACK [192.168.1.1:8080]\nESTABLISHED\nECHO:hello, ECHO:world\nFIN -> ACK -> FIN -> ACK [closed]\nCLOSED\n",
		},
		{
			name: "cannot send when closed",
			code: `{{FUNC}}
const client = new TCPClient();
console.log(client.send("test"));`,
			expected: "-1\n",
		},
		{
			name: "cannot connect twice",
			code: `{{FUNC}}
const client = new TCPClient();
client.connect("1.2.3.4", 80);
console.log(client.connect("5.6.7.8", 80));`,
			expected: "ERROR: already connected\n",
		},
		{
			name: "send returns byte count",
			code: `{{FUNC}}
const client = new TCPClient();
client.connect("10.0.0.1", 3000);
console.log(client.send("abcdef"));
console.log(client.send("hi"));`,
			expected: "6\n2\n",
		},
	],
};
