import type { Lesson } from "../../types";

export const tcpServer: Lesson = {
	id: "tcp-server",
	title: "TCP Server Simulation",
	chapterId: "sockets",
	content: `## TCP Server Simulation

A **TCP server** listens on a port, accepts incoming connections, and processes requests. In this lesson, we simulate the server-side behavior of TCP communication.

### Server Lifecycle

\`\`\`
1. bind(port)    — Reserve a port
2. listen()      — Start accepting connections
3. accept()      — Accept a client connection
4. receive()     — Read data from client
5. send()        — Send response to client
6. close()       — Close connection
\`\`\`

### Connection Management

A server maintains a list of active connections. Each connection has:
- A client address (IP:port)
- A state (LISTENING, CONNECTED, CLOSED)
- A send/receive buffer

### Your Task

Implement a \`TCPServer\` class that simulates a TCP server:
- \`bind(port)\` — binds to a port, returns true if successful
- \`listen()\` — starts listening (must be bound first)
- \`accept(clientAddr)\` — accepts a connection, returns a connection ID
- \`receive(connId, data)\` — buffers received data for a connection
- \`send(connId, data)\` — returns the data that would be sent
- \`getBuffer(connId)\` — returns all buffered received data
- \`close(connId)\` — closes a connection
- \`getState(connId)\` — returns connection state`,

	starterCode: `class TCPServer {
	constructor() {
		this.port = null;
		this.listening = false;
		this.connections = {};
		this.nextConnId = 1;
	}

	bind(port) {
		// Your implementation here
	}

	listen() {
		// Your implementation here
	}

	accept(clientAddr) {
		// Your implementation here
	}

	receive(connId, data) {
		// Your implementation here
	}

	send(connId, data) {
		// Your implementation here
	}

	getBuffer(connId) {
		// Your implementation here
	}

	close(connId) {
		// Your implementation here
	}

	getState(connId) {
		// Your implementation here
	}
}

const server = new TCPServer();
console.log(server.bind(8080));
console.log(server.listen());
const connId = server.accept("192.168.1.10:50000");
console.log("conn=" + connId + " state=" + server.getState(connId));
server.receive(connId, "Hello");
server.receive(connId, " World");
console.log("buffer: " + server.getBuffer(connId));
console.log("send: " + server.send(connId, "ACK"));
server.close(connId);
console.log("state=" + server.getState(connId));
`,

	solution: `class TCPServer {
	constructor() {
		this.port = null;
		this.listening = false;
		this.connections = {};
		this.nextConnId = 1;
	}

	bind(port) {
		if (this.port !== null) return false;
		this.port = port;
		return true;
	}

	listen() {
		if (this.port === null) return false;
		this.listening = true;
		return true;
	}

	accept(clientAddr) {
		if (!this.listening) return -1;
		const id = this.nextConnId++;
		this.connections[id] = { clientAddr, state: "CONNECTED", buffer: "" };
		return id;
	}

	receive(connId, data) {
		const conn = this.connections[connId];
		if (!conn || conn.state !== "CONNECTED") return false;
		conn.buffer += data;
		return true;
	}

	send(connId, data) {
		const conn = this.connections[connId];
		if (!conn || conn.state !== "CONNECTED") return null;
		return data;
	}

	getBuffer(connId) {
		const conn = this.connections[connId];
		if (!conn) return null;
		return conn.buffer;
	}

	close(connId) {
		const conn = this.connections[connId];
		if (!conn) return false;
		conn.state = "CLOSED";
		return true;
	}

	getState(connId) {
		const conn = this.connections[connId];
		if (!conn) return "UNKNOWN";
		return conn.state;
	}
}

const server = new TCPServer();
console.log(server.bind(8080));
console.log(server.listen());
const connId = server.accept("192.168.1.10:50000");
console.log("conn=" + connId + " state=" + server.getState(connId));
server.receive(connId, "Hello");
server.receive(connId, " World");
console.log("buffer: " + server.getBuffer(connId));
console.log("send: " + server.send(connId, "ACK"));
server.close(connId);
console.log("state=" + server.getState(connId));
`,

	tests: [
		{
			name: "full server lifecycle",
			expected: "true\ntrue\nconn=1 state=CONNECTED\nbuffer: Hello World\nsend: ACK\nstate=CLOSED\n",
		},
		{
			name: "rejects accept before listen",
			code: `{{FUNC}}
const server = new TCPServer();
server.bind(3000);
console.log(server.accept("10.0.0.1:5000"));`,
			expected: "-1\n",
		},
		{
			name: "manages multiple connections",
			code: `{{FUNC}}
const server = new TCPServer();
server.bind(80);
server.listen();
const c1 = server.accept("10.0.0.1:5000");
const c2 = server.accept("10.0.0.2:5001");
server.receive(c1, "req1");
server.receive(c2, "req2");
console.log(server.getBuffer(c1));
console.log(server.getBuffer(c2));
console.log(c1 !== c2);`,
			expected: "req1\nreq2\ntrue\n",
		},
		{
			name: "cannot send on closed connection",
			code: `{{FUNC}}
const server = new TCPServer();
server.bind(80);
server.listen();
const c = server.accept("10.0.0.1:5000");
server.close(c);
console.log(server.send(c, "data"));`,
			expected: "null\n",
		},
	],
};
