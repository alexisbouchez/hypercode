import type { Lesson } from "../../types";

export const eventDrivenServer: Lesson = {
	id: "event-driven-server",
	title: "Event-Driven Server",
	chapterId: "sockets",
	content: `## Event-Driven Server

Modern servers use an **event-driven** architecture to handle many connections efficiently without creating a thread per connection. This is the model used by Node.js, nginx, and many high-performance servers.

### Event Loop Model

\`\`\`
┌─────────────────────────────────┐
│         Event Queue             │
│  [connect] [data] [close] ...  │
└──────────────┬──────────────────┘
               │
               v
┌─────────────────────────────────┐
│         Event Loop              │
│   while (events.length > 0) {  │
│     event = events.shift();    │
│     handler(event);            │
│   }                            │
└─────────────────────────────────┘
\`\`\`

### Event Types

| Event | Trigger |
|-------|---------|
| \`connect\` | New client connects |
| \`data\` | Client sends data |
| \`close\` | Client disconnects |
| \`error\` | An error occurs |

### Your Task

Implement an \`EventServer\` class:
- \`on(eventType, handler)\` — registers an event handler
- \`emit(eventType, ...args)\` — triggers all handlers for an event type
- \`handleConnection(clientId)\` — emits "connect" event
- \`handleData(clientId, data)\` — emits "data" event
- \`handleClose(clientId)\` — emits "close" event
- \`processEventQueue(events)\` — processes an array of event objects \`{ type, clientId, data? }\` in order, returning a log of all handler outputs`,

	starterCode: `class EventServer {
	constructor() {
		this.handlers = {};
		this.log = [];
	}

	on(eventType, handler) {
		// Your implementation here
	}

	emit(eventType, ...args) {
		// Your implementation here
	}

	handleConnection(clientId) {
		// Your implementation here
	}

	handleData(clientId, data) {
		// Your implementation here
	}

	handleClose(clientId) {
		// Your implementation here
	}

	processEventQueue(events) {
		// Your implementation here
	}
}

const server = new EventServer();
server.on("connect", (id) => "Connected: " + id);
server.on("data", (id, data) => "Data from " + id + ": " + data);
server.on("close", (id) => "Closed: " + id);

const events = [
	{ type: "connect", clientId: "c1" },
	{ type: "data", clientId: "c1", data: "Hello" },
	{ type: "connect", clientId: "c2" },
	{ type: "data", clientId: "c2", data: "World" },
	{ type: "close", clientId: "c1" },
];

const log = server.processEventQueue(events);
log.forEach(entry => console.log(entry));
`,

	solution: `class EventServer {
	constructor() {
		this.handlers = {};
		this.log = [];
	}

	on(eventType, handler) {
		if (!this.handlers[eventType]) this.handlers[eventType] = [];
		this.handlers[eventType].push(handler);
	}

	emit(eventType, ...args) {
		const results = [];
		if (this.handlers[eventType]) {
			for (const handler of this.handlers[eventType]) {
				results.push(handler(...args));
			}
		}
		return results;
	}

	handleConnection(clientId) {
		return this.emit("connect", clientId);
	}

	handleData(clientId, data) {
		return this.emit("data", clientId, data);
	}

	handleClose(clientId) {
		return this.emit("close", clientId);
	}

	processEventQueue(events) {
		const log = [];
		for (const event of events) {
			let results;
			if (event.type === "connect") results = this.handleConnection(event.clientId);
			else if (event.type === "data") results = this.handleData(event.clientId, event.data);
			else if (event.type === "close") results = this.handleClose(event.clientId);
			else results = this.emit(event.type, event.clientId, event.data);
			log.push(...results);
		}
		return log;
	}
}

const server = new EventServer();
server.on("connect", (id) => "Connected: " + id);
server.on("data", (id, data) => "Data from " + id + ": " + data);
server.on("close", (id) => "Closed: " + id);

const events = [
	{ type: "connect", clientId: "c1" },
	{ type: "data", clientId: "c1", data: "Hello" },
	{ type: "connect", clientId: "c2" },
	{ type: "data", clientId: "c2", data: "World" },
	{ type: "close", clientId: "c1" },
];

const log = server.processEventQueue(events);
log.forEach(entry => console.log(entry));
`,

	tests: [
		{
			name: "processes event queue in order",
			expected: "Connected: c1\nData from c1: Hello\nConnected: c2\nData from c2: World\nClosed: c1\n",
		},
		{
			name: "supports multiple handlers per event",
			code: `{{FUNC}}
const server = new EventServer();
server.on("connect", (id) => "Handler1: " + id);
server.on("connect", (id) => "Handler2: " + id);
const results = server.emit("connect", "x");
results.forEach(r => console.log(r));`,
			expected: "Handler1: x\nHandler2: x\n",
		},
		{
			name: "handles empty event queue",
			code: `{{FUNC}}
const server = new EventServer();
const log = server.processEventQueue([]);
console.log(log.length);`,
			expected: "0\n",
		},
	],
};
