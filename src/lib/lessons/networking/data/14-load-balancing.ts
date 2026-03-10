import type { Lesson } from "../../types";

export const loadBalancing: Lesson = {
	id: "load-balancing",
	title: "Load Balancing",
	chapterId: "advanced",
	content: `## Load Balancing

A **load balancer** distributes incoming network traffic across multiple servers to improve reliability, performance, and availability.

### Algorithms

#### Round Robin
Requests are distributed sequentially across servers in a circular order.

\`\`\`
Request 1 -> Server A
Request 2 -> Server B
Request 3 -> Server C
Request 4 -> Server A  (cycles back)
\`\`\`

#### Least Connections
Each request goes to the server with the fewest active connections.

#### Weighted Round Robin
Servers with higher weight receive more requests proportionally.

### Health Checks

Load balancers periodically check server health. Unhealthy servers are removed from the pool.

### Your Task

Implement a \`LoadBalancer\` class with multiple strategies:
- \`addServer(id, weight)\` — adds a server with optional weight (default 1)
- \`removeServer(id)\` — removes a server
- \`roundRobin()\` — returns next server using round-robin
- \`leastConnections()\` — returns server with fewest connections
- \`connect(serverId)\` — increments connection count
- \`disconnect(serverId)\` — decrements connection count
- \`getStats()\` — returns connection counts for all servers`,

	starterCode: `class LoadBalancer {
	constructor() {
		this.servers = [];
		this.connections = {};
		this.rrIndex = 0;
	}

	addServer(id, weight) {
		// Your implementation here
	}

	removeServer(id) {
		// Your implementation here
	}

	roundRobin() {
		// Your implementation here
	}

	leastConnections() {
		// Your implementation here
	}

	connect(serverId) {
		// Your implementation here
	}

	disconnect(serverId) {
		// Your implementation here
	}

	getStats() {
		// Your implementation here
	}
}

const lb = new LoadBalancer();
lb.addServer("A");
lb.addServer("B");
lb.addServer("C");

console.log("RR: " + lb.roundRobin());
console.log("RR: " + lb.roundRobin());
console.log("RR: " + lb.roundRobin());
console.log("RR: " + lb.roundRobin());

lb.connect("A");
lb.connect("A");
lb.connect("B");
console.log("LC: " + lb.leastConnections());
`,

	solution: `class LoadBalancer {
	constructor() {
		this.servers = [];
		this.connections = {};
		this.rrIndex = 0;
	}

	addServer(id, weight) {
		this.servers.push({ id, weight: weight || 1 });
		this.connections[id] = 0;
	}

	removeServer(id) {
		this.servers = this.servers.filter(s => s.id !== id);
		delete this.connections[id];
		if (this.rrIndex >= this.servers.length) this.rrIndex = 0;
	}

	roundRobin() {
		if (this.servers.length === 0) return null;
		const server = this.servers[this.rrIndex % this.servers.length];
		this.rrIndex = (this.rrIndex + 1) % this.servers.length;
		return server.id;
	}

	leastConnections() {
		if (this.servers.length === 0) return null;
		let minConn = Infinity;
		let best = null;
		for (const s of this.servers) {
			if (this.connections[s.id] < minConn) {
				minConn = this.connections[s.id];
				best = s.id;
			}
		}
		return best;
	}

	connect(serverId) {
		if (this.connections[serverId] !== undefined) {
			this.connections[serverId]++;
		}
	}

	disconnect(serverId) {
		if (this.connections[serverId] !== undefined && this.connections[serverId] > 0) {
			this.connections[serverId]--;
		}
	}

	getStats() {
		return { ...this.connections };
	}
}

const lb = new LoadBalancer();
lb.addServer("A");
lb.addServer("B");
lb.addServer("C");

console.log("RR: " + lb.roundRobin());
console.log("RR: " + lb.roundRobin());
console.log("RR: " + lb.roundRobin());
console.log("RR: " + lb.roundRobin());

lb.connect("A");
lb.connect("A");
lb.connect("B");
console.log("LC: " + lb.leastConnections());
`,

	tests: [
		{
			name: "round-robin and least-connections",
			expected: "RR: A\nRR: B\nRR: C\nRR: A\nLC: C\n",
		},
		{
			name: "least connections after disconnect",
			code: `{{FUNC}}
const lb = new LoadBalancer();
lb.addServer("X");
lb.addServer("Y");
lb.connect("X");
lb.connect("X");
lb.connect("Y");
lb.disconnect("X");
const stats = lb.getStats();
console.log("X=" + stats["X"] + " Y=" + stats["Y"]);
console.log("LC: " + lb.leastConnections());`,
			expected: "X=1 Y=1\nLC: X\n",
		},
		{
			name: "round-robin after server removal",
			code: `{{FUNC}}
const lb = new LoadBalancer();
lb.addServer("A");
lb.addServer("B");
lb.addServer("C");
lb.roundRobin();
lb.removeServer("B");
console.log(lb.roundRobin());
console.log(lb.roundRobin());`,
			expected: "C\nA\n",
		},
		{
			name: "returns null with no servers",
			code: `{{FUNC}}
const lb = new LoadBalancer();
console.log(lb.roundRobin());
console.log(lb.leastConnections());`,
			expected: "null\nnull\n",
		},
	],
};
