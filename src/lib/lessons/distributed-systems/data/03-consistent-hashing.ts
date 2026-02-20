import type { Lesson } from "../../types";

export const consistentHashing: Lesson = {
	id: "consistent-hashing",
	title: "Consistent Hashing",
	chapterId: "data-distribution",
	content: `## Consistent Hashing

When distributing data across N servers, naive hashing (\`key % N\`) means adding or removing a server reshuffles almost every key. **Consistent hashing** minimizes this disruption.

### The Hash Ring

Imagine a ring of integers from 0 to MAX. Each server is placed at one or more points on the ring by hashing its name. To find which server owns a key, hash the key and walk **clockwise** until you hit a server.

When a server is added, only the keys between the new server and its predecessor need to move. When removed, only its keys move to the next server. On average, only \`K/N\` keys move (K = total keys, N = servers).

\`\`\`js
function hashCode(str) {
  let hash = 0;
  for (let i = 0; i < str.length; i++) {
    hash = (hash * 31 + str.charCodeAt(i)) >>> 0;
  }
  return hash % 1000; // ring size 1000
}

class ConsistentHashRing {
  constructor() {
    this.ring = {}; // position -> server
    this.sortedPositions = [];
  }

  addServer(server) {
    const pos = hashCode(server);
    this.ring[pos] = server;
    this.sortedPositions.push(pos);
    this.sortedPositions.sort((a, b) => a - b);
  }

  removeServer(server) {
    const pos = hashCode(server);
    delete this.ring[pos];
    this.sortedPositions = this.sortedPositions.filter(p => p !== pos);
  }

  getServer(key) {
    if (this.sortedPositions.length === 0) return null;
    const hash = hashCode(key);
    for (const pos of this.sortedPositions) {
      if (hash <= pos) return this.ring[pos];
    }
    return this.ring[this.sortedPositions[0]]; // wrap around
  }
}
\`\`\`

### Used By

- **Amazon DynamoDB**, **Apache Cassandra**, **Riak** — distribute partitions across nodes.
- **CDN cache servers** — route requests to the nearest cache.
- **Load balancers** — consistent session routing.

### Your Task

Implement a \`ConsistentHashRing\` class using the \`hashCode\` function provided. Implement \`addServer\`, \`removeServer\`, and \`getServer\`.`,

	starterCode: `function hashCode(str) {
	let hash = 0;
	for (let i = 0; i < str.length; i++) {
		hash = (hash * 31 + str.charCodeAt(i)) >>> 0;
	}
	return hash % 1000;
}

class ConsistentHashRing {
	constructor() {
		this.ring = {};
		this.sortedPositions = [];
	}

	addServer(server) {
		// Place server at its hash position
		// Keep sortedPositions sorted
	}

	removeServer(server) {
		// Remove server from ring and sortedPositions
	}

	getServer(key) {
		// Find first server at or after key's hash position (wrap around)
	}
}

const ring = new ConsistentHashRing();
ring.addServer("server-A");
ring.addServer("server-B");
ring.addServer("server-C");

console.log(ring.getServer("user:1001"));
console.log(ring.getServer("user:1002"));
ring.removeServer("server-B");
console.log(ring.getServer("user:1001"));
`,

	solution: `function hashCode(str) {
	let hash = 0;
	for (let i = 0; i < str.length; i++) {
		hash = (hash * 31 + str.charCodeAt(i)) >>> 0;
	}
	return hash % 1000;
}

class ConsistentHashRing {
	constructor() {
		this.ring = {};
		this.sortedPositions = [];
	}

	addServer(server) {
		const pos = hashCode(server);
		this.ring[pos] = server;
		this.sortedPositions.push(pos);
		this.sortedPositions.sort((a, b) => a - b);
	}

	removeServer(server) {
		const pos = hashCode(server);
		delete this.ring[pos];
		this.sortedPositions = this.sortedPositions.filter(p => p !== pos);
	}

	getServer(key) {
		if (this.sortedPositions.length === 0) return null;
		const hash = hashCode(key);
		for (const pos of this.sortedPositions) {
			if (hash <= pos) return this.ring[pos];
		}
		return this.ring[this.sortedPositions[0]];
	}
}

const ring = new ConsistentHashRing();
ring.addServer("server-A");
ring.addServer("server-B");
ring.addServer("server-C");

console.log(ring.getServer("user:1001"));
console.log(ring.getServer("user:1002"));
ring.removeServer("server-B");
console.log(ring.getServer("user:1001"));
`,

	tests: [
		{
			name: "routes keys to consistent servers",
			expected: "server-A\nserver-C\nserver-A\n",
		},
		{
			name: "empty ring returns null",
			code: `{{FUNC}}
const ring = new ConsistentHashRing();
console.log(ring.getServer("key"));`,
			expected: "null\n",
		},
		{
			name: "single server gets all keys",
			code: `{{FUNC}}
const ring = new ConsistentHashRing();
ring.addServer("only-server");
console.log(ring.getServer("anything"));
console.log(ring.getServer("something-else"));`,
			expected: "only-server\nonly-server\n",
		},
	],
};
