import type { Lesson } from "../../types";

export const httpRequestResponse: Lesson = {
	id: "http-request-response",
	title: "HTTP Request & Response",
	chapterId: "protocols",
	content: `## HTTP Request & Response

**HTTP** (HyperText Transfer Protocol) is the foundation of data exchange on the web. It follows a request-response model where a client sends a request and a server returns a response.

### HTTP Request Structure

\`\`\`
GET /index.html HTTP/1.1
Host: www.example.com
User-Agent: Mozilla/5.0
Accept: text/html

\`\`\`

A request has:
- **Request line**: method, path, HTTP version
- **Headers**: key-value pairs
- **Body** (optional, for POST/PUT)

### HTTP Response Structure

\`\`\`
HTTP/1.1 200 OK
Content-Type: text/html
Content-Length: 13

Hello, World!
\`\`\`

### Common Methods

| Method | Purpose | Has Body |
|--------|---------|----------|
| GET | Retrieve resource | No |
| POST | Create resource | Yes |
| PUT | Update resource | Yes |
| DELETE | Remove resource | No |

### Status Codes

| Range | Category | Examples |
|-------|----------|----------|
| 1xx | Informational | 100 Continue |
| 2xx | Success | 200 OK, 201 Created |
| 3xx | Redirection | 301 Moved, 304 Not Modified |
| 4xx | Client Error | 400 Bad Request, 404 Not Found |
| 5xx | Server Error | 500 Internal Server Error |

### Your Task

Implement \`parseHTTPRequest(raw)\` that parses a raw HTTP request string into an object with: \`method\`, \`path\`, \`version\`, \`headers\` (object), and \`body\`.

Also implement \`buildHTTPResponse(status, statusText, headers, body)\` that builds a raw HTTP response string.`,

	starterCode: `function parseHTTPRequest(raw) {
	// Your implementation here
}

function buildHTTPResponse(status, statusText, headers, body) {
	// Your implementation here
}

const req = parseHTTPRequest("GET /api/users HTTP/1.1\\r\\nHost: example.com\\r\\nAccept: application/json\\r\\n\\r\\n");
console.log(req.method + " " + req.path + " " + req.version);
console.log("Host: " + req.headers["Host"]);

const res = buildHTTPResponse(200, "OK", { "Content-Type": "text/plain" }, "Hello");
console.log(res);
`,

	solution: `function parseHTTPRequest(raw) {
	const [headerSection, ...bodyParts] = raw.split("\\r\\n\\r\\n");
	const body = bodyParts.join("\\r\\n\\r\\n");
	const lines = headerSection.split("\\r\\n");
	const [method, path, version] = lines[0].split(" ");

	const headers = {};
	for (let i = 1; i < lines.length; i++) {
		if (lines[i] === "") continue;
		const colonIdx = lines[i].indexOf(": ");
		headers[lines[i].substring(0, colonIdx)] = lines[i].substring(colonIdx + 2);
	}

	return { method, path, version, headers, body };
}

function buildHTTPResponse(status, statusText, headers, body) {
	let res = "HTTP/1.1 " + status + " " + statusText + "\\r\\n";
	for (const [k, v] of Object.entries(headers)) {
		res += k + ": " + v + "\\r\\n";
	}
	res += "\\r\\n" + body;
	return res;
}

const req = parseHTTPRequest("GET /api/users HTTP/1.1\\r\\nHost: example.com\\r\\nAccept: application/json\\r\\n\\r\\n");
console.log(req.method + " " + req.path + " " + req.version);
console.log("Host: " + req.headers["Host"]);

const res = buildHTTPResponse(200, "OK", { "Content-Type": "text/plain" }, "Hello");
console.log(res);
`,

	tests: [
		{
			name: "parses GET request and builds response",
			expected: "GET /api/users HTTP/1.1\nHost: example.com\nHTTP/1.1 200 OK\r\nContent-Type: text/plain\r\n\r\nHello\n",
		},
		{
			name: "parses POST request with body",
			code: `{{FUNC}}
const req = parseHTTPRequest("POST /api/data HTTP/1.1\\r\\nContent-Type: application/json\\r\\n\\r\\n{\\"key\\":\\"value\\"}");
console.log(req.method);
console.log(req.body);`,
			expected: "POST\n{\"key\":\"value\"}\n",
		},
		{
			name: "builds 404 response",
			code: `{{FUNC}}
const res = buildHTTPResponse(404, "Not Found", { "Content-Type": "text/html" }, "<h1>Not Found</h1>");
const lines = res.split("\\r\\n");
console.log(lines[0]);
console.log(lines[3]);`,
			expected: "HTTP/1.1 404 Not Found\n<h1>Not Found</h1>\n",
		},
		{
			name: "parses multiple headers",
			code: `{{FUNC}}
const req = parseHTTPRequest("GET / HTTP/1.1\\r\\nHost: test.com\\r\\nAccept: */*\\r\\nConnection: keep-alive\\r\\n\\r\\n");
console.log(Object.keys(req.headers).length);
console.log(req.headers["Connection"]);`,
			expected: "3\nkeep-alive\n",
		},
	],
};
