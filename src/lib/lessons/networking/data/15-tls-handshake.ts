import type { Lesson } from "../../types";

export const tlsHandshake: Lesson = {
	id: "tls-handshake",
	title: "TLS Handshake",
	chapterId: "advanced",
	content: `## TLS Handshake

**TLS** (Transport Layer Security) provides encryption, authentication, and integrity for network communication. HTTPS = HTTP + TLS. The TLS handshake establishes a secure connection before any application data is exchanged.

### TLS 1.2 Handshake Steps

\`\`\`
Client                              Server
  │                                   │
  │── ClientHello ──────────────────>│  (supported ciphers, random)
  │                                   │
  │<──────────────── ServerHello ────│  (chosen cipher, random)
  │<──────────────── Certificate ────│  (server's public key)
  │<──────────── ServerHelloDone ────│
  │                                   │
  │── ClientKeyExchange ────────────>│  (pre-master secret)
  │── ChangeCipherSpec ─────────────>│
  │── Finished ─────────────────────>│
  │                                   │
  │<─────────── ChangeCipherSpec ────│
  │<──────────────────── Finished ───│
  │                                   │
  │       SECURE CONNECTION          │
\`\`\`

### Key Concepts

- **Cipher Suite**: Combination of key exchange, encryption, and MAC algorithms (e.g., \`TLS_RSA_WITH_AES_256_CBC_SHA256\`)
- **Pre-Master Secret**: Random value encrypted with server's public key
- **Master Secret**: Derived from pre-master secret + client random + server random
- **Session Keys**: Derived from master secret, used for symmetric encryption

### Your Task

Implement \`simulateTLSHandshake(clientCiphers, serverCiphers, serverCert)\` that simulates a TLS handshake:
1. Client sends supported ciphers and a random value
2. Server picks the first matching cipher and sends its random + certificate
3. Client generates a pre-master secret, derives a master secret (simplified: XOR of all three values)
4. Both sides derive session keys

Return an array of handshake message objects with \`step\`, \`from\`, \`to\`, and \`data\`.

Also implement \`deriveSessionKey(preMaster, clientRandom, serverRandom)\` that returns a simplified "key" (sum mod 256).`,

	starterCode: `function deriveSessionKey(preMaster, clientRandom, serverRandom) {
	// Your implementation here
}

function simulateTLSHandshake(clientCiphers, serverCiphers, serverCert) {
	// Your implementation here
	// Return array of handshake step objects
}

const steps = simulateTLSHandshake(
	["AES_256_CBC", "AES_128_CBC", "RC4"],
	["AES_128_CBC", "AES_256_CBC"],
	"server-cert-xyz"
);

for (const s of steps) {
	console.log(s.step + ": " + s.from + "->" + s.to + " " + JSON.stringify(s.data));
}
`,

	solution: `function deriveSessionKey(preMaster, clientRandom, serverRandom) {
	return (preMaster + clientRandom + serverRandom) % 256;
}

function simulateTLSHandshake(clientCiphers, serverCiphers, serverCert) {
	const clientRandom = 42;
	const serverRandom = 87;
	const preMaster = 153;

	const chosenCipher = clientCiphers.find(c => serverCiphers.includes(c)) || null;
	if (!chosenCipher) return [{ step: "Error", from: "server", to: "client", data: { error: "no common cipher" } }];

	const sessionKey = deriveSessionKey(preMaster, clientRandom, serverRandom);

	return [
		{ step: "ClientHello", from: "client", to: "server", data: { ciphers: clientCiphers, random: clientRandom } },
		{ step: "ServerHello", from: "server", to: "client", data: { cipher: chosenCipher, random: serverRandom } },
		{ step: "Certificate", from: "server", to: "client", data: { cert: serverCert } },
		{ step: "ClientKeyExchange", from: "client", to: "server", data: { preMaster } },
		{ step: "Finished", from: "both", to: "both", data: { sessionKey, cipher: chosenCipher } },
	];
}

const steps = simulateTLSHandshake(
	["AES_256_CBC", "AES_128_CBC", "RC4"],
	["AES_128_CBC", "AES_256_CBC"],
	"server-cert-xyz"
);

for (const s of steps) {
	console.log(s.step + ": " + s.from + "->" + s.to + " " + JSON.stringify(s.data));
}
`,

	tests: [
		{
			name: "simulates full TLS handshake",
			expected: `ClientHello: client->server {"ciphers":["AES_256_CBC","AES_128_CBC","RC4"],"random":42}\nServerHello: server->client {"cipher":"AES_256_CBC","random":87}\nCertificate: server->client {"cert":"server-cert-xyz"}\nClientKeyExchange: client->server {"preMaster":153}\nFinished: both->both {"sessionKey":26,"cipher":"AES_256_CBC"}\n`,
		},
		{
			name: "derives session key correctly",
			code: `{{FUNC}}
console.log(deriveSessionKey(100, 50, 25));
console.log(deriveSessionKey(200, 200, 200));`,
			expected: "175\n88\n",
		},
		{
			name: "picks first common cipher",
			code: `{{FUNC}}
const steps = simulateTLSHandshake(["RC4", "AES_128"], ["AES_128", "RC4"], "cert");
const serverHello = steps.find(s => s.step === "ServerHello");
console.log(serverHello.data.cipher);`,
			expected: "RC4\n",
		},
		{
			name: "handles no common cipher",
			code: `{{FUNC}}
const steps = simulateTLSHandshake(["AES_256"], ["RC4"], "cert");
console.log(steps[0].step);
console.log(steps[0].data.error);`,
			expected: "Error\nno common cipher\n",
		},
	],
};
