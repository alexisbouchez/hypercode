import type { Lesson } from "../../types";

export const materials: Lesson = {
	id: "materials",
	title: "Materials and Colors",
	chapterId: "the-scene",
	content: `## Materials

Materials define the **appearance** of a geometry — its color, shininess, transparency, and how it reacts to light.

### Core Materials

**\`MeshBasicMaterial\`** — flat, unaffected by lights. Always fully bright.
\`\`\`js
new THREE.MeshBasicMaterial({ color: 0xff4400 })
\`\`\`

**\`MeshNormalMaterial\`** — maps surface normals to colors (RGB = XYZ). No lighting needed. Great for debugging.
\`\`\`js
new THREE.MeshNormalMaterial()
\`\`\`

**\`MeshStandardMaterial\`** — physically-based rendering (PBR). Reacts to lights. Use \`roughness\` and \`metalness\` for realism.
\`\`\`js
new THREE.MeshStandardMaterial({ color: 0x2244ff, roughness: 0.4, metalness: 0.8 })
\`\`\`

**\`MeshPhongMaterial\`** — older shading model with shininess. Lighter than Standard.
\`\`\`js
new THREE.MeshPhongMaterial({ color: 0x00ff88, shininess: 100 })
\`\`\`

### Colors

Three.js \`Color\` can be set with:
- Hex number: \`0xff4400\` (R=255, G=68, B=0)
- Hex string: \`"#ff4400"\`
- Color name: \`"tomato"\`
- RGB components: \`new THREE.Color(1, 0.5, 0)\`

\`\`\`js
const mat = new THREE.MeshStandardMaterial({ color: 0xff6347 });
console.log(mat.color.getHexString()); // "ff6347"
console.log(mat.color.r.toFixed(2));   // "1.00"
\`\`\`

### Wireframe

Any material can render as wireframe (edges only):
\`\`\`js
new THREE.MeshBasicMaterial({ color: 0x00ff00, wireframe: true })
\`\`\`

### Your Task

Implement \`createMaterial(hex, roughness, metalness)\` that returns a \`MeshStandardMaterial\` with the given parameters.`,

	starterCode: `function createMaterial(hex, roughness, metalness) {
	// Return a MeshStandardMaterial with the given color, roughness, and metalness
}

const mat = createMaterial(0xff6347, 0.3, 0.7);
console.log(mat.type);
console.log(mat.color.getHexString());
console.log(mat.roughness);
console.log(mat.metalness);

// Preview: a spinning sphere showing material properties
try {
	const renderer = new THREE.WebGLRenderer({ antialias: true });
	renderer.setSize(innerWidth, innerHeight);
	document.body.appendChild(renderer.domElement);

	const scene = new THREE.Scene();
	scene.background = new THREE.Color(0x111122);

	const camera = new THREE.PerspectiveCamera(60, innerWidth / innerHeight, 0.1, 100);
	camera.position.set(0, 0, 5);

	// Lights are needed for MeshStandardMaterial
	scene.add(new THREE.AmbientLight(0xffffff, 0.3));
	const dirLight = new THREE.DirectionalLight(0xffffff, 2);
	dirLight.position.set(5, 5, 5);
	scene.add(dirLight);

	const geometry = new THREE.SphereGeometry(1.5, 64, 64);

	const meshes = [
		{ mat: new THREE.MeshNormalMaterial(), x: -4, label: "Normal" },
		{ mat: new THREE.MeshStandardMaterial({ color: 0xff6347, roughness: 0.2, metalness: 0.8 }), x: 0, label: "Standard" },
		{ mat: new THREE.MeshPhongMaterial({ color: 0x44aaff, shininess: 150 }), x: 4, label: "Phong" },
	];

	meshes.forEach(({ mat, x }) => {
		const mesh = new THREE.Mesh(geometry, mat);
		mesh.position.x = x;
		scene.add(mesh);
	});

	function animate() {
		requestAnimationFrame(animate);
		scene.children.filter(c => c.type === "Mesh").forEach(m => { m.rotation.y += 0.01; });
		renderer.render(scene, camera);
	}
	animate();
} catch (e) {}
`,

	solution: `function createMaterial(hex, roughness, metalness) {
	return new THREE.MeshStandardMaterial({ color: hex, roughness, metalness });
}

const mat = createMaterial(0xff6347, 0.3, 0.7);
console.log(mat.type);
console.log(mat.color.getHexString());
console.log(mat.roughness);
console.log(mat.metalness);

try {
	const renderer = new THREE.WebGLRenderer({ antialias: true });
	renderer.setSize(innerWidth, innerHeight);
	document.body.appendChild(renderer.domElement);
	const scene = new THREE.Scene();
	scene.background = new THREE.Color(0x111122);
	const camera = new THREE.PerspectiveCamera(60, innerWidth / innerHeight, 0.1, 100);
	camera.position.set(0, 0, 5);
	scene.add(new THREE.AmbientLight(0xffffff, 0.3));
	const dirLight = new THREE.DirectionalLight(0xffffff, 2);
	dirLight.position.set(5, 5, 5);
	scene.add(dirLight);
	const geometry = new THREE.SphereGeometry(1.5, 64, 64);
	const meshes = [
		{ mat: new THREE.MeshNormalMaterial(), x: -4 },
		{ mat: new THREE.MeshStandardMaterial({ color: 0xff6347, roughness: 0.2, metalness: 0.8 }), x: 0 },
		{ mat: new THREE.MeshPhongMaterial({ color: 0x44aaff, shininess: 150 }), x: 4 },
	];
	meshes.forEach(({ mat, x }) => { const mesh = new THREE.Mesh(geometry, mat); mesh.position.x = x; scene.add(mesh); });
	function animate() { requestAnimationFrame(animate); scene.children.filter(c => c.type === "Mesh").forEach(m => { m.rotation.y += 0.01; }); renderer.render(scene, camera); }
	animate();
} catch (e) {}
`,

	tests: [
		{
			name: "type is MeshStandardMaterial",
			code: `{{FUNC}}
const m = createMaterial(0xff0000, 0.5, 0.5);
console.log(m.type);`,
			expected: "MeshStandardMaterial\n",
		},
		{
			name: "color hex string matches input",
			code: `{{FUNC}}
const m = createMaterial(0xff6347, 0.3, 0.7);
console.log(m.color.getHexString());`,
			expected: "ff6347\n",
		},
		{
			name: "roughness is set correctly",
			code: `{{FUNC}}
const m = createMaterial(0xffffff, 0.4, 0.6);
console.log(m.roughness);`,
			expected: "0.4\n",
		},
		{
			name: "metalness is set correctly",
			code: `{{FUNC}}
const m = createMaterial(0xffffff, 0.1, 0.9);
console.log(m.metalness);`,
			expected: "0.9\n",
		},
	],
};
