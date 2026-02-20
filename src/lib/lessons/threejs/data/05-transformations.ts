import type { Lesson } from "../../types";

export const transformations: Lesson = {
	id: "transformations",
	title: "Transformations",
	chapterId: "the-scene",
	content: `## Transformations

Every Three.js object has three transform properties that control its position, orientation, and size in the world.

### Position

\`position\` is a \`Vector3\`. Move objects with \`set(x, y, z)\` or assign individual axes.

\`\`\`js
mesh.position.set(2, 1, -3);
mesh.position.x = 5; // individual axis
\`\`\`

### Rotation

\`rotation\` is an \`Euler\` (angles in **radians**). Three.js uses a right-hand coordinate system.

\`\`\`js
mesh.rotation.y = Math.PI / 4; // 45° around Y axis
mesh.rotation.set(0, Math.PI, 0); // 180° flip
\`\`\`

Convert degrees: \`radians = degrees * (Math.PI / 180)\`

### Scale

\`scale\` is a \`Vector3\`, defaulting to \`(1, 1, 1)\`. Values < 1 shrink, > 1 grow.

\`\`\`js
mesh.scale.set(2, 0.5, 2); // wide and flat
mesh.scale.x = 3;          // stretch horizontally only
\`\`\`

### Chaining

Transformations combine: a mesh at \`position (0, 2, 0)\` scaled by \`(2, 2, 2)\` appears large and elevated.

\`\`\`js
const mesh = new THREE.Mesh(
  new THREE.BoxGeometry(1, 1, 1),
  new THREE.MeshNormalMaterial()
);
mesh.position.set(1, 2, 0);
mesh.rotation.y = Math.PI / 4;
mesh.scale.set(1.5, 1.5, 1.5);
\`\`\`

### Your Task

Implement \`transformMesh(px, py, pz, ry, sx)\` that:
- Creates a \`BoxGeometry(1, 1, 1)\` with \`MeshNormalMaterial\`
- Sets position to \`(px, py, pz)\`
- Sets \`rotation.y\` to \`ry\`
- Sets \`scale.x\` to \`sx\`
- Returns the mesh`,

	starterCode: `function transformMesh(px, py, pz, ry, sx) {
	const mesh = new THREE.Mesh(
		new THREE.BoxGeometry(1, 1, 1),
		new THREE.MeshNormalMaterial()
	);
	// Set position, rotation.y, and scale.x
	return mesh;
}

const m = transformMesh(3, 1, -2, Math.PI / 4, 2);
console.log(m.position.x);
console.log(m.position.y);
console.log(m.rotation.y.toFixed(4));
console.log(m.scale.x);

// Preview: transformed boxes
try {
	const renderer = new THREE.WebGLRenderer({ antialias: true });
	renderer.setSize(innerWidth, innerHeight);
	document.body.appendChild(renderer.domElement);

	const scene = new THREE.Scene();
	scene.background = new THREE.Color(0x0a0a1a);

	const camera = new THREE.PerspectiveCamera(60, innerWidth / innerHeight, 0.1, 100);
	camera.position.set(0, 4, 10);
	camera.lookAt(0, 0, 0);

	const configs = [
		{ pos: [-4, 0, 0], ry: 0, scale: [1, 1, 1] },
		{ pos: [0, 0, 0], ry: Math.PI / 4, scale: [1, 2, 1] },
		{ pos: [4, 0, 0], ry: Math.PI / 2, scale: [2, 1, 2] },
	];

	configs.forEach(({ pos, ry, scale }) => {
		const mesh = new THREE.Mesh(
			new THREE.BoxGeometry(1, 1, 1),
			new THREE.MeshNormalMaterial()
		);
		mesh.position.set(...pos);
		mesh.rotation.y = ry;
		mesh.scale.set(...scale);
		scene.add(mesh);
	});

	function animate() {
		requestAnimationFrame(animate);
		scene.children.filter(c => c.type === "Mesh").forEach(m => { m.rotation.x += 0.005; });
		renderer.render(scene, camera);
	}
	animate();
} catch (e) {}
`,

	solution: `function transformMesh(px, py, pz, ry, sx) {
	const mesh = new THREE.Mesh(
		new THREE.BoxGeometry(1, 1, 1),
		new THREE.MeshNormalMaterial()
	);
	mesh.position.set(px, py, pz);
	mesh.rotation.y = ry;
	mesh.scale.x = sx;
	return mesh;
}

const m = transformMesh(3, 1, -2, Math.PI / 4, 2);
console.log(m.position.x);
console.log(m.position.y);
console.log(m.rotation.y.toFixed(4));
console.log(m.scale.x);

try {
	const renderer = new THREE.WebGLRenderer({ antialias: true });
	renderer.setSize(innerWidth, innerHeight);
	document.body.appendChild(renderer.domElement);
	const scene = new THREE.Scene();
	scene.background = new THREE.Color(0x0a0a1a);
	const camera = new THREE.PerspectiveCamera(60, innerWidth / innerHeight, 0.1, 100);
	camera.position.set(0, 4, 10);
	camera.lookAt(0, 0, 0);
	[[-4, 0, 0, 0, 1, 1, 1], [0, 0, 0, Math.PI / 4, 1, 2, 1], [4, 0, 0, Math.PI / 2, 2, 1, 2]].forEach(([x, y, z, ry, sx, sy, sz]) => {
		const mesh = new THREE.Mesh(new THREE.BoxGeometry(1, 1, 1), new THREE.MeshNormalMaterial());
		mesh.position.set(x, y, z); mesh.rotation.y = ry; mesh.scale.set(sx, sy, sz); scene.add(mesh);
	});
	function animate() { requestAnimationFrame(animate); scene.children.filter(c => c.type === "Mesh").forEach(m => { m.rotation.x += 0.005; }); renderer.render(scene, camera); }
	animate();
} catch (e) {}
`,

	tests: [
		{
			name: "position.x is set",
			code: `{{FUNC}}
const m = transformMesh(3, 1, -2, 0, 1);
console.log(m.position.x);`,
			expected: "3\n",
		},
		{
			name: "position.y is set",
			code: `{{FUNC}}
const m = transformMesh(3, 1, -2, 0, 1);
console.log(m.position.y);`,
			expected: "1\n",
		},
		{
			name: "rotation.y is set",
			code: `{{FUNC}}
const m = transformMesh(0, 0, 0, Math.PI / 4, 1);
console.log(m.rotation.y.toFixed(4));`,
			expected: "0.7854\n",
		},
		{
			name: "scale.x is set",
			code: `{{FUNC}}
const m = transformMesh(0, 0, 0, 0, 2);
console.log(m.scale.x);`,
			expected: "2\n",
		},
	],
};
