import type { Lesson } from "../../types";

export const helloThreejs: Lesson = {
	id: "hello-threejs",
	title: "Hello Three.js",
	chapterId: "the-scene",
	content: `## Hello Three.js

Three.js is a JavaScript library that makes 3D graphics in the browser accessible. It wraps WebGL — the raw GPU API — into a clean, high-level scene graph.

Every Three.js program rests on three pillars:

**Scene** — the world. You add objects to it.
\`\`\`js
const scene = new THREE.Scene();
\`\`\`

**Camera** — the viewpoint. \`PerspectiveCamera\` mimics human vision with a field of view.
\`\`\`js
// PerspectiveCamera(fov, aspectRatio, near, far)
const camera = new THREE.PerspectiveCamera(75, innerWidth / innerHeight, 0.1, 1000);
camera.position.z = 5;
\`\`\`

**Renderer** — draws the scene from the camera's perspective onto a \`<canvas>\`.
\`\`\`js
const renderer = new THREE.WebGLRenderer({ antialias: true });
renderer.setSize(innerWidth, innerHeight);
document.body.appendChild(renderer.domElement);
\`\`\`

### Your First Object

A visible object is a **Mesh** = Geometry (shape) + Material (appearance).

\`\`\`js
const geometry = new THREE.BoxGeometry(1, 1, 1);
const material = new THREE.MeshNormalMaterial(); // colorful, no lighting needed
const cube = new THREE.Mesh(geometry, material);
scene.add(cube);
\`\`\`

### The Render Loop

Everything is static until you animate it. \`requestAnimationFrame\` calls your function ~60 times per second:

\`\`\`js
function animate() {
  requestAnimationFrame(animate);
  cube.rotation.x += 0.01;
  cube.rotation.y += 0.01;
  renderer.render(scene, camera);
}
animate();
\`\`\`

### Your Task

Implement \`createScene()\` that creates a scene, adds a \`BoxGeometry(1, 1, 1)\` mesh with \`MeshNormalMaterial\`, and returns the scene. Then set up the full renderer for the Preview.`,

	starterCode: `// Task: implement createScene()
function createScene() {
	const scene = new THREE.Scene();
	// Create a BoxGeometry(1, 1, 1) with MeshNormalMaterial
	// Add the mesh to the scene
	// Return the scene
}

const scene = createScene();
console.log("Objects in scene:", scene.children.length);
console.log("First object type:", scene.children[0]?.type);

// Preview: full spinning cube
try {
	const renderer = new THREE.WebGLRenderer({ antialias: true });
	renderer.setSize(innerWidth, innerHeight);
	document.body.appendChild(renderer.domElement);

	const camera = new THREE.PerspectiveCamera(75, innerWidth / innerHeight, 0.1, 1000);
	camera.position.z = 3;

	const geometry = new THREE.BoxGeometry(1, 1, 1);
	const material = new THREE.MeshNormalMaterial();
	const cube = new THREE.Mesh(geometry, material);
	scene.add(cube);

	function animate() {
		requestAnimationFrame(animate);
		cube.rotation.x += 0.01;
		cube.rotation.y += 0.01;
		renderer.render(scene, camera);
	}
	animate();
} catch (e) {}
`,

	solution: `function createScene() {
	const scene = new THREE.Scene();
	const geometry = new THREE.BoxGeometry(1, 1, 1);
	const material = new THREE.MeshNormalMaterial();
	const mesh = new THREE.Mesh(geometry, material);
	scene.add(mesh);
	return scene;
}

const scene = createScene();
console.log("Objects in scene:", scene.children.length);
console.log("First object type:", scene.children[0]?.type);

try {
	const renderer = new THREE.WebGLRenderer({ antialias: true });
	renderer.setSize(innerWidth, innerHeight);
	document.body.appendChild(renderer.domElement);

	const camera = new THREE.PerspectiveCamera(75, innerWidth / innerHeight, 0.1, 1000);
	camera.position.z = 3;

	const geometry = new THREE.BoxGeometry(1, 1, 1);
	const material = new THREE.MeshNormalMaterial();
	const cube = new THREE.Mesh(geometry, material);
	scene.add(cube);

	function animate() {
		requestAnimationFrame(animate);
		cube.rotation.x += 0.01;
		cube.rotation.y += 0.01;
		renderer.render(scene, camera);
	}
	animate();
} catch (e) {}
`,

	tests: [
		{
			name: "createScene() returns a scene with 1 mesh",
			code: `{{FUNC}}
const s = createScene();
console.log(s.children.length);`,
			expected: "1\n",
		},
		{
			name: "the object added is a Mesh",
			code: `{{FUNC}}
const s = createScene();
console.log(s.children[0].type);`,
			expected: "Mesh\n",
		},
		{
			name: "geometry is BoxGeometry",
			code: `{{FUNC}}
const s = createScene();
console.log(s.children[0].geometry.type);`,
			expected: "BoxGeometry\n",
		},
		{
			name: "material is MeshNormalMaterial",
			code: `{{FUNC}}
const s = createScene();
console.log(s.children[0].material.type);`,
			expected: "MeshNormalMaterial\n",
		},
	],
};
