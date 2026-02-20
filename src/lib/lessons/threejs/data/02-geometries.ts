import type { Lesson } from "../../types";

export const geometries: Lesson = {
	id: "geometries",
	title: "Geometries",
	chapterId: "the-scene",
	content: `## Geometries

Geometry defines the **shape** of a 3D object — its vertices, edges, and faces. Three.js ships with many built-in geometries:

| Class | Description | Key Parameters |
|-------|-------------|----------------|
| \`BoxGeometry\` | Rectangular box | width, height, depth |
| \`SphereGeometry\` | UV sphere | radius, widthSegments, heightSegments |
| \`CylinderGeometry\` | Cylinder/cone | radiusTop, radiusBottom, height, segments |
| \`TorusGeometry\` | Donut/ring | radius, tube, radialSegments, tubularSegments |
| \`PlaneGeometry\` | Flat plane | width, height |
| \`ConeGeometry\` | Cone | radius, height, segments |

\`\`\`js
const box    = new THREE.BoxGeometry(2, 1, 1);
const sphere = new THREE.SphereGeometry(0.5, 32, 32);
const torus  = new THREE.TorusGeometry(1, 0.4, 16, 100);
\`\`\`

### Accessing Parameters

Each geometry stores its constructor arguments in \`.parameters\`:

\`\`\`js
const sphere = new THREE.SphereGeometry(2, 16, 16);
console.log(sphere.parameters.radius);         // 2
console.log(sphere.parameters.widthSegments);  // 16
\`\`\`

Segment count controls smoothness — more segments = smoother, but more triangles.

### Your Task

Implement the two functions below:
- \`createSphere(radius, segments)\` — returns a \`SphereGeometry\`
- \`createTorus(radius, tube)\` — returns a \`TorusGeometry\` with 16 radialSegments and 100 tubularSegments`,

	starterCode: `function createSphere(radius, segments) {
	// Return a SphereGeometry(radius, segments, segments)
}

function createTorus(radius, tube) {
	// Return a TorusGeometry(radius, tube, 16, 100)
}

const sphere = createSphere(1.5, 32);
console.log(sphere.parameters.radius);
console.log(sphere.parameters.widthSegments);

const torus = createTorus(2, 0.5);
console.log(torus.parameters.radius);
console.log(torus.parameters.tube);

// Preview: showcase of geometries
try {
	const renderer = new THREE.WebGLRenderer({ antialias: true });
	renderer.setSize(innerWidth, innerHeight);
	document.body.appendChild(renderer.domElement);

	const scene = new THREE.Scene();
	scene.background = new THREE.Color(0x0a0a1a);
	const camera = new THREE.PerspectiveCamera(75, innerWidth / innerHeight, 0.1, 1000);
	camera.position.set(0, 0, 8);

	const material = new THREE.MeshNormalMaterial({ wireframe: false });

	const shapes = [
		new THREE.Mesh(new THREE.BoxGeometry(1.5, 1.5, 1.5), material),
		new THREE.Mesh(new THREE.SphereGeometry(0.9, 32, 32), material),
		new THREE.Mesh(new THREE.TorusGeometry(0.8, 0.3, 16, 100), material),
		new THREE.Mesh(new THREE.CylinderGeometry(0.5, 0.5, 1.5, 32), material),
		new THREE.Mesh(new THREE.ConeGeometry(0.8, 1.5, 32), material),
	];

	shapes.forEach((mesh, i) => {
		mesh.position.x = (i - 2) * 3;
		scene.add(mesh);
	});

	function animate() {
		requestAnimationFrame(animate);
		shapes.forEach((m) => { m.rotation.x += 0.008; m.rotation.y += 0.012; });
		renderer.render(scene, camera);
	}
	animate();
} catch (e) {}
`,

	solution: `function createSphere(radius, segments) {
	return new THREE.SphereGeometry(radius, segments, segments);
}

function createTorus(radius, tube) {
	return new THREE.TorusGeometry(radius, tube, 16, 100);
}

const sphere = createSphere(1.5, 32);
console.log(sphere.parameters.radius);
console.log(sphere.parameters.widthSegments);

const torus = createTorus(2, 0.5);
console.log(torus.parameters.radius);
console.log(torus.parameters.tube);

try {
	const renderer = new THREE.WebGLRenderer({ antialias: true });
	renderer.setSize(innerWidth, innerHeight);
	document.body.appendChild(renderer.domElement);

	const scene = new THREE.Scene();
	scene.background = new THREE.Color(0x0a0a1a);
	const camera = new THREE.PerspectiveCamera(75, innerWidth / innerHeight, 0.1, 1000);
	camera.position.set(0, 0, 8);

	const material = new THREE.MeshNormalMaterial();
	const shapes = [
		new THREE.Mesh(new THREE.BoxGeometry(1.5, 1.5, 1.5), material),
		new THREE.Mesh(new THREE.SphereGeometry(0.9, 32, 32), material),
		new THREE.Mesh(new THREE.TorusGeometry(0.8, 0.3, 16, 100), material),
		new THREE.Mesh(new THREE.CylinderGeometry(0.5, 0.5, 1.5, 32), material),
		new THREE.Mesh(new THREE.ConeGeometry(0.8, 1.5, 32), material),
	];
	shapes.forEach((mesh, i) => { mesh.position.x = (i - 2) * 3; scene.add(mesh); });
	function animate() {
		requestAnimationFrame(animate);
		shapes.forEach((m) => { m.rotation.x += 0.008; m.rotation.y += 0.012; });
		renderer.render(scene, camera);
	}
	animate();
} catch (e) {}
`,

	tests: [
		{
			name: "createSphere radius=1.5",
			code: `{{FUNC}}
const s = createSphere(1.5, 32);
console.log(s.parameters.radius);`,
			expected: "1.5\n",
		},
		{
			name: "createSphere widthSegments=32",
			code: `{{FUNC}}
const s = createSphere(1, 32);
console.log(s.parameters.widthSegments);`,
			expected: "32\n",
		},
		{
			name: "createTorus radius=2",
			code: `{{FUNC}}
const t = createTorus(2, 0.5);
console.log(t.parameters.radius);`,
			expected: "2\n",
		},
		{
			name: "createTorus tubularSegments=100",
			code: `{{FUNC}}
const t = createTorus(1, 0.3);
console.log(t.parameters.tubularSegments);`,
			expected: "100\n",
		},
	],
};
