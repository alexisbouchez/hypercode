import type { Lesson } from "../../types";

export const raycasting: Lesson = {
	id: "raycasting",
	title: "Raycasting",
	chapterId: "animation-and-interaction",
	content: `## Raycasting

Raycasting lets you detect which 3D objects the mouse is hovering over or clicking on. A ray is cast from the camera through the mouse position into the scene.

### Setting Up

\`\`\`js
const raycaster = new THREE.Raycaster();
const pointer = new THREE.Vector2();

window.addEventListener('mousemove', (e) => {
  // Normalize to [-1, +1] range
  pointer.x =  (e.clientX / innerWidth)  * 2 - 1;
  pointer.y = -(e.clientY / innerHeight) * 2 + 1; // Y is flipped
});
\`\`\`

### Casting the Ray

In the render loop, update the raycaster and check for intersections:

\`\`\`js
raycaster.setFromCamera(pointer, camera);
const intersects = raycaster.intersectObjects(scene.children);

if (intersects.length > 0) {
  const hit = intersects[0]; // closest object
  hit.object.material.color.set(0xff0000); // highlight
}
\`\`\`

### Intersection Data

Each intersection contains:
- \`object\` — the intersected mesh
- \`distance\` — distance from camera
- \`point\` — world-space hit point (\`Vector3\`)
- \`face\` — the hit face

### Recursive

Pass \`true\` as second arg to check inside groups:

\`\`\`js
raycaster.intersectObjects(scene.children, true);
\`\`\`

### Your Task

Implement \`normalizePointer(clientX, clientY, width, height)\` that returns a \`THREE.Vector2\` with properly normalized coordinates for raycasting.`,

	starterCode: `function normalizePointer(clientX, clientY, width, height) {
	// Return a THREE.Vector2 with:
	// x = (clientX / width) * 2 - 1
	// y = -(clientY / height) * 2 + 1
}

const p = normalizePointer(400, 300, 800, 600);
console.log(p.x.toFixed(4));
console.log(p.y.toFixed(4));

const corner = normalizePointer(0, 0, 800, 600);
console.log(corner.x.toFixed(4));
console.log(corner.y.toFixed(4));

// Preview: clickable colored boxes
try {
	const renderer = new THREE.WebGLRenderer({ antialias: true });
	renderer.setSize(innerWidth, innerHeight);
	document.body.appendChild(renderer.domElement);

	const scene = new THREE.Scene();
	scene.background = new THREE.Color(0x0a0a1a);

	const camera = new THREE.PerspectiveCamera(60, innerWidth / innerHeight, 0.1, 100);
	camera.position.set(0, 0, 8);

	scene.add(new THREE.AmbientLight(0xffffff, 0.4));
	const dir = new THREE.DirectionalLight(0xffffff, 2);
	dir.position.set(5, 5, 5);
	scene.add(dir);

	const meshes = [];
	const colors = [0xff4400, 0x4488ff, 0x44ff88, 0xffaa00, 0xff44aa];
	[-4, -2, 0, 2, 4].forEach((x, i) => {
		const mesh = new THREE.Mesh(
			new THREE.BoxGeometry(1.2, 1.2, 1.2),
			new THREE.MeshStandardMaterial({ color: colors[i] })
		);
		mesh.position.x = x;
		mesh.userData.baseColor = colors[i];
		scene.add(mesh);
		meshes.push(mesh);
	});

	const raycaster = new THREE.Raycaster();
	const pointer = new THREE.Vector2();

	window.addEventListener('mousemove', (e) => {
		const p = normalizePointer(e.clientX, e.clientY, innerWidth, innerHeight);
		pointer.copy(p);
		raycaster.setFromCamera(pointer, camera);
		const hits = raycaster.intersectObjects(meshes);
		meshes.forEach(m => m.material.color.set(m.userData.baseColor));
		if (hits.length > 0) hits[0].object.material.color.set(0xffffff);
	});

	function animate() {
		requestAnimationFrame(animate);
		meshes.forEach(m => { m.rotation.y += 0.01; });
		renderer.render(scene, camera);
	}
	animate();
} catch (e) {}
`,

	solution: `function normalizePointer(clientX, clientY, width, height) {
	return new THREE.Vector2(
		(clientX / width) * 2 - 1,
		-(clientY / height) * 2 + 1
	);
}

const p = normalizePointer(400, 300, 800, 600);
console.log(p.x.toFixed(4));
console.log(p.y.toFixed(4));

const corner = normalizePointer(0, 0, 800, 600);
console.log(corner.x.toFixed(4));
console.log(corner.y.toFixed(4));

try {
	const renderer = new THREE.WebGLRenderer({ antialias: true });
	renderer.setSize(innerWidth, innerHeight);
	document.body.appendChild(renderer.domElement);
	const scene = new THREE.Scene();
	scene.background = new THREE.Color(0x0a0a1a);
	const camera = new THREE.PerspectiveCamera(60, innerWidth / innerHeight, 0.1, 100);
	camera.position.set(0, 0, 8);
	scene.add(new THREE.AmbientLight(0xffffff, 0.4));
	const dir = new THREE.DirectionalLight(0xffffff, 2);
	dir.position.set(5, 5, 5);
	scene.add(dir);
	const meshes = [];
	const colors = [0xff4400, 0x4488ff, 0x44ff88, 0xffaa00, 0xff44aa];
	[-4, -2, 0, 2, 4].forEach((x, i) => {
		const mesh = new THREE.Mesh(new THREE.BoxGeometry(1.2, 1.2, 1.2), new THREE.MeshStandardMaterial({ color: colors[i] }));
		mesh.position.x = x; mesh.userData.baseColor = colors[i];
		scene.add(mesh); meshes.push(mesh);
	});
	const raycaster = new THREE.Raycaster();
	const pointer = new THREE.Vector2();
	window.addEventListener('mousemove', (e) => {
		const p = normalizePointer(e.clientX, e.clientY, innerWidth, innerHeight);
		pointer.copy(p);
		raycaster.setFromCamera(pointer, camera);
		const hits = raycaster.intersectObjects(meshes);
		meshes.forEach(m => m.material.color.set(m.userData.baseColor));
		if (hits.length > 0) hits[0].object.material.color.set(0xffffff);
	});
	function animate() { requestAnimationFrame(animate); meshes.forEach(m => { m.rotation.y += 0.01; }); renderer.render(scene, camera); }
	animate();
} catch (e) {}
`,

	tests: [
		{
			name: "center of screen maps to (0, 0)",
			code: `{{FUNC}}
const p = normalizePointer(400, 300, 800, 600);
console.log(p.x.toFixed(4));
console.log(p.y.toFixed(4));`,
			expected: "0.0000\n0.0000\n",
		},
		{
			name: "top-left maps to (-1, 1)",
			code: `{{FUNC}}
const p = normalizePointer(0, 0, 800, 600);
console.log(p.x.toFixed(4));
console.log(p.y.toFixed(4));`,
			expected: "-1.0000\n1.0000\n",
		},
		{
			name: "bottom-right maps to (1, -1)",
			code: `{{FUNC}}
const p = normalizePointer(800, 600, 800, 600);
console.log(p.x.toFixed(4));
console.log(p.y.toFixed(4));`,
			expected: "1.0000\n-1.0000\n",
		},
		{
			name: "returns a Vector2",
			code: `{{FUNC}}
const p = normalizePointer(400, 300, 800, 600);
console.log(p.isVector2);`,
			expected: "true\n",
		},
	],
};
