import type { Lesson } from "../../types";

export const camera: Lesson = {
	id: "camera",
	title: "Camera & Viewport",
	chapterId: "animation-and-interaction",
	content: `## Camera & Viewport

Three.js has several camera types. \`PerspectiveCamera\` is the most common — it mimics how human eyes perceive depth.

### PerspectiveCamera Parameters

\`\`\`js
new THREE.PerspectiveCamera(fov, aspect, near, far)
\`\`\`

| Parameter | Description | Typical Value |
|-----------|-------------|---------------|
| \`fov\` | Vertical field of view in degrees | 45–90 |
| \`aspect\` | Width / Height ratio | \`innerWidth / innerHeight\` |
| \`near\` | Near clipping plane | \`0.1\` |
| \`far\` | Far clipping plane | \`1000\` |

Objects outside \`[near, far]\` range are not rendered.

### lookAt

Point the camera at any position in the world:

\`\`\`js
camera.position.set(5, 3, 10);
camera.lookAt(0, 0, 0); // look at origin
camera.lookAt(target.position); // look at an object
\`\`\`

### Updating on Resize

When the window resizes, update the camera and renderer:

\`\`\`js
window.addEventListener('resize', () => {
  camera.aspect = innerWidth / innerHeight;
  camera.updateProjectionMatrix();
  renderer.setSize(innerWidth, innerHeight);
});
\`\`\`

\`updateProjectionMatrix()\` is required after changing \`fov\`, \`aspect\`, \`near\`, or \`far\`.

### OrthographicCamera

For 2D or isometric views:

\`\`\`js
new THREE.OrthographicCamera(left, right, top, bottom, near, far)
\`\`\`

### Your Task

Implement \`createCamera(fov, aspect, near, far, px, py, pz)\` that:
- Creates a \`PerspectiveCamera\` with the given parameters
- Sets position to \`(px, py, pz)\`
- Calls \`lookAt(0, 0, 0)\`
- Returns the camera`,

	starterCode: `function createCamera(fov, aspect, near, far, px, py, pz) {
	// Create PerspectiveCamera, set position, lookAt origin
}

const cam = createCamera(75, 16/9, 0.1, 1000, 5, 3, 8);
console.log(cam.fov);
console.log(cam.near);
console.log(cam.far);
console.log(cam.position.x);

// Preview: multiple camera angles switching
try {
	const renderer = new THREE.WebGLRenderer({ antialias: true });
	renderer.setSize(innerWidth, innerHeight);
	document.body.appendChild(renderer.domElement);

	const scene = new THREE.Scene();
	scene.background = new THREE.Color(0x0a0a1a);

	const cameras = [
		createCamera(75, innerWidth / innerHeight, 0.1, 1000, 0, 0, 8),
		createCamera(45, innerWidth / innerHeight, 0.1, 1000, 8, 4, 0),
		createCamera(110, innerWidth / innerHeight, 0.1, 1000, 0, 8, 0),
	];
	let camIndex = 0;

	scene.add(new THREE.AmbientLight(0xffffff, 0.3));
	const dir = new THREE.DirectionalLight(0xffeedd, 2);
	dir.position.set(5, 8, 5);
	scene.add(dir);

	const geo = new THREE.BoxGeometry(1.5, 1.5, 1.5);
	[-3, 0, 3].forEach((x, i) => {
		const mesh = new THREE.Mesh(geo, new THREE.MeshStandardMaterial({ color: [0xff4400, 0x4488ff, 0x44ff88][i], roughness: 0.3, metalness: 0.5 }));
		mesh.position.x = x;
		scene.add(mesh);
	});

	setInterval(() => { camIndex = (camIndex + 1) % cameras.length; }, 2000);

	const clock = new THREE.Clock();
	function animate() {
		requestAnimationFrame(animate);
		scene.children.filter(c => c.type === "Mesh").forEach(m => { m.rotation.y += 0.01; });
		renderer.render(scene, cameras[camIndex]);
	}
	animate();
} catch (e) {}
`,

	solution: `function createCamera(fov, aspect, near, far, px, py, pz) {
	const cam = new THREE.PerspectiveCamera(fov, aspect, near, far);
	cam.position.set(px, py, pz);
	cam.lookAt(0, 0, 0);
	return cam;
}

const cam = createCamera(75, 16/9, 0.1, 1000, 5, 3, 8);
console.log(cam.fov);
console.log(cam.near);
console.log(cam.far);
console.log(cam.position.x);

try {
	const renderer = new THREE.WebGLRenderer({ antialias: true });
	renderer.setSize(innerWidth, innerHeight);
	document.body.appendChild(renderer.domElement);
	const scene = new THREE.Scene();
	scene.background = new THREE.Color(0x0a0a1a);
	const cameras = [
		createCamera(75, innerWidth / innerHeight, 0.1, 1000, 0, 0, 8),
		createCamera(45, innerWidth / innerHeight, 0.1, 1000, 8, 4, 0),
		createCamera(110, innerWidth / innerHeight, 0.1, 1000, 0, 8, 0),
	];
	let camIndex = 0;
	scene.add(new THREE.AmbientLight(0xffffff, 0.3));
	const dir = new THREE.DirectionalLight(0xffeedd, 2);
	dir.position.set(5, 8, 5);
	scene.add(dir);
	const geo = new THREE.BoxGeometry(1.5, 1.5, 1.5);
	[-3, 0, 3].forEach((x, i) => { const mesh = new THREE.Mesh(geo, new THREE.MeshStandardMaterial({ color: [0xff4400, 0x4488ff, 0x44ff88][i], roughness: 0.3, metalness: 0.5 })); mesh.position.x = x; scene.add(mesh); });
	setInterval(() => { camIndex = (camIndex + 1) % cameras.length; }, 2000);
	function animate() { requestAnimationFrame(animate); scene.children.filter(c => c.type === "Mesh").forEach(m => { m.rotation.y += 0.01; }); renderer.render(scene, cameras[camIndex]); }
	animate();
} catch (e) {}
`,

	tests: [
		{
			name: "fov is set",
			code: `{{FUNC}}
const c = createCamera(75, 1, 0.1, 1000, 0, 0, 5);
console.log(c.fov);`,
			expected: "75\n",
		},
		{
			name: "near and far are set",
			code: `{{FUNC}}
const c = createCamera(60, 1, 0.5, 500, 0, 0, 5);
console.log(c.near);
console.log(c.far);`,
			expected: "0.5\n500\n",
		},
		{
			name: "position is set",
			code: `{{FUNC}}
const c = createCamera(75, 1, 0.1, 1000, 5, 3, 8);
console.log(c.position.x);
console.log(c.position.y);`,
			expected: "5\n3\n",
		},
		{
			name: "type is PerspectiveCamera",
			code: `{{FUNC}}
const c = createCamera(75, 1, 0.1, 1000, 0, 0, 5);
console.log(c.type);`,
			expected: "PerspectiveCamera\n",
		},
	],
};
