import type { Lesson } from "../../types";

export const animation: Lesson = {
	id: "animation",
	title: "Animation Loop",
	chapterId: "animation-and-interaction",
	content: `## Animation Loop

Three.js scenes are static until you animate them. The browser provides \`requestAnimationFrame\` which calls your function ~60 times per second, synchronized with the display refresh rate.

### Basic Loop

\`\`\`js
function animate() {
  requestAnimationFrame(animate); // schedule next frame
  mesh.rotation.y += 0.01;       // update state
  renderer.render(scene, camera); // draw
}
animate(); // kick it off
\`\`\`

Each call to \`requestAnimationFrame\` schedules exactly one future call — the loop is self-sustaining.

### Delta Time

Running at exactly 60 fps isn't guaranteed. For frame-rate-independent animation, use a \`Clock\`:

\`\`\`js
const clock = new THREE.Clock();

function animate() {
  requestAnimationFrame(animate);
  const delta = clock.getDelta(); // seconds since last frame
  mesh.rotation.y += delta * 1.5; // 1.5 radians/sec
  renderer.render(scene, camera);
}
\`\`\`

### Animating Properties

Any numeric property can be animated: rotation, position, scale, material color/opacity.

\`\`\`js
// Oscillate position with a sine wave
mesh.position.y = Math.sin(clock.getElapsedTime()) * 2;

// Pulse scale
const s = 1 + Math.sin(clock.getElapsedTime() * 2) * 0.3;
mesh.scale.set(s, s, s);
\`\`\`

### Your Task

Implement \`buildAnimator(mesh)\` that takes a mesh and returns an object \`{ step }\` where:
- \`step(delta)\` increments \`mesh.rotation.y\` by \`delta * 2\` and \`mesh.rotation.x\` by \`delta * 1\`
- Returns the mesh (unchanged)`,

	starterCode: `function buildAnimator(mesh) {
	return {
		step(delta) {
			// Increment mesh.rotation.y by delta * 2
			// Increment mesh.rotation.x by delta * 1
		}
	};
}

const mesh = new THREE.Mesh(new THREE.BoxGeometry(1, 1, 1), new THREE.MeshNormalMaterial());
const animator = buildAnimator(mesh);
animator.step(0.5);
console.log(mesh.rotation.y.toFixed(4));
console.log(mesh.rotation.x.toFixed(4));

// Preview: animated spinning cube with delta time
try {
	const renderer = new THREE.WebGLRenderer({ antialias: true });
	renderer.setSize(innerWidth, innerHeight);
	document.body.appendChild(renderer.domElement);

	const scene = new THREE.Scene();
	scene.background = new THREE.Color(0x050510);

	const camera = new THREE.PerspectiveCamera(60, innerWidth / innerHeight, 0.1, 100);
	camera.position.set(0, 2, 6);
	camera.lookAt(0, 0, 0);

	scene.add(new THREE.AmbientLight(0xffffff, 0.4));
	const dir = new THREE.DirectionalLight(0xffffff, 2);
	dir.position.set(5, 5, 5);
	scene.add(dir);

	const clock = new THREE.Clock();
	const geo = new THREE.BoxGeometry(1.5, 1.5, 1.5);
	const mat = new THREE.MeshStandardMaterial({ color: 0x4488ff, roughness: 0.3, metalness: 0.6 });
	const cube = new THREE.Mesh(geo, mat);
	scene.add(cube);

	function animate() {
		requestAnimationFrame(animate);
		const delta = clock.getDelta();
		const elapsed = clock.getElapsedTime();
		cube.rotation.y += delta * 1.5;
		cube.rotation.x += delta * 0.5;
		cube.position.y = Math.sin(elapsed * 2) * 0.5;
		renderer.render(scene, camera);
	}
	animate();
} catch (e) {}
`,

	solution: `function buildAnimator(mesh) {
	return {
		step(delta) {
			mesh.rotation.y += delta * 2;
			mesh.rotation.x += delta * 1;
		}
	};
}

const mesh = new THREE.Mesh(new THREE.BoxGeometry(1, 1, 1), new THREE.MeshNormalMaterial());
const animator = buildAnimator(mesh);
animator.step(0.5);
console.log(mesh.rotation.y.toFixed(4));
console.log(mesh.rotation.x.toFixed(4));

try {
	const renderer = new THREE.WebGLRenderer({ antialias: true });
	renderer.setSize(innerWidth, innerHeight);
	document.body.appendChild(renderer.domElement);
	const scene = new THREE.Scene();
	scene.background = new THREE.Color(0x050510);
	const camera = new THREE.PerspectiveCamera(60, innerWidth / innerHeight, 0.1, 100);
	camera.position.set(0, 2, 6);
	camera.lookAt(0, 0, 0);
	scene.add(new THREE.AmbientLight(0xffffff, 0.4));
	const dir = new THREE.DirectionalLight(0xffffff, 2);
	dir.position.set(5, 5, 5);
	scene.add(dir);
	const clock = new THREE.Clock();
	const cube = new THREE.Mesh(new THREE.BoxGeometry(1.5, 1.5, 1.5), new THREE.MeshStandardMaterial({ color: 0x4488ff, roughness: 0.3, metalness: 0.6 }));
	scene.add(cube);
	function animate() { requestAnimationFrame(animate); const delta = clock.getDelta(); const elapsed = clock.getElapsedTime(); cube.rotation.y += delta * 1.5; cube.rotation.x += delta * 0.5; cube.position.y = Math.sin(elapsed * 2) * 0.5; renderer.render(scene, camera); }
	animate();
} catch (e) {}
`,

	tests: [
		{
			name: "rotation.y incremented by delta*2",
			code: `{{FUNC}}
const mesh = new THREE.Mesh(new THREE.BoxGeometry(1,1,1), new THREE.MeshNormalMaterial());
const a = buildAnimator(mesh);
a.step(0.5);
console.log(mesh.rotation.y.toFixed(4));`,
			expected: "1.0000\n",
		},
		{
			name: "rotation.x incremented by delta*1",
			code: `{{FUNC}}
const mesh = new THREE.Mesh(new THREE.BoxGeometry(1,1,1), new THREE.MeshNormalMaterial());
const a = buildAnimator(mesh);
a.step(0.5);
console.log(mesh.rotation.x.toFixed(4));`,
			expected: "0.5000\n",
		},
		{
			name: "multiple steps accumulate",
			code: `{{FUNC}}
const mesh = new THREE.Mesh(new THREE.BoxGeometry(1,1,1), new THREE.MeshNormalMaterial());
const a = buildAnimator(mesh);
a.step(0.1);
a.step(0.1);
a.step(0.1);
console.log(mesh.rotation.y.toFixed(4));`,
			expected: "0.6000\n",
		},
		{
			name: "step with delta=1",
			code: `{{FUNC}}
const mesh = new THREE.Mesh(new THREE.BoxGeometry(1,1,1), new THREE.MeshNormalMaterial());
const a = buildAnimator(mesh);
a.step(1);
console.log(mesh.rotation.y.toFixed(4));
console.log(mesh.rotation.x.toFixed(4));`,
			expected: "2.0000\n1.0000\n",
		},
	],
};
