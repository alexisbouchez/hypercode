import type { Lesson } from "../../types";

export const fog: Lesson = {
	id: "fog",
	title: "Fog",
	chapterId: "animation-and-interaction",
	content: `## Fog

Fog makes distant objects fade into the background, creating a sense of atmosphere and depth. Set it directly on the scene.

### Fog (Linear)

\`THREE.Fog\` fades linearly from \`near\` to \`far\` distance:

\`\`\`js
scene.fog = new THREE.Fog(0xccccff, 10, 50);
// color, near (start), far (full opacity)
\`\`\`

Objects closer than \`near\` are unaffected; beyond \`far\` they are completely the fog color.

### FogExp2 (Exponential)

\`THREE.FogExp2\` uses an exponential curve for more realistic-looking haze:

\`\`\`js
scene.fog = new THREE.FogExp2(0x8899aa, 0.05);
// color, density (higher = thicker)
\`\`\`

### Matching Background

For a seamless look, match the scene background to the fog color:

\`\`\`js
const fogColor = 0x334455;
scene.background = new THREE.Color(fogColor);
scene.fog = new THREE.Fog(fogColor, 10, 60);
\`\`\`

### Fog Properties

\`\`\`js
const fog = new THREE.Fog(0xffffff, 5, 30);
console.log(fog.color.getHexString()); // "ffffff"
console.log(fog.near);  // 5
console.log(fog.far);   // 30

const fog2 = new THREE.FogExp2(0xaabbcc, 0.02);
console.log(fog2.density); // 0.02
\`\`\`

### Your Task

Implement \`createFog(colorHex, near, far)\` that returns a \`THREE.Fog\` with the given parameters.`,

	starterCode: `function createFog(colorHex, near, far) {
	// Return a new THREE.Fog with the given color, near, and far
}

const fog = createFog(0x334455, 5, 40);
console.log(fog.color.getHexString());
console.log(fog.near);
console.log(fog.far);

// Preview: foggy forest of boxes
try {
	const renderer = new THREE.WebGLRenderer({ antialias: true });
	renderer.setSize(innerWidth, innerHeight);
	document.body.appendChild(renderer.domElement);

	const fogColor = 0x334455;
	const scene = new THREE.Scene();
	scene.background = new THREE.Color(fogColor);
	scene.fog = createFog(fogColor, 5, 40);

	const camera = new THREE.PerspectiveCamera(60, innerWidth / innerHeight, 0.1, 200);
	camera.position.set(0, 2, 0);

	scene.add(new THREE.AmbientLight(0xffffff, 0.6));
	const dir = new THREE.DirectionalLight(0xffffff, 1);
	dir.position.set(1, 2, 1);
	scene.add(dir);

	// Ground
	const ground = new THREE.Mesh(
		new THREE.PlaneGeometry(200, 200),
		new THREE.MeshStandardMaterial({ color: 0x223322 })
	);
	ground.rotation.x = -Math.PI / 2;
	scene.add(ground);

	// Trees (boxes)
	const treeMat = new THREE.MeshStandardMaterial({ color: 0x226622 });
	for (let i = 0; i < 80; i++) {
		const h = 1 + Math.random() * 3;
		const tree = new THREE.Mesh(new THREE.BoxGeometry(0.5, h, 0.5), treeMat);
		tree.position.set((Math.random() - 0.5) * 60, h / 2, -Math.random() * 50 - 2);
		scene.add(tree);
	}

	const clock = new THREE.Clock();
	function animate() {
		requestAnimationFrame(animate);
		camera.position.z -= clock.getDelta() * 3;
		if (camera.position.z < -48) camera.position.z = 0;
		renderer.render(scene, camera);
	}
	animate();
} catch (e) {}
`,

	solution: `function createFog(colorHex, near, far) {
	return new THREE.Fog(colorHex, near, far);
}

const fog = createFog(0x334455, 5, 40);
console.log(fog.color.getHexString());
console.log(fog.near);
console.log(fog.far);

try {
	const renderer = new THREE.WebGLRenderer({ antialias: true });
	renderer.setSize(innerWidth, innerHeight);
	document.body.appendChild(renderer.domElement);
	const fogColor = 0x334455;
	const scene = new THREE.Scene();
	scene.background = new THREE.Color(fogColor);
	scene.fog = createFog(fogColor, 5, 40);
	const camera = new THREE.PerspectiveCamera(60, innerWidth / innerHeight, 0.1, 200);
	camera.position.set(0, 2, 0);
	scene.add(new THREE.AmbientLight(0xffffff, 0.6));
	const dir = new THREE.DirectionalLight(0xffffff, 1);
	dir.position.set(1, 2, 1);
	scene.add(dir);
	const ground = new THREE.Mesh(new THREE.PlaneGeometry(200, 200), new THREE.MeshStandardMaterial({ color: 0x223322 }));
	ground.rotation.x = -Math.PI / 2;
	scene.add(ground);
	const treeMat = new THREE.MeshStandardMaterial({ color: 0x226622 });
	for (let i = 0; i < 80; i++) { const h = 1 + Math.random() * 3; const tree = new THREE.Mesh(new THREE.BoxGeometry(0.5, h, 0.5), treeMat); tree.position.set((Math.random() - 0.5) * 60, h / 2, -Math.random() * 50 - 2); scene.add(tree); }
	const clock = new THREE.Clock();
	function animate() { requestAnimationFrame(animate); camera.position.z -= clock.getDelta() * 3; if (camera.position.z < -48) camera.position.z = 0; renderer.render(scene, camera); }
	animate();
} catch (e) {}
`,

	tests: [
		{
			name: "fog color matches input",
			code: `{{FUNC}}
const f = createFog(0x334455, 5, 40);
console.log(f.color.getHexString());`,
			expected: "334455\n",
		},
		{
			name: "fog near is set",
			code: `{{FUNC}}
const f = createFog(0xffffff, 10, 50);
console.log(f.near);`,
			expected: "10\n",
		},
		{
			name: "fog far is set",
			code: `{{FUNC}}
const f = createFog(0xffffff, 10, 50);
console.log(f.far);`,
			expected: "50\n",
		},
		{
			name: "returns a Fog instance",
			code: `{{FUNC}}
const f = createFog(0xffffff, 1, 100);
console.log(f.isFog);`,
			expected: "true\n",
		},
	],
};
