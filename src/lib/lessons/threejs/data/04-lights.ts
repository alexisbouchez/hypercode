import type { Lesson } from "../../types";

export const lights: Lesson = {
	id: "lights",
	title: "Lights",
	chapterId: "lights-and-shadows",
	content: `## Lights

\`MeshStandardMaterial\` and \`MeshPhongMaterial\` require lights — without them, objects appear black. \`MeshBasicMaterial\` and \`MeshNormalMaterial\` are unaffected by lights.

### Types of Lights

**\`AmbientLight\`** — fills the whole scene uniformly. No direction, no shadows. Use it to prevent completely dark areas.
\`\`\`js
const ambient = new THREE.AmbientLight(0xffffff, 0.4);
scene.add(ambient);
\`\`\`

**\`DirectionalLight\`** — parallel rays from one direction (like the sun). Creates shadows. Position determines direction.
\`\`\`js
const sun = new THREE.DirectionalLight(0xfff4cc, 2.0);
sun.position.set(5, 10, 5);
scene.add(sun);
\`\`\`

**\`PointLight\`** — emits light in all directions from a single point (like a light bulb). Has range via \`distance\`.
\`\`\`js
const bulb = new THREE.PointLight(0xff8800, 3, 20);
bulb.position.set(0, 3, 0);
scene.add(bulb);
\`\`\`

**\`SpotLight\`** — cone-shaped light with inner/outer angle (like a stage spotlight).
\`\`\`js
const spot = new THREE.SpotLight(0xffffff, 2, 30, Math.PI / 6);
spot.position.set(0, 10, 0);
scene.add(spot);
\`\`\`

### Color and Intensity

Light color multiplies with material color. White light (\`0xffffff\`) leaves material color unchanged. Intensity scales brightness.

\`\`\`js
const light = new THREE.DirectionalLight(0x4488ff, 1.5);
console.log(light.color.getHexString()); // "4488ff"
console.log(light.intensity);            // 1.5
\`\`\`

### Your Task

Implement \`createLightRig()\` that returns an object \`{ ambient, directional }\` where:
- \`ambient\` is an \`AmbientLight\` with color \`0xffffff\` and intensity \`0.3\`
- \`directional\` is a \`DirectionalLight\` with color \`0xffeedd\` and intensity \`2\`, positioned at \`(5, 8, 5)\``,

	starterCode: `function createLightRig() {
	const ambient = new THREE.AmbientLight(/* color, intensity */);
	const directional = new THREE.DirectionalLight(/* color, intensity */);
	directional.position.set(/* x, y, z */);
	return { ambient, directional };
}

const { ambient, directional } = createLightRig();
console.log(ambient.type);
console.log(ambient.intensity);
console.log(directional.color.getHexString());
console.log(directional.position.x);

// Preview: spheres lit with different lights
try {
	const renderer = new THREE.WebGLRenderer({ antialias: true });
	renderer.setSize(innerWidth, innerHeight);
	document.body.appendChild(renderer.domElement);

	const scene = new THREE.Scene();
	scene.background = new THREE.Color(0x050510);

	const camera = new THREE.PerspectiveCamera(60, innerWidth / innerHeight, 0.1, 100);
	camera.position.set(0, 2, 8);
	camera.lookAt(0, 0, 0);

	const { ambient: a, directional: d } = createLightRig();
	scene.add(a, d);

	// Add colorful point lights
	[[0xff2200, -4, 0], [0x0044ff, 4, 0], [0x00ff88, 0, 3]].forEach(([color, x, z]) => {
		const pt = new THREE.PointLight(color, 3, 12);
		pt.position.set(x, 2, z);
		scene.add(pt);
	});

	const geo = new THREE.SphereGeometry(1, 64, 64);
	[-4, 0, 4].forEach((x) => {
		const mesh = new THREE.Mesh(geo, new THREE.MeshStandardMaterial({ color: 0xcccccc, roughness: 0.3, metalness: 0.6 }));
		mesh.position.x = x;
		scene.add(mesh);
	});

	function animate() {
		requestAnimationFrame(animate);
		renderer.render(scene, camera);
	}
	animate();
} catch (e) {}
`,

	solution: `function createLightRig() {
	const ambient = new THREE.AmbientLight(0xffffff, 0.3);
	const directional = new THREE.DirectionalLight(0xffeedd, 2);
	directional.position.set(5, 8, 5);
	return { ambient, directional };
}

const { ambient, directional } = createLightRig();
console.log(ambient.type);
console.log(ambient.intensity);
console.log(directional.color.getHexString());
console.log(directional.position.x);

try {
	const renderer = new THREE.WebGLRenderer({ antialias: true });
	renderer.setSize(innerWidth, innerHeight);
	document.body.appendChild(renderer.domElement);
	const scene = new THREE.Scene();
	scene.background = new THREE.Color(0x050510);
	const camera = new THREE.PerspectiveCamera(60, innerWidth / innerHeight, 0.1, 100);
	camera.position.set(0, 2, 8);
	camera.lookAt(0, 0, 0);
	const { ambient: a, directional: d } = createLightRig();
	scene.add(a, d);
	[[0xff2200, -4, 0], [0x0044ff, 4, 0], [0x00ff88, 0, 3]].forEach(([color, x, z]) => {
		const pt = new THREE.PointLight(color, 3, 12); pt.position.set(x, 2, z); scene.add(pt);
	});
	const geo = new THREE.SphereGeometry(1, 64, 64);
	[-4, 0, 4].forEach((x) => {
		const mesh = new THREE.Mesh(geo, new THREE.MeshStandardMaterial({ color: 0xcccccc, roughness: 0.3, metalness: 0.6 }));
		mesh.position.x = x; scene.add(mesh);
	});
	function animate() { requestAnimationFrame(animate); renderer.render(scene, camera); }
	animate();
} catch (e) {}
`,

	tests: [
		{
			name: "ambient is AmbientLight",
			code: `{{FUNC}}
const { ambient } = createLightRig();
console.log(ambient.type);`,
			expected: "AmbientLight\n",
		},
		{
			name: "ambient intensity is 0.3",
			code: `{{FUNC}}
const { ambient } = createLightRig();
console.log(ambient.intensity);`,
			expected: "0.3\n",
		},
		{
			name: "directional color is ffeedd",
			code: `{{FUNC}}
const { directional } = createLightRig();
console.log(directional.color.getHexString());`,
			expected: "ffeedd\n",
		},
		{
			name: "directional position x=5",
			code: `{{FUNC}}
const { directional } = createLightRig();
console.log(directional.position.x);`,
			expected: "5\n",
		},
	],
};
