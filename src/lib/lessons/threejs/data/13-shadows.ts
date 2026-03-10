import type { Lesson } from "../../types";

export const shadows: Lesson = {
	id: "shadows",
	title: "Shadows",
	chapterId: "lights-and-shadows",
	content: `## Shadows

Shadows add realism by grounding objects in the scene. Three.js uses **shadow maps** — the renderer draws the scene from each light's perspective to determine what is in shadow.

### Enabling Shadows

Shadows are off by default. You need to flip three switches:

1. **Renderer** — enable the shadow map engine:
\`\`\`js
renderer.shadowMap.enabled = true;
renderer.shadowMap.type = THREE.PCFSoftShadowMap; // softer edges
\`\`\`

2. **Light** — tell the light to cast shadows:
\`\`\`js
const light = new THREE.DirectionalLight(0xffffff, 2);
light.castShadow = true;
\`\`\`

3. **Objects** — each mesh decides whether it casts and/or receives:
\`\`\`js
cube.castShadow = true;    // this object blocks light
floor.receiveShadow = true; // shadows appear on this surface
\`\`\`

### Shadow Map Types

Three.js provides several shadow map algorithms:

| Type | Quality | Performance |
|------|---------|-------------|
| \`THREE.BasicShadowMap\` | Hard edges, blocky | Fastest |
| \`THREE.PCFShadowMap\` | Smoother edges (default) | Medium |
| \`THREE.PCFSoftShadowMap\` | Soft, natural look | Slower |
| \`THREE.VSMShadowMap\` | Very soft, some artifacts | Slower |

### Shadow Camera

Each shadow-casting light has a \`shadow.camera\` that controls the shadow frustum. For \`DirectionalLight\`, it is an orthographic camera:

\`\`\`js
light.shadow.camera.top = 10;
light.shadow.camera.bottom = -10;
light.shadow.camera.left = -10;
light.shadow.camera.right = 10;
light.shadow.camera.near = 0.5;
light.shadow.camera.far = 50;
\`\`\`

### Shadow Map Size

Higher resolution = sharper shadows but more GPU memory:

\`\`\`js
light.shadow.mapSize.width = 2048;
light.shadow.mapSize.height = 2048;
\`\`\`

### Which Lights Cast Shadows?

Only \`DirectionalLight\`, \`PointLight\`, and \`SpotLight\` can cast shadows. \`AmbientLight\` and \`HemisphereLight\` cannot.

### Your Task

Implement \`setupShadows(renderer, light, caster, receiver)\` that:
- Enables the shadow map on \`renderer\` with \`PCFSoftShadowMap\` type
- Sets \`light.castShadow\` to \`true\`
- Sets shadow map size to \`2048 x 2048\` on the light
- Sets \`caster.castShadow\` to \`true\`
- Sets \`receiver.receiveShadow\` to \`true\``,

	starterCode: `function setupShadows(renderer, light, caster, receiver) {
	// 1. Enable shadow map on renderer with PCFSoftShadowMap type

	// 2. Enable castShadow on the light

	// 3. Set shadow map size to 2048x2048

	// 4. Set caster.castShadow = true

	// 5. Set receiver.receiveShadow = true
}

// Test
const mockRenderer = { shadowMap: { enabled: false, type: null } };
const mockLight = { castShadow: false, shadow: { mapSize: { width: 512, height: 512 } } };
const mockCaster = { castShadow: false };
const mockReceiver = { receiveShadow: false };
setupShadows(mockRenderer, mockLight, mockCaster, mockReceiver);
console.log(mockRenderer.shadowMap.enabled);
console.log(mockRenderer.shadowMap.type === THREE.PCFSoftShadowMap);
console.log(mockLight.castShadow);
console.log(mockLight.shadow.mapSize.width);
console.log(mockCaster.castShadow);
console.log(mockReceiver.receiveShadow);

// Preview: cube casting shadow on a floor
try {
	const renderer = new THREE.WebGLRenderer({ antialias: true });
	renderer.setSize(innerWidth, innerHeight);
	document.body.appendChild(renderer.domElement);

	const scene = new THREE.Scene();
	scene.background = new THREE.Color(0x1a1a2e);

	const camera = new THREE.PerspectiveCamera(50, innerWidth / innerHeight, 0.1, 100);
	camera.position.set(4, 4, 6);
	camera.lookAt(0, 0, 0);

	const ambient = new THREE.AmbientLight(0xffffff, 0.3);
	scene.add(ambient);

	const dirLight = new THREE.DirectionalLight(0xffffff, 2);
	dirLight.position.set(5, 8, 5);
	scene.add(dirLight);

	const cube = new THREE.Mesh(
		new THREE.BoxGeometry(1.5, 1.5, 1.5),
		new THREE.MeshStandardMaterial({ color: 0x4488ff })
	);
	cube.position.y = 1;
	scene.add(cube);

	const floor = new THREE.Mesh(
		new THREE.PlaneGeometry(20, 20),
		new THREE.MeshStandardMaterial({ color: 0x444466 })
	);
	floor.rotation.x = -Math.PI / 2;
	scene.add(floor);

	setupShadows(renderer, dirLight, cube, floor);

	const clock = new THREE.Clock();
	function animate() {
		requestAnimationFrame(animate);
		const t = clock.getElapsedTime();
		cube.rotation.y = t * 0.5;
		cube.position.y = 1 + Math.sin(t) * 0.3;
		renderer.render(scene, camera);
	}
	animate();
} catch (e) {}
`,

	solution: `function setupShadows(renderer, light, caster, receiver) {
	renderer.shadowMap.enabled = true;
	renderer.shadowMap.type = THREE.PCFSoftShadowMap;
	light.castShadow = true;
	light.shadow.mapSize.width = 2048;
	light.shadow.mapSize.height = 2048;
	caster.castShadow = true;
	receiver.receiveShadow = true;
}

const mockRenderer = { shadowMap: { enabled: false, type: null } };
const mockLight = { castShadow: false, shadow: { mapSize: { width: 512, height: 512 } } };
const mockCaster = { castShadow: false };
const mockReceiver = { receiveShadow: false };
setupShadows(mockRenderer, mockLight, mockCaster, mockReceiver);
console.log(mockRenderer.shadowMap.enabled);
console.log(mockRenderer.shadowMap.type === THREE.PCFSoftShadowMap);
console.log(mockLight.castShadow);
console.log(mockLight.shadow.mapSize.width);
console.log(mockCaster.castShadow);
console.log(mockReceiver.receiveShadow);

try {
	const renderer = new THREE.WebGLRenderer({ antialias: true });
	renderer.setSize(innerWidth, innerHeight);
	document.body.appendChild(renderer.domElement);
	const scene = new THREE.Scene();
	scene.background = new THREE.Color(0x1a1a2e);
	const camera = new THREE.PerspectiveCamera(50, innerWidth / innerHeight, 0.1, 100);
	camera.position.set(4, 4, 6);
	camera.lookAt(0, 0, 0);
	scene.add(new THREE.AmbientLight(0xffffff, 0.3));
	const dirLight = new THREE.DirectionalLight(0xffffff, 2);
	dirLight.position.set(5, 8, 5);
	scene.add(dirLight);
	const cube = new THREE.Mesh(new THREE.BoxGeometry(1.5, 1.5, 1.5), new THREE.MeshStandardMaterial({ color: 0x4488ff }));
	cube.position.y = 1;
	scene.add(cube);
	const floor = new THREE.Mesh(new THREE.PlaneGeometry(20, 20), new THREE.MeshStandardMaterial({ color: 0x444466 }));
	floor.rotation.x = -Math.PI / 2;
	scene.add(floor);
	setupShadows(renderer, dirLight, cube, floor);
	const clock = new THREE.Clock();
	function animate() { requestAnimationFrame(animate); const t = clock.getElapsedTime(); cube.rotation.y = t * 0.5; cube.position.y = 1 + Math.sin(t) * 0.3; renderer.render(scene, camera); }
	animate();
} catch (e) {}
`,

	tests: [
		{
			name: "renderer shadowMap is enabled",
			code: `{{FUNC}}
const r = { shadowMap: { enabled: false, type: null } };
const l = { castShadow: false, shadow: { mapSize: { width: 512, height: 512 } } };
const c = { castShadow: false };
const v = { receiveShadow: false };
setupShadows(r, l, c, v);
console.log(r.shadowMap.enabled);`,
			expected: "true\n",
		},
		{
			name: "renderer uses PCFSoftShadowMap",
			code: `{{FUNC}}
const r = { shadowMap: { enabled: false, type: null } };
const l = { castShadow: false, shadow: { mapSize: { width: 512, height: 512 } } };
const c = { castShadow: false };
const v = { receiveShadow: false };
setupShadows(r, l, c, v);
console.log(r.shadowMap.type === THREE.PCFSoftShadowMap);`,
			expected: "true\n",
		},
		{
			name: "light castShadow is true",
			code: `{{FUNC}}
const r = { shadowMap: { enabled: false, type: null } };
const l = { castShadow: false, shadow: { mapSize: { width: 512, height: 512 } } };
const c = { castShadow: false };
const v = { receiveShadow: false };
setupShadows(r, l, c, v);
console.log(l.castShadow);`,
			expected: "true\n",
		},
		{
			name: "shadow map size is 2048",
			code: `{{FUNC}}
const r = { shadowMap: { enabled: false, type: null } };
const l = { castShadow: false, shadow: { mapSize: { width: 512, height: 512 } } };
const c = { castShadow: false };
const v = { receiveShadow: false };
setupShadows(r, l, c, v);
console.log(l.shadow.mapSize.width);
console.log(l.shadow.mapSize.height);`,
			expected: "2048\n2048\n",
		},
		{
			name: "caster castShadow is true",
			code: `{{FUNC}}
const r = { shadowMap: { enabled: false, type: null } };
const l = { castShadow: false, shadow: { mapSize: { width: 512, height: 512 } } };
const c = { castShadow: false };
const v = { receiveShadow: false };
setupShadows(r, l, c, v);
console.log(c.castShadow);`,
			expected: "true\n",
		},
		{
			name: "receiver receiveShadow is true",
			code: `{{FUNC}}
const r = { shadowMap: { enabled: false, type: null } };
const l = { castShadow: false, shadow: { mapSize: { width: 512, height: 512 } } };
const c = { castShadow: false };
const v = { receiveShadow: false };
setupShadows(r, l, c, v);
console.log(v.receiveShadow);`,
			expected: "true\n",
		},
	],
};
