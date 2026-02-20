import type { Lesson } from "../../types";

export const solarSystem: Lesson = {
	id: "solar-system",
	title: "Solar System",
	chapterId: "advanced",
	content: `## Final Project: Solar System

You've learned all the building blocks. Now combine them into a small solar system:

- **Scene** with dark background and fog
- **Sun** at the center with an emissive material (glows without light)
- **Planets** orbiting at different radii using Groups
- **Lights** from the sun (PointLight)
- **Stars** particle system in the background
- **Animation** with delta time for frame-rate independence

### Emissive Materials

\`MeshStandardMaterial\` can self-illuminate with \`emissive\` and \`emissiveIntensity\`:

\`\`\`js
new THREE.MeshStandardMaterial({
  color: 0xffaa00,
  emissive: 0xff6600,
  emissiveIntensity: 1.5
})
\`\`\`

### PointLight with Distance

A \`PointLight\` from the sun illuminates planets:

\`\`\`js
const sunLight = new THREE.PointLight(0xfff4cc, 3, 100);
sunLight.position.set(0, 0, 0);
scene.add(sunLight);
\`\`\`

### Orbit Groups Pattern

Each planet lives in its own orbit group. Rotating the group orbits the planet:

\`\`\`js
const orbit = new THREE.Group();
const planet = new THREE.Mesh(geo, mat);
planet.position.x = orbitRadius;
orbit.add(planet);
scene.add(orbit);

// In animation loop:
orbit.rotation.y += delta * orbitSpeed;
\`\`\`

### Your Task

Implement \`createPlanet(radius, color, orbitRadius)\` that:
- Creates a sphere mesh with \`MeshStandardMaterial\` of the given color
- Wraps it in an orbit \`Group\` with the planet at \`(orbitRadius, 0, 0)\`
- Returns \`{ mesh, orbit }\``,

	starterCode: `function createPlanet(radius, color, orbitRadius) {
	const geo = new THREE.SphereGeometry(radius, 32, 32);
	const mat = new THREE.MeshStandardMaterial({ color });
	const mesh = new THREE.Mesh(geo, mat);
	const orbit = new THREE.Group();
	// Position the mesh at (orbitRadius, 0, 0) and add it to orbit
	return { mesh, orbit };
}

const { mesh, orbit } = createPlanet(0.5, 0x4488ff, 4);
console.log(mesh.geometry.parameters.radius);
console.log(mesh.position.x);
console.log(orbit.children.length);

// Preview: full solar system
try {
	const renderer = new THREE.WebGLRenderer({ antialias: true });
	renderer.setSize(innerWidth, innerHeight);
	document.body.appendChild(renderer.domElement);

	const scene = new THREE.Scene();
	scene.background = new THREE.Color(0x000005);
	scene.fog = new THREE.FogExp2(0x000005, 0.008);

	const camera = new THREE.PerspectiveCamera(60, innerWidth / innerHeight, 0.1, 500);
	camera.position.set(0, 12, 22);
	camera.lookAt(0, 0, 0);

	// Ambient + sun light
	scene.add(new THREE.AmbientLight(0x111122, 0.5));
	const sunLight = new THREE.PointLight(0xfff4cc, 3, 80);
	scene.add(sunLight);

	// Sun
	const sun = new THREE.Mesh(
		new THREE.SphereGeometry(2, 64, 64),
		new THREE.MeshStandardMaterial({ color: 0xffaa00, emissive: 0xff6600, emissiveIntensity: 1.5 })
	);
	scene.add(sun);

	// Planets
	const planetData = [
		{ radius: 0.3, color: 0xaaaaaa, orbitRadius: 4,  speed: 2.0 },
		{ radius: 0.5, color: 0xddaa66, orbitRadius: 7,  speed: 1.2 },
		{ radius: 0.55, color: 0x4488ff, orbitRadius: 10, speed: 0.8 },
		{ radius: 0.35, color: 0xff4422, orbitRadius: 13, speed: 0.5 },
		{ radius: 1.2, color: 0xddcc88, orbitRadius: 18, speed: 0.25 },
	];

	const orbits = planetData.map(({ radius, color, orbitRadius, speed }) => {
		const { orbit, mesh } = createPlanet(radius, color, orbitRadius);
		scene.add(orbit);
		return { orbit, speed, mesh };
	});

	// Stars
	const starPositions = new Float32Array(6000 * 3);
	for (let i = 0; i < 6000; i++) {
		const r = 100 + Math.random() * 200;
		const theta = Math.random() * Math.PI * 2;
		const phi = Math.acos(2 * Math.random() - 1);
		starPositions[i * 3]     = r * Math.sin(phi) * Math.cos(theta);
		starPositions[i * 3 + 1] = r * Math.sin(phi) * Math.sin(theta);
		starPositions[i * 3 + 2] = r * Math.cos(phi);
	}
	const starGeo = new THREE.BufferGeometry();
	starGeo.setAttribute('position', new THREE.BufferAttribute(starPositions, 3));
	scene.add(new THREE.Points(starGeo, new THREE.PointsMaterial({ color: 0xffffff, size: 0.3, sizeAttenuation: true })));

	const clock = new THREE.Clock();
	function animate() {
		requestAnimationFrame(animate);
		const delta = clock.getDelta();
		sun.rotation.y += delta * 0.3;
		orbits.forEach(({ orbit, speed, mesh }) => {
			orbit.rotation.y += delta * speed * 0.4;
			mesh.rotation.y += delta * 1.5;
		});
		renderer.render(scene, camera);
	}
	animate();
} catch (e) {}
`,

	solution: `function createPlanet(radius, color, orbitRadius) {
	const geo = new THREE.SphereGeometry(radius, 32, 32);
	const mat = new THREE.MeshStandardMaterial({ color });
	const mesh = new THREE.Mesh(geo, mat);
	const orbit = new THREE.Group();
	mesh.position.set(orbitRadius, 0, 0);
	orbit.add(mesh);
	return { mesh, orbit };
}

const { mesh, orbit } = createPlanet(0.5, 0x4488ff, 4);
console.log(mesh.geometry.parameters.radius);
console.log(mesh.position.x);
console.log(orbit.children.length);

try {
	const renderer = new THREE.WebGLRenderer({ antialias: true });
	renderer.setSize(innerWidth, innerHeight);
	document.body.appendChild(renderer.domElement);
	const scene = new THREE.Scene();
	scene.background = new THREE.Color(0x000005);
	scene.fog = new THREE.FogExp2(0x000005, 0.008);
	const camera = new THREE.PerspectiveCamera(60, innerWidth / innerHeight, 0.1, 500);
	camera.position.set(0, 12, 22);
	camera.lookAt(0, 0, 0);
	scene.add(new THREE.AmbientLight(0x111122, 0.5));
	const sunLight = new THREE.PointLight(0xfff4cc, 3, 80);
	scene.add(sunLight);
	const sun = new THREE.Mesh(new THREE.SphereGeometry(2, 64, 64), new THREE.MeshStandardMaterial({ color: 0xffaa00, emissive: 0xff6600, emissiveIntensity: 1.5 }));
	scene.add(sun);
	const planetData = [
		{ radius: 0.3, color: 0xaaaaaa, orbitRadius: 4, speed: 2.0 },
		{ radius: 0.5, color: 0xddaa66, orbitRadius: 7, speed: 1.2 },
		{ radius: 0.55, color: 0x4488ff, orbitRadius: 10, speed: 0.8 },
		{ radius: 0.35, color: 0xff4422, orbitRadius: 13, speed: 0.5 },
		{ radius: 1.2, color: 0xddcc88, orbitRadius: 18, speed: 0.25 },
	];
	const orbits = planetData.map(({ radius, color, orbitRadius, speed }) => {
		const { orbit, mesh } = createPlanet(radius, color, orbitRadius);
		scene.add(orbit);
		return { orbit, speed, mesh };
	});
	const starPositions = new Float32Array(6000 * 3);
	for (let i = 0; i < 6000; i++) { const r = 100 + Math.random() * 200; const theta = Math.random() * Math.PI * 2; const phi = Math.acos(2 * Math.random() - 1); starPositions[i*3]=r*Math.sin(phi)*Math.cos(theta); starPositions[i*3+1]=r*Math.sin(phi)*Math.sin(theta); starPositions[i*3+2]=r*Math.cos(phi); }
	const starGeo = new THREE.BufferGeometry();
	starGeo.setAttribute('position', new THREE.BufferAttribute(starPositions, 3));
	scene.add(new THREE.Points(starGeo, new THREE.PointsMaterial({ color: 0xffffff, size: 0.3, sizeAttenuation: true })));
	const clock = new THREE.Clock();
	function animate() { requestAnimationFrame(animate); const delta = clock.getDelta(); sun.rotation.y += delta * 0.3; orbits.forEach(({ orbit, speed, mesh }) => { orbit.rotation.y += delta * speed * 0.4; mesh.rotation.y += delta * 1.5; }); renderer.render(scene, camera); }
	animate();
} catch (e) {}
`,

	tests: [
		{
			name: "planet radius is set",
			code: `{{FUNC}}
const { mesh } = createPlanet(0.5, 0x4488ff, 4);
console.log(mesh.geometry.parameters.radius);`,
			expected: "0.5\n",
		},
		{
			name: "planet position.x equals orbitRadius",
			code: `{{FUNC}}
const { mesh } = createPlanet(0.5, 0x4488ff, 4);
console.log(mesh.position.x);`,
			expected: "4\n",
		},
		{
			name: "orbit contains the planet",
			code: `{{FUNC}}
const { mesh, orbit } = createPlanet(0.5, 0x4488ff, 4);
console.log(orbit.children.includes(mesh));`,
			expected: "true\n",
		},
		{
			name: "planet material color",
			code: `{{FUNC}}
const { mesh } = createPlanet(0.5, 0x4488ff, 4);
console.log(mesh.material.color.getHexString());`,
			expected: "4488ff\n",
		},
	],
};
