import type { Lesson } from "../../types";

export const groups: Lesson = {
	id: "groups",
	title: "Groups & Hierarchy",
	chapterId: "animation-and-interaction",
	content: `## Groups & Hierarchy

Three.js lets you nest objects in a **scene graph** — a tree where parent transforms propagate to children.

### Group

\`THREE.Group\` is an invisible container. Children inherit their parent's position, rotation, and scale.

\`\`\`js
const group = new THREE.Group();
group.add(meshA);
group.add(meshB);
scene.add(group);

// Rotating the group rotates all children together
group.rotation.y += 0.01;
\`\`\`

### Scene Graph Rules

- Child transforms are **relative** to the parent
- A child at position \`(1, 0, 0)\` inside a group at \`(3, 0, 0)\` is at world position \`(4, 0, 0)\`
- Scaling a parent scales all children

\`\`\`js
const arm = new THREE.Group();
arm.position.set(2, 0, 0); // offset from parent

const hand = new THREE.Mesh(geo, mat);
hand.position.set(1, 0, 0); // offset from arm
arm.add(hand);
\`\`\`

### add vs scene.add

Any \`Object3D\` (including \`Mesh\`, \`Group\`, \`Light\`) can be a parent:

\`\`\`js
parent.add(child); // child becomes relative to parent
scene.add(parent); // top-level in scene
\`\`\`

### Your Task

Implement \`buildSolarSystem()\` that returns a \`Group\` containing:
- A center sphere (the "sun") of radius \`1\` with \`MeshNormalMaterial\` at position \`(0, 0, 0)\`
- An orbit \`Group\` with a smaller sphere of radius \`0.4\` at position \`(3, 0, 0)\` (the "planet")

Return \`{ sun, orbitGroup, planet }\`.`,

	starterCode: `function buildSolarSystem() {
	const sunGeo = new THREE.SphereGeometry(1, 32, 32);
	const sun = new THREE.Mesh(sunGeo, new THREE.MeshNormalMaterial());

	const orbitGroup = new THREE.Group();

	const planetGeo = new THREE.SphereGeometry(0.4, 32, 32);
	const planet = new THREE.Mesh(planetGeo, new THREE.MeshNormalMaterial());
	// Position the planet at (3, 0, 0) within the orbit group
	// Add the planet to the orbit group

	return { sun, orbitGroup, planet };
}

const { sun, orbitGroup, planet } = buildSolarSystem();
console.log(sun.geometry.parameters.radius);
console.log(planet.geometry.parameters.radius);
console.log(planet.position.x);

// Preview: orbiting planet
try {
	const renderer = new THREE.WebGLRenderer({ antialias: true });
	renderer.setSize(innerWidth, innerHeight);
	document.body.appendChild(renderer.domElement);

	const scene = new THREE.Scene();
	scene.background = new THREE.Color(0x000008);

	const camera = new THREE.PerspectiveCamera(60, innerWidth / innerHeight, 0.1, 100);
	camera.position.set(0, 5, 10);
	camera.lookAt(0, 0, 0);

	const { sun: s, orbitGroup: og, planet: p } = buildSolarSystem();
	scene.add(s);
	scene.add(og);

	const clock = new THREE.Clock();
	function animate() {
		requestAnimationFrame(animate);
		og.rotation.y = clock.getElapsedTime() * 0.8;
		s.rotation.y += 0.005;
		renderer.render(scene, camera);
	}
	animate();
} catch (e) {}
`,

	solution: `function buildSolarSystem() {
	const sunGeo = new THREE.SphereGeometry(1, 32, 32);
	const sun = new THREE.Mesh(sunGeo, new THREE.MeshNormalMaterial());

	const orbitGroup = new THREE.Group();

	const planetGeo = new THREE.SphereGeometry(0.4, 32, 32);
	const planet = new THREE.Mesh(planetGeo, new THREE.MeshNormalMaterial());
	planet.position.set(3, 0, 0);
	orbitGroup.add(planet);

	return { sun, orbitGroup, planet };
}

const { sun, orbitGroup, planet } = buildSolarSystem();
console.log(sun.geometry.parameters.radius);
console.log(planet.geometry.parameters.radius);
console.log(planet.position.x);

try {
	const renderer = new THREE.WebGLRenderer({ antialias: true });
	renderer.setSize(innerWidth, innerHeight);
	document.body.appendChild(renderer.domElement);
	const scene = new THREE.Scene();
	scene.background = new THREE.Color(0x000008);
	const camera = new THREE.PerspectiveCamera(60, innerWidth / innerHeight, 0.1, 100);
	camera.position.set(0, 5, 10);
	camera.lookAt(0, 0, 0);
	const { sun: s, orbitGroup: og } = buildSolarSystem();
	scene.add(s); scene.add(og);
	const clock = new THREE.Clock();
	function animate() { requestAnimationFrame(animate); og.rotation.y = clock.getElapsedTime() * 0.8; s.rotation.y += 0.005; renderer.render(scene, camera); }
	animate();
} catch (e) {}
`,

	tests: [
		{
			name: "sun radius is 1",
			code: `{{FUNC}}
const { sun } = buildSolarSystem();
console.log(sun.geometry.parameters.radius);`,
			expected: "1\n",
		},
		{
			name: "planet radius is 0.4",
			code: `{{FUNC}}
const { planet } = buildSolarSystem();
console.log(planet.geometry.parameters.radius);`,
			expected: "0.4\n",
		},
		{
			name: "planet position.x is 3",
			code: `{{FUNC}}
const { planet } = buildSolarSystem();
console.log(planet.position.x);`,
			expected: "3\n",
		},
		{
			name: "orbitGroup contains planet",
			code: `{{FUNC}}
const { orbitGroup, planet } = buildSolarSystem();
console.log(orbitGroup.children.includes(planet));`,
			expected: "true\n",
		},
	],
};
