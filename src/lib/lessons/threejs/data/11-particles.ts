import type { Lesson } from "../../types";

export const particles: Lesson = {
	id: "particles",
	title: "Particles",
	chapterId: "advanced",
	content: `## Particles

Three.js can render thousands of points efficiently using \`Points\` with a \`BufferGeometry\`.

### BufferGeometry + Float32Array

Particle positions are stored in a typed array and uploaded to the GPU:

\`\`\`js
const count = 1000;
const positions = new Float32Array(count * 3); // x,y,z per particle

for (let i = 0; i < count; i++) {
  positions[i * 3]     = (Math.random() - 0.5) * 20; // x
  positions[i * 3 + 1] = (Math.random() - 0.5) * 20; // y
  positions[i * 3 + 2] = (Math.random() - 0.5) * 20; // z
}

const geo = new THREE.BufferGeometry();
geo.setAttribute('position', new THREE.BufferAttribute(positions, 3));
\`\`\`

### PointsMaterial

\`\`\`js
const mat = new THREE.PointsMaterial({
  color: 0x88ccff,
  size: 0.1,
  sizeAttenuation: true, // size scales with distance
});
\`\`\`

### Creating the Points Object

\`\`\`js
const particles = new THREE.Points(geo, mat);
scene.add(particles);
\`\`\`

### Animating Particles

You can update positions each frame by modifying the array and flagging it dirty:

\`\`\`js
geo.attributes.position.needsUpdate = true;
\`\`\`

### Your Task

Implement \`createParticleSystem(count, spread, size, color)\` that:
- Creates a \`BufferGeometry\` with \`count\` particles, each randomly positioned in a cube of side \`spread\` centered at origin
- Uses \`PointsMaterial\` with the given \`size\` and \`color\`
- Returns the \`Points\` object`,

	starterCode: `function createParticleSystem(count, spread, size, color) {
	const positions = new Float32Array(count * 3);
	for (let i = 0; i < count; i++) {
		// Set positions[i*3], positions[i*3+1], positions[i*3+2]
		// to random values in range [-spread/2, spread/2]
	}
	const geo = new THREE.BufferGeometry();
	// setAttribute 'position' with a BufferAttribute
	const mat = new THREE.PointsMaterial({ size, color });
	return new THREE.Points(geo, mat);
}

const ps = createParticleSystem(500, 10, 0.1, 0x88ccff);
console.log(ps.type);
console.log(ps.geometry.attributes.position.count);
console.log(ps.material.size);

// Preview: animated star field
try {
	const renderer = new THREE.WebGLRenderer({ antialias: true });
	renderer.setSize(innerWidth, innerHeight);
	document.body.appendChild(renderer.domElement);

	const scene = new THREE.Scene();
	scene.background = new THREE.Color(0x000005);

	const camera = new THREE.PerspectiveCamera(75, innerWidth / innerHeight, 0.1, 100);
	camera.position.set(0, 0, 5);

	const stars = createParticleSystem(5000, 80, 0.08, 0xffffff);
	scene.add(stars);

	const nebula = createParticleSystem(2000, 30, 0.12, 0x4466ff);
	nebula.position.set(0, 0, -20);
	scene.add(nebula);

	function animate() {
		requestAnimationFrame(animate);
		stars.rotation.y += 0.0002;
		stars.rotation.x += 0.0001;
		renderer.render(scene, camera);
	}
	animate();
} catch (e) {}
`,

	solution: `function createParticleSystem(count, spread, size, color) {
	const positions = new Float32Array(count * 3);
	for (let i = 0; i < count; i++) {
		positions[i * 3]     = (Math.random() - 0.5) * spread;
		positions[i * 3 + 1] = (Math.random() - 0.5) * spread;
		positions[i * 3 + 2] = (Math.random() - 0.5) * spread;
	}
	const geo = new THREE.BufferGeometry();
	geo.setAttribute('position', new THREE.BufferAttribute(positions, 3));
	const mat = new THREE.PointsMaterial({ size, color });
	return new THREE.Points(geo, mat);
}

const ps = createParticleSystem(500, 10, 0.1, 0x88ccff);
console.log(ps.type);
console.log(ps.geometry.attributes.position.count);
console.log(ps.material.size);

try {
	const renderer = new THREE.WebGLRenderer({ antialias: true });
	renderer.setSize(innerWidth, innerHeight);
	document.body.appendChild(renderer.domElement);
	const scene = new THREE.Scene();
	scene.background = new THREE.Color(0x000005);
	const camera = new THREE.PerspectiveCamera(75, innerWidth / innerHeight, 0.1, 100);
	camera.position.set(0, 0, 5);
	const stars = createParticleSystem(5000, 80, 0.08, 0xffffff);
	scene.add(stars);
	const nebula = createParticleSystem(2000, 30, 0.12, 0x4466ff);
	nebula.position.set(0, 0, -20);
	scene.add(nebula);
	function animate() { requestAnimationFrame(animate); stars.rotation.y += 0.0002; stars.rotation.x += 0.0001; renderer.render(scene, camera); }
	animate();
} catch (e) {}
`,

	tests: [
		{
			name: "returns Points object",
			code: `{{FUNC}}
const ps = createParticleSystem(100, 10, 0.1, 0xffffff);
console.log(ps.type);`,
			expected: "Points\n",
		},
		{
			name: "position count equals input count",
			code: `{{FUNC}}
const ps = createParticleSystem(500, 10, 0.1, 0xffffff);
console.log(ps.geometry.attributes.position.count);`,
			expected: "500\n",
		},
		{
			name: "material size is set",
			code: `{{FUNC}}
const ps = createParticleSystem(100, 10, 0.05, 0xffffff);
console.log(ps.material.size);`,
			expected: "0.05\n",
		},
		{
			name: "material color is set",
			code: `{{FUNC}}
const ps = createParticleSystem(100, 10, 0.1, 0x88ccff);
console.log(ps.material.color.getHexString());`,
			expected: "88ccff\n",
		},
	],
};
