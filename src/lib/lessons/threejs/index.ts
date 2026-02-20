import type { Chapter } from "../types";
import { helloThreejs } from "./data/01-hello-threejs";
import { geometries } from "./data/02-geometries";
import { materials } from "./data/03-materials";
import { lights } from "./data/04-lights";
import { transformations } from "./data/05-transformations";
import { animation } from "./data/06-animation";
import { groups } from "./data/07-groups";
import { camera } from "./data/08-camera";
import { fog } from "./data/09-fog";
import { raycasting } from "./data/10-raycasting";
import { particles } from "./data/11-particles";
import { solarSystem } from "./data/12-solar-system";

export const threejsChapters: Chapter[] = [
	{ id: "the-scene", title: "The Scene" },
	{ id: "lights-and-shadows", title: "Lights & Shadows" },
	{ id: "animation-and-interaction", title: "Animation & Interaction" },
	{ id: "advanced", title: "Advanced" },
];

export const threejsLessons = [
	helloThreejs,
	geometries,
	materials,
	lights,
	transformations,
	animation,
	groups,
	camera,
	fog,
	raycasting,
	particles,
	solarSystem,
];
