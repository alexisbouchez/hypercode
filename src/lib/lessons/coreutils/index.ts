import type { Chapter, Lesson } from "../types";
import { echo } from "./data/01-echo";
import { cat } from "./data/02-cat";
import { rev } from "./data/03-rev";
import { wcC } from "./data/04-wc-c";
import { wcL } from "./data/05-wc-l";
import { wcW } from "./data/06-wc-w";
import { head } from "./data/07-head";
import { tail } from "./data/08-tail";
import { grep } from "./data/09-grep";
import { toupper } from "./data/10-toupper";
import { tr } from "./data/11-tr";
import { uniq } from "./data/12-uniq";
import { tac } from "./data/13-tac";

export const coreutilsChapters: Chapter[] = [
	{ id: "output", title: "Output" },
	{ id: "counting", title: "Counting" },
	{ id: "filtering", title: "Filtering" },
	{ id: "transformation", title: "Transformation" },
];

export const coreutilsLessons: Lesson[] = [
	echo,
	cat,
	rev,
	wcC,
	wcL,
	wcW,
	head,
	tail,
	grep,
	toupper,
	tr,
	uniq,
	tac,
];
