import type { Chapter, Lesson } from "../types";
import { echo } from "./data/01-echo";
import { pwd } from "./data/02-pwd";
import { ls } from "./data/03-ls";
import { cd } from "./data/04-cd";
import { cat } from "./data/05-cat";
import { mkdir } from "./data/06-mkdir";
import { touch } from "./data/07-touch";
import { cp } from "./data/08-cp";
import { mv } from "./data/09-mv";
import { rm } from "./data/10-rm";
import { head } from "./data/11-head";
import { grep } from "./data/12-grep";
import { pipes } from "./data/13-pipes";
import { wc } from "./data/14-wc";
import { variables } from "./data/15-variables";
import { loops } from "./data/16-loops";
import { conditionals } from "./data/17-conditionals";

export const linuxChapters: Chapter[] = [
  { id: "the-shell", title: "The Shell" },
  { id: "working-with-files", title: "Working with Files" },
  { id: "text-processing", title: "Text Processing" },
  { id: "shell-scripting", title: "Shell Scripting" },
];

export const linuxLessons: Lesson[] = [
  echo,
  pwd,
  ls,
  cd,
  cat,
  mkdir,
  touch,
  cp,
  mv,
  rm,
  head,
  grep,
  pipes,
  wc,
  variables,
  loops,
  conditionals,
];
