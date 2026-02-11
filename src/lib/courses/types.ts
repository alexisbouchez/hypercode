import type { Chapter, Lesson } from "@/lib/lessons/types";

export interface Course {
  id: string;
  title: string;
  description: string;
  language: string;
  chapters: Chapter[];
  lessons: Lesson[];
  pdfPath?: string;
  runtimeLabel: string;
  introductionContent: string;
  whatsNextContent: string;
}
