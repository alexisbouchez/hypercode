const STORAGE_KEY = "hypercode-progress";

interface Progress {
  completedLessons: string[];
  currentLessonId: string | null;
  savedCode: Record<string, string>;
}

function getDefaultProgress(): Progress {
  return {
    completedLessons: [],
    currentLessonId: null,
    savedCode: {},
  };
}

export function loadProgress(): Progress {
  if (typeof window === "undefined") return getDefaultProgress();
  try {
    const raw = localStorage.getItem(STORAGE_KEY);
    if (!raw) return getDefaultProgress();
    return JSON.parse(raw) as Progress;
  } catch {
    return getDefaultProgress();
  }
}

function save(progress: Progress) {
  if (typeof window === "undefined") return;
  localStorage.setItem(STORAGE_KEY, JSON.stringify(progress));
}

export function markCompleted(lessonId: string) {
  const progress = loadProgress();
  if (!progress.completedLessons.includes(lessonId)) {
    progress.completedLessons.push(lessonId);
  }
  save(progress);
}

export function isCompleted(lessonId: string): boolean {
  return loadProgress().completedLessons.includes(lessonId);
}

export function setCurrentLesson(lessonId: string) {
  const progress = loadProgress();
  progress.currentLessonId = lessonId;
  save(progress);
}

export function getCurrentLessonId(): string | null {
  return loadProgress().currentLessonId;
}

export function saveCode(lessonId: string, code: string) {
  const progress = loadProgress();
  progress.savedCode[lessonId] = code;
  save(progress);
}

export function getSavedCode(lessonId: string): string | null {
  return loadProgress().savedCode[lessonId] ?? null;
}

export function getCompletedCount(): number {
  return loadProgress().completedLessons.length;
}

export function resetProgress() {
  if (typeof window === "undefined") return;
  localStorage.removeItem(STORAGE_KEY);
}
