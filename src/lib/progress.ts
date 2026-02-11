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

function storageKey(courseId: string): string {
  return `hypercode-progress-${courseId}`;
}

/** Migrate legacy "hypercode-progress" to "hypercode-progress-go" once. */
function migrateIfNeeded() {
  if (typeof window === "undefined") return;
  const OLD_KEY = "hypercode-progress";
  const NEW_KEY = storageKey("go");
  try {
    const old = localStorage.getItem(OLD_KEY);
    if (old && !localStorage.getItem(NEW_KEY)) {
      localStorage.setItem(NEW_KEY, old);
      localStorage.removeItem(OLD_KEY);
    }
  } catch {
    // ignore
  }
}

let migrated = false;

function ensureMigrated() {
  if (!migrated) {
    migrateIfNeeded();
    migrated = true;
  }
}

export function loadProgress(courseId: string): Progress {
  if (typeof window === "undefined") return getDefaultProgress();
  ensureMigrated();
  try {
    const raw = localStorage.getItem(storageKey(courseId));
    if (!raw) return getDefaultProgress();
    return JSON.parse(raw) as Progress;
  } catch {
    return getDefaultProgress();
  }
}

function save(courseId: string, progress: Progress) {
  if (typeof window === "undefined") return;
  localStorage.setItem(storageKey(courseId), JSON.stringify(progress));
}

export function markCompleted(courseId: string, lessonId: string) {
  const progress = loadProgress(courseId);
  if (!progress.completedLessons.includes(lessonId)) {
    progress.completedLessons.push(lessonId);
  }
  save(courseId, progress);
}

export function unmarkCompleted(courseId: string, lessonId: string) {
  const progress = loadProgress(courseId);
  progress.completedLessons = progress.completedLessons.filter(
    (id) => id !== lessonId,
  );
  save(courseId, progress);
}

export function isCompleted(courseId: string, lessonId: string): boolean {
  return loadProgress(courseId).completedLessons.includes(lessonId);
}

export function setCurrentLesson(courseId: string, lessonId: string) {
  const progress = loadProgress(courseId);
  progress.currentLessonId = lessonId;
  save(courseId, progress);
}

export function getCurrentLessonId(courseId: string): string | null {
  return loadProgress(courseId).currentLessonId;
}

export function saveCode(courseId: string, lessonId: string, code: string) {
  const progress = loadProgress(courseId);
  progress.savedCode[lessonId] = code;
  save(courseId, progress);
}

export function getSavedCode(courseId: string, lessonId: string): string | null {
  return loadProgress(courseId).savedCode[lessonId] ?? null;
}

export function getCompletedCount(courseId: string): number {
  return loadProgress(courseId).completedLessons.length;
}

export function resetProgress(courseId: string) {
  if (typeof window === "undefined") return;
  localStorage.removeItem(storageKey(courseId));
}
