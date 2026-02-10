"use client";

import { useEffect } from "react";
import { useRouter } from "next/navigation";
import { getCurrentLessonId } from "@/lib/progress";
import { lessons } from "@/lib/lessons";

export default function Page() {
  const router = useRouter();

  useEffect(() => {
    const lastLesson = getCurrentLessonId();
    const target =
      lastLesson && lessons.some((l) => l.id === lastLesson)
        ? lastLesson
        : lessons[0].id;
    router.replace(`/lessons/${target}`);
  }, [router]);

  return null;
}
