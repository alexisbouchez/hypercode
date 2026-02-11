"use client";

import { useEffect } from "react";
import { useRouter } from "next/navigation";
import { getCurrentLessonId } from "@/lib/progress";
import { lessons } from "@/lib/lessons";

export default function Page() {
  const router = useRouter();

  useEffect(() => {
    const lastLesson = getCurrentLessonId();
    if (lastLesson && lessons.some((l) => l.id === lastLesson)) {
      router.replace(`/lessons/${lastLesson}`);
    } else {
      router.replace("/introduction");
    }
  }, [router]);

  return null;
}
