"use client";

import { useEffect } from "react";
import { useRouter, useParams } from "next/navigation";
import { getCurrentLessonId } from "@/lib/progress";
import { getCourse } from "@/lib/courses";

export default function CoursePage() {
  const router = useRouter();
  const params = useParams();
  const courseId = params.courseId as string;

  useEffect(() => {
    const course = getCourse(courseId);
    if (!course) {
      router.replace("/");
      return;
    }

    const lastLesson = getCurrentLessonId(courseId);
    if (lastLesson && course.lessons.some((l) => l.id === lastLesson)) {
      router.replace(`/${courseId}/lessons/${lastLesson}`);
    } else {
      router.replace(`/${courseId}/introduction`);
    }
  }, [router, courseId]);

  return null;
}
