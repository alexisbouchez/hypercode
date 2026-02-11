import { notFound } from "next/navigation";
import type { Metadata } from "next";
import { courses, getCourse } from "@/lib/courses";
import { LessonShellWrapper } from "@/components/lesson-shell-wrapper";

interface LessonPageProps {
  params: Promise<{ courseId: string; slug: string }>;
}

export function generateStaticParams() {
  return courses.flatMap((course) =>
    course.lessons.map((lesson) => ({
      courseId: course.id,
      slug: lesson.id,
    })),
  );
}

export async function generateMetadata({
  params,
}: LessonPageProps): Promise<Metadata> {
  const { courseId, slug } = await params;
  const course = getCourse(courseId);
  if (!course) return {};

  const lesson = course.lessons.find((l) => l.id === slug);
  if (!lesson) return {};

  const index = course.lessons.indexOf(lesson);
  const title = `${lesson.title} - ${course.title} - Hypercode`;
  const description = `Lesson ${index + 1} of ${course.lessons.length}: ${lesson.title}. Learn ${course.title} interactively - no account needed.`;
  const url = `https://hypercode.alexisbouchez.com/${courseId}/lessons/${slug}`;

  return {
    title,
    description,
    openGraph: {
      title,
      description,
      url,
      type: "article",
    },
    twitter: {
      title,
      description,
    },
    alternates: {
      canonical: url,
    },
  };
}

export default async function LessonPage({ params }: LessonPageProps) {
  const { courseId, slug } = await params;
  const course = getCourse(courseId);
  if (!course) notFound();

  const lesson = course.lessons.find((l) => l.id === slug);
  if (!lesson) notFound();

  return (
    <LessonShellWrapper
      courseId={course.id}
      language={course.language}
      runtimeLabel={course.runtimeLabel}
      pdfPath={course.pdfPath}
      lesson={lesson}
      lessons={course.lessons}
      chapters={course.chapters}
    />
  );
}
