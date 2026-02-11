import { notFound } from "next/navigation";
import type { Metadata } from "next";
import { courses, getCourse } from "@/lib/courses";
import { ContentShell } from "@/components/content-shell";

interface WhatsNextPageProps {
  params: Promise<{ courseId: string }>;
}

export function generateStaticParams() {
  return courses.map((c) => ({ courseId: c.id }));
}

export async function generateMetadata({
  params,
}: WhatsNextPageProps): Promise<Metadata> {
  const { courseId } = await params;
  const course = getCourse(courseId);
  if (!course) return {};

  return {
    title: `What's Next? - ${course.title} - Hypercode`,
    description: `You have completed the ${course.title} course. Here are next steps and resources.`,
  };
}

export default async function WhatsNextPage({ params }: WhatsNextPageProps) {
  const { courseId } = await params;
  const course = getCourse(courseId);
  if (!course) notFound();

  const lastLesson = course.lessons[course.lessons.length - 1];

  return (
    <ContentShell
      courseId={course.id}
      chapters={course.chapters}
      lessons={course.lessons}
      pdfPath={course.pdfPath}
      title="What's Next?"
      content={course.whatsNextContent}
      activePage="whats-next"
      prevHref={`/${course.id}/lessons/${lastLesson.id}`}
    />
  );
}
