import { notFound } from "next/navigation";
import type { Metadata } from "next";
import { lessons, chapters, getLessonById, getLessonIndex } from "@/lib/lessons";
import { LessonShell } from "@/components/lesson-shell";

interface LessonPageProps {
  params: Promise<{ slug: string }>;
}

export function generateStaticParams() {
  return lessons.map((lesson) => ({ slug: lesson.id }));
}

export async function generateMetadata({
  params,
}: LessonPageProps): Promise<Metadata> {
  const { slug } = await params;
  const lesson = getLessonById(slug);
  if (!lesson) return {};

  const index = getLessonIndex(slug);
  const title = `${lesson.title} - Hypercode`;
  const description = `Lesson ${index + 1} of ${lessons.length}: ${lesson.title}. Learn Go interactively - no account needed.`;
  const url = `https://hypercode.alexisbouchez.com/lessons/${slug}`;

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
  const { slug } = await params;
  const lesson = getLessonById(slug);
  if (!lesson) notFound();

  return <LessonShell lesson={lesson} lessons={lessons} chapters={chapters} />;
}
