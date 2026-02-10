import type { MetadataRoute } from "next";
import { lessons } from "@/lib/lessons";

export default function sitemap(): MetadataRoute.Sitemap {
  const base = "https://hypercode.alexisbouchez.com";

  return lessons.map((lesson) => ({
    url: `${base}/lessons/${lesson.id}`,
    changeFrequency: "monthly",
    priority: 0.8,
  }));
}
