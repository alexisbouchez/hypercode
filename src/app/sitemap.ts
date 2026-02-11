import type { MetadataRoute } from "next";
import { lessons } from "@/lib/lessons";

export default function sitemap(): MetadataRoute.Sitemap {
  const base = "https://hypercode.alexisbouchez.com";

  return [
    {
      url: `${base}/introduction`,
      changeFrequency: "monthly",
      priority: 0.9,
    },
    ...lessons.map((lesson) => ({
      url: `${base}/lessons/${lesson.id}`,
      changeFrequency: "monthly" as const,
      priority: 0.8,
    })),
    {
      url: `${base}/whats-next`,
      changeFrequency: "monthly",
      priority: 0.7,
    },
  ];
}
