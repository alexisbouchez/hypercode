import type { NextConfig } from "next";

const nextConfig: NextConfig = {
  async headers() {
    return [
      {
        source: "/:path*.wasm",
        headers: [
          { key: "Content-Type", value: "application/wasm" },
        ],
      },
      {
        source: "/(.*)",
        headers: [
          { key: "Cross-Origin-Opener-Policy", value: "same-origin" },
          { key: "Cross-Origin-Embedder-Policy", value: "require-corp" },
        ],
      },
    ];
  },
  async redirects() {
    return [
      {
        source: "/introduction",
        destination: "/go/introduction",
        permanent: true,
      },
      {
        source: "/lessons/:slug",
        destination: "/go/lessons/:slug",
        permanent: true,
      },
      {
        source: "/whats-next",
        destination: "/go/whats-next",
        permanent: true,
      },
    ];
  },
};

export default nextConfig;
