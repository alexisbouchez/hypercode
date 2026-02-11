"use client";

import ReactMarkdown from "react-markdown";
import remarkGfm from "remark-gfm";
import { useEffect, useRef, useState, type ComponentPropsWithoutRef } from "react";
import { useTheme } from "next-themes";
import { codeToHtml } from "shiki";

function CodeBlock({ code, language }: { code: string; language: string }) {
  const [html, setHtml] = useState<string | null>(null);
  const containerRef = useRef<HTMLDivElement>(null);
  const { resolvedTheme } = useTheme();

  useEffect(() => {
    let cancelled = false;
    const theme = resolvedTheme === "light" ? "github-light-default" : "github-dark-default";
    codeToHtml(code, {
      lang: language || "text",
      theme,
    }).then((result) => {
      if (!cancelled) setHtml(result);
    });
    return () => {
      cancelled = true;
    };
  }, [code, language, resolvedTheme]);

  if (!html) {
    return (
      <pre className="bg-[oklch(0.97_0_0)] dark:bg-[oklch(0.18_0_0)] text-foreground p-4 mb-4 overflow-x-auto text-sm font-mono leading-relaxed border-l-2 border-primary">
        <code>{code}</code>
      </pre>
    );
  }

  return (
    <div
      ref={containerRef}
      className="shiki-block mb-4 overflow-x-auto text-sm leading-relaxed border-l-2 border-primary [&_pre]:p-4 [&_code]:font-mono"
      dangerouslySetInnerHTML={{ __html: html }}
    />
  );
}

interface LessonContentProps {
  content: string;
}

export function LessonContent({ content }: LessonContentProps) {
  return (
    <div className="lesson-prose">
      <ReactMarkdown
        remarkPlugins={[remarkGfm]}
        components={{
          h2: ({ children, ...props }: ComponentPropsWithoutRef<"h2">) => (
            <h2
              className="text-xl font-bold text-foreground mt-8 mb-4 first:mt-0 tracking-tight"
              {...props}
            >
              {children}
            </h2>
          ),
          h3: ({ children, ...props }: ComponentPropsWithoutRef<"h3">) => (
            <h3
              className="text-base font-semibold text-foreground mt-6 mb-3 tracking-tight"
              {...props}
            >
              {children}
            </h3>
          ),
          p: ({ children, ...props }: ComponentPropsWithoutRef<"p">) => (
            <p
              className="text-[15px] leading-relaxed text-foreground/85 mb-4"
              {...props}
            >
              {children}
            </p>
          ),
          ul: ({ children, ...props }: ComponentPropsWithoutRef<"ul">) => (
            <ul
              className="list-disc list-outside pl-5 mb-4 space-y-1.5 text-[15px] leading-relaxed text-foreground/85"
              {...props}
            >
              {children}
            </ul>
          ),
          ol: ({ children, ...props }: ComponentPropsWithoutRef<"ol">) => (
            <ol
              className="list-decimal list-outside pl-5 mb-4 space-y-1.5 text-[15px] leading-relaxed text-foreground/85"
              {...props}
            >
              {children}
            </ol>
          ),
          li: ({ children, ...props }: ComponentPropsWithoutRef<"li">) => (
            <li className="pl-1" {...props}>
              {children}
            </li>
          ),
          strong: ({ children, ...props }: ComponentPropsWithoutRef<"strong">) => (
            <strong className="font-semibold text-foreground" {...props}>
              {children}
            </strong>
          ),
          em: ({ children, ...props }: ComponentPropsWithoutRef<"em">) => (
            <em className="italic text-foreground/90" {...props}>
              {children}
            </em>
          ),
          pre: ({ children }: ComponentPropsWithoutRef<"pre">) => {
            // react-markdown wraps code blocks in <pre><code>
            // Extract the code content and language from the child <code> element
            const codeElement = children as React.ReactElement<{
              className?: string;
              children?: string;
            }>;
            const className = codeElement?.props?.className ?? "";
            const lang = className.replace("language-", "") || "text";
            const code =
              typeof codeElement?.props?.children === "string"
                ? codeElement.props.children.replace(/\n$/, "")
                : "";

            return <CodeBlock code={code} language={lang} />;
          },
          code: ({ className, children, ...props }: ComponentPropsWithoutRef<"code">) => {
            // Only handle inline code here; block code is handled by `pre`
            if (className?.includes("language-")) {
              return (
                <code className={className} {...props}>
                  {children}
                </code>
              );
            }
            return (
              <code
                className="bg-muted px-1.5 py-0.5 rounded text-[13px] font-mono text-foreground"
                {...props}
              >
                {children}
              </code>
            );
          },
          table: ({ children, ...props }: ComponentPropsWithoutRef<"table">) => (
            <div className="overflow-x-auto mb-4">
              <table className="w-full text-sm border-collapse" {...props}>
                {children}
              </table>
            </div>
          ),
          th: ({ children, ...props }: ComponentPropsWithoutRef<"th">) => (
            <th
              className="text-left p-2 border-b border-border font-semibold text-foreground"
              {...props}
            >
              {children}
            </th>
          ),
          td: ({ children, ...props }: ComponentPropsWithoutRef<"td">) => (
            <td
              className="p-2 border-b border-border text-foreground/85"
              {...props}
            >
              {children}
            </td>
          ),
          hr: (props: ComponentPropsWithoutRef<"hr">) => (
            <hr className="my-6 border-border" {...props} />
          ),
          a: ({ children, ...props }: ComponentPropsWithoutRef<"a">) => (
            <a
              className="text-primary underline underline-offset-2 hover:text-primary/80"
              target="_blank"
              rel="noopener noreferrer"
              {...props}
            >
              {children}
            </a>
          ),
          blockquote: ({ children, ...props }: ComponentPropsWithoutRef<"blockquote">) => (
            <blockquote
              className="border-l-2 border-primary/50 pl-4 my-4 text-foreground/75 italic"
              {...props}
            >
              {children}
            </blockquote>
          ),
        }}
      >
        {content}
      </ReactMarkdown>
    </div>
  );
}
