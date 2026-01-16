/**
 * REST-based Thread implementation
 *
 * This provides the same interface as the subprocess-based Thread
 * but uses the REST API for communication.
 */

import {
  HypercodeRestClient,
  InputItem,
  TurnStreamEvent,
  OutputItem,
} from "./restClient";
import { ThreadOptions } from "./threadOptions";

export interface RestThreadOptions extends ThreadOptions {
  /** Base URL for the REST API */
  baseUrl: string;
  /** API key for authentication */
  apiKey?: string;
}

export interface RestThreadEvent {
  type: string;
  item?: unknown;
  status?: string;
  error?: { code: string; message: string };
}

export interface RestResponseItem {
  type: string;
  role?: string;
  content?: Array<{ type: string; text: string }>;
  summary?: Array<{ type: string; text: string }>;
  call_id?: string;
  name?: string;
  arguments?: string;
  action?: Array<{ type: string; path?: string; operation?: string; command?: string[] }>;
}

export interface RestTurnOptions {
  /** Abort signal for cancellation */
  signal?: AbortSignal;
  /** Images to include */
  images?: Array<{
    type: "local_image" | "base64_image";
    path?: string;
    data?: string;
    mediaType?: string;
  }>;
}

/**
 * Thread that communicates via REST API
 */
export class RestThread {
  private client: HypercodeRestClient;
  private threadId: string | null = null;
  private options: RestThreadOptions;

  constructor(options: RestThreadOptions, existingThreadId?: string) {
    this.client = new HypercodeRestClient({
      baseUrl: options.baseUrl,
      apiKey: options.apiKey,
    });
    this.options = options;
    this.threadId = existingThreadId || null;
  }

  /**
   * Get the thread ID (creates thread if needed)
   */
  async getThreadId(): Promise<string> {
    if (!this.threadId) {
      const response = await this.client.createThread({
        workingDirectory: this.options.workingDirectory,
        model: this.options.model,
      });
      this.threadId = response.thread_id;
    }
    return this.threadId;
  }

  /**
   * Run a turn and get the complete response
   */
  async run(
    input: string | string[],
    options: RestTurnOptions = {},
  ): Promise<RestResponseItem[]> {
    const threadId = await this.getThreadId();

    const inputItems: InputItem[] = [];

    // Convert string input to message items
    const messages = Array.isArray(input) ? input : [input];
    for (const msg of messages) {
      inputItems.push({ type: "message", content: msg });
    }

    // Add images if provided
    if (options.images) {
      for (const image of options.images) {
        if (image.type === "local_image" && image.path) {
          inputItems.push({ type: "local_image", path: image.path });
        } else if (image.type === "base64_image" && image.data) {
          inputItems.push({
            type: "base64_image",
            data: image.data,
            media_type: image.mediaType,
          });
        }
      }
    }

    const response = await this.client.startTurn(threadId, inputItems);

    // Convert output items to ResponseItems
    return response.output.map((item) => this.convertOutputItem(item));
  }

  /**
   * Run a turn with streaming
   */
  async *runStreamed(
    input: string | string[],
    options: RestTurnOptions = {},
  ): AsyncGenerator<RestThreadEvent> {
    const threadId = await this.getThreadId();

    const inputItems: InputItem[] = [];

    // Convert string input to message items
    const messages = Array.isArray(input) ? input : [input];
    for (const msg of messages) {
      inputItems.push({ type: "message", content: msg });
    }

    // Add images if provided
    if (options.images) {
      for (const image of options.images) {
        if (image.type === "local_image" && image.path) {
          inputItems.push({ type: "local_image", path: image.path });
        } else if (image.type === "base64_image" && image.data) {
          inputItems.push({
            type: "base64_image",
            data: image.data,
            media_type: image.mediaType,
          });
        }
      }
    }

    for await (const event of this.client.startTurnStream(
      threadId,
      inputItems,
      options.signal,
    )) {
      yield this.convertStreamEvent(event);
    }
  }

  /**
   * Interrupt the current turn
   */
  async interrupt(): Promise<void> {
    if (this.threadId) {
      await this.client.interruptTurn(this.threadId);
    }
  }

  /**
   * Fork this thread
   */
  async fork(turnIndex?: number): Promise<RestThread> {
    if (!this.threadId) {
      throw new Error("Cannot fork a thread that hasn't been started");
    }

    const response = await this.client.forkThread(this.threadId, {
      turn_index: turnIndex,
    });

    return new RestThread(this.options, response.thread_id);
  }

  /**
   * Rollback to a specific turn
   */
  async rollback(turnIndex: number): Promise<void> {
    if (!this.threadId) {
      throw new Error("Cannot rollback a thread that hasn't been started");
    }

    await this.client.rollbackThread(this.threadId, turnIndex);
  }

  /**
   * Archive (delete) this thread
   */
  async archive(): Promise<void> {
    if (this.threadId) {
      await this.client.archiveThread(this.threadId);
      this.threadId = null;
    }
  }

  private convertOutputItem(item: OutputItem): RestResponseItem {
    switch (item.type) {
      case "message":
        return {
          type: "message",
          role: "assistant",
          content: [{ type: "output_text", text: item.content || "" }],
        };
      case "reasoning":
        return {
          type: "reasoning",
          summary: [{ type: "summary_text", text: item.content || "" }],
        };
      case "tool_call":
        return {
          type: "function_call",
          call_id: item.id || "",
          name: item.name || "",
          arguments: JSON.stringify(item.arguments || {}),
        };
      case "file_change":
        return {
          type: "local_shell_call",
          call_id: "",
          action: [
            {
              type: "fs",
              path: String(item.path || ""),
              operation: String(item.operation || "write"),
            },
          ],
        };
      case "command_execution":
        return {
          type: "local_shell_call",
          call_id: "",
          action: [
            {
              type: "exec",
              command: [String(item.command || "")],
            },
          ],
        };
      default:
        return {
          type: "message",
          role: "assistant",
          content: [{ type: "output_text", text: JSON.stringify(item) }],
        };
    }
  }

  private convertStreamEvent(event: TurnStreamEvent): RestThreadEvent {
    switch (event.type) {
      case "turn_started":
        return { type: "turn.started" };
      case "message_delta":
        return {
          type: "item.updated",
          item: {
            type: "message",
            role: "assistant",
            content: [
              { type: "output_text", text: event.content || "" },
            ],
          },
        };
      case "message_complete":
        return {
          type: "item.completed",
          item: {
            type: "message",
            role: "assistant",
            content: [
              { type: "output_text", text: event.content || "" },
            ],
          },
        };
      case "turn_completed":
        return { type: "turn.completed", status: event.status || "completed" };
      case "error":
        return {
          type: "error",
          error: {
            code: event.code || "unknown",
            message: event.message || "Unknown error",
          },
        };
      default:
        return { type: "turn.started" };
    }
  }
}
