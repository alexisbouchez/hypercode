/**
 * REST API client for Hypercode
 *
 * This client communicates with the Hypercode REST API server
 * instead of spawning a subprocess.
 *
 * Requires Node.js 18+ for native fetch support.
 */

export interface RestClientOptions {
  /** Base URL for the Hypercode REST API (e.g., "http://localhost:8080") */
  baseUrl: string;
  /** API key for authentication (optional) */
  apiKey?: string;
  /** Request timeout in milliseconds */
  timeout?: number;
}

export interface CreateThreadRequest {
  workingDirectory?: string;
  model?: string;
  instructions?: string;
}

export interface CreateThreadResponse {
  thread_id: string;
  created_at: string;
}

export interface InputItem {
  type: "message" | "local_image" | "base64_image";
  content?: string;
  path?: string;
  data?: string;
  media_type?: string;
}

export interface StartTurnRequest {
  input: InputItem[];
  stream?: boolean;
  previous_response_id?: string;
}

export interface TurnResponse {
  turn_id: string;
  output: OutputItem[];
  status: string;
}

export interface OutputItem {
  type: string;
  content?: string;
  id?: string;
  name?: string;
  arguments?: unknown;
  tool_call_id?: string;
  output?: string;
  path?: string;
  operation?: string;
  command?: string;
  exit_code?: number;
  stdout?: string;
  stderr?: string;
}

export interface TurnStreamEvent {
  type: string;
  turn_id?: string;
  content?: string;
  id?: string;
  name?: string;
  arguments?: unknown;
  tool_call_id?: string;
  output?: string;
  path?: string;
  operation?: string;
  command?: string;
  exit_code?: number;
  status?: string;
  code?: string;
  message?: string;
}

/**
 * REST API client for Hypercode
 */
export class HypercodeRestClient {
  private baseUrl: string;
  private apiKey?: string;
  private timeout: number;

  constructor(options: RestClientOptions) {
    this.baseUrl = options.baseUrl.replace(/\/$/, ""); // Remove trailing slash
    this.apiKey = options.apiKey;
    this.timeout = options.timeout ?? 30000;
  }

  private async request<T>(
    method: string,
    path: string,
    body?: unknown,
  ): Promise<T> {
    const url = `${this.baseUrl}${path}`;
    const headers: Record<string, string> = {
      "Content-Type": "application/json",
    };

    if (this.apiKey) {
      headers["Authorization"] = `Bearer ${this.apiKey}`;
    }

    const controller = new AbortController();
    const timeoutId = setTimeout(() => controller.abort(), this.timeout);

    try {
      const response = await fetch(url, {
        method,
        headers,
        body: body ? JSON.stringify(body) : undefined,
        signal: controller.signal,
      });

      if (!response.ok) {
        const errorBody = await response.text();
        throw new Error(
          `HTTP ${response.status}: ${response.statusText} - ${errorBody}`,
        );
      }

      return (await response.json()) as T;
    } finally {
      clearTimeout(timeoutId);
    }
  }

  /**
   * Create a new thread
   */
  async createThread(
    options: CreateThreadRequest = {},
  ): Promise<CreateThreadResponse> {
    return this.request<CreateThreadResponse>("POST", "/threads", options);
  }

  /**
   * List all threads
   */
  async listThreads(): Promise<{ threads: Array<{ thread_id: string; status: string }> }> {
    return this.request("GET", "/threads");
  }

  /**
   * Get a thread by ID
   */
  async getThread(
    threadId: string,
  ): Promise<{ thread_id: string; status: string }> {
    return this.request("GET", `/threads/${threadId}`);
  }

  /**
   * Resume a thread
   */
  async resumeThread(
    threadId: string,
    options: { model?: string } = {},
  ): Promise<{ thread_id: string; status: string }> {
    return this.request("POST", `/threads/${threadId}/resume`, options);
  }

  /**
   * Fork a thread
   */
  async forkThread(
    threadId: string,
    options: { turn_index?: number } = {},
  ): Promise<{ thread_id: string; forked_from: string }> {
    return this.request("POST", `/threads/${threadId}/fork`, options);
  }

  /**
   * Archive (delete) a thread
   */
  async archiveThread(threadId: string): Promise<{ success: boolean }> {
    return this.request("DELETE", `/threads/${threadId}`);
  }

  /**
   * Rollback a thread to a specific turn
   */
  async rollbackThread(
    threadId: string,
    turnIndex: number,
  ): Promise<{ success: boolean }> {
    return this.request("POST", `/threads/${threadId}/rollback`, {
      turn_index: turnIndex,
    });
  }

  /**
   * Start a turn (non-streaming)
   */
  async startTurn(
    threadId: string,
    input: InputItem[],
  ): Promise<TurnResponse> {
    return this.request<TurnResponse>("POST", `/threads/${threadId}/turns`, {
      input,
      stream: false,
    });
  }

  /**
   * Start a turn with streaming (SSE)
   */
  async *startTurnStream(
    threadId: string,
    input: InputItem[],
    signal?: AbortSignal,
  ): AsyncGenerator<TurnStreamEvent> {
    const url = `${this.baseUrl}/threads/${threadId}/turns/stream`;
    const headers: Record<string, string> = {
      "Content-Type": "application/json",
      Accept: "text/event-stream",
    };

    if (this.apiKey) {
      headers["Authorization"] = `Bearer ${this.apiKey}`;
    }

    const response = await fetch(url, {
      method: "POST",
      headers,
      body: JSON.stringify({ input, stream: true }),
      signal,
    });

    if (!response.ok) {
      const errorBody = await response.text();
      throw new Error(
        `HTTP ${response.status}: ${response.statusText} - ${errorBody}`,
      );
    }

    if (!response.body) {
      throw new Error("No response body");
    }

    const reader = response.body.getReader();
    const decoder = new TextDecoder();
    let buffer = "";

    try {
      while (true) {
        const { done, value } = await reader.read();
        if (done) break;

        buffer += decoder.decode(value, { stream: true });
        const lines = buffer.split("\n");
        buffer = lines.pop() || "";

        for (const line of lines) {
          if (line.startsWith("data: ")) {
            const data = line.slice(6);
            if (data === "{}") continue; // Keep-alive or done
            try {
              const event = JSON.parse(data) as TurnStreamEvent;
              yield event;
            } catch {
              // Skip invalid JSON
            }
          }
        }
      }
    } finally {
      reader.releaseLock();
    }
  }

  /**
   * Interrupt the current turn
   */
  async interruptTurn(threadId: string): Promise<{ success: boolean }> {
    return this.request("POST", `/threads/${threadId}/interrupt`);
  }

  /**
   * List available models
   */
  async listModels(): Promise<{ models: Array<{ id: string; name: string }> }> {
    return this.request("GET", "/models");
  }

  /**
   * Read configuration
   */
  async readConfig(): Promise<{ config: Record<string, unknown> }> {
    return this.request("GET", "/config");
  }

  /**
   * Write a configuration value
   */
  async writeConfig(
    key: string,
    value: unknown,
  ): Promise<{ success: boolean }> {
    return this.request("POST", "/config", { key, value });
  }

  /**
   * Get account information
   */
  async getAccount(): Promise<{
    authenticated: boolean;
    auth_mode?: string;
    email?: string;
  }> {
    return this.request("GET", "/account");
  }

  /**
   * Get rate limits
   */
  async getRateLimits(): Promise<{
    requests_per_minute?: number;
    tokens_per_minute?: number;
    remaining_requests?: number;
    remaining_tokens?: number;
  }> {
    return this.request("GET", "/account/rate-limits");
  }

  /**
   * Login with API key
   */
  async login(
    apiKey: string,
    provider?: string,
  ): Promise<{ success: boolean; auth_mode: string }> {
    return this.request("POST", "/account/login", {
      api_key: apiKey,
      provider,
    });
  }

  /**
   * Logout
   */
  async logout(): Promise<{ success: boolean }> {
    return this.request("POST", "/account/logout");
  }

  /**
   * Health check
   */
  async healthCheck(): Promise<{ status: string; version: string }> {
    return this.request("GET", "/health");
  }
}
