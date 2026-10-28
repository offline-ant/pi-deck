import { fauxAssistantMessage, fauxProvider, type TranscriptContext } from "@earendil-works/pi-ai";
import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";

/** Smoke-test fixture: every model call stays inside Pi's in-process faux provider. */
export default function piDeckFaux(pi: ExtensionAPI): void {
  const provider = fauxProvider({ provider: "pi-deck-faux", models: [{ id: "one" }], tokensPerSecond: 100_000 });
  const lastUserText = (context: TranscriptContext): string => {
    const message = context.messages.findLast((item) => item.role === "user");
    if (!message) return "";
    return typeof message.content === "string" ? message.content
      : message.content.flatMap((item) => item.type === "text" ? [item.text] : []).join("\n");
  };
  // Prompts containing "slow" stay working for a few seconds (manual checks).
  provider.setResponses(Array.from({ length: 100 }, () => async (context: TranscriptContext) => {
    const text = lastUserText(context);
    if (text.includes("slow")) await new Promise((resolve) => setTimeout(resolve, 4000));
    return fauxAssistantMessage(`faux reply: ${text}`);
  }));
  pi.registerProvider(provider.provider);
  pi.on("before_agent_start", (_event, ctx) => {
    if (ctx.model?.provider !== "pi-deck-faux") throw new Error("pi-deck smoke refuses non-faux providers");
  });
  pi.registerCommand("deck-confirm", {
    description: "Ask a confirm dialog (pi-deck smoke test)",
    handler: async (_args, ctx) => {
      const confirmed = await ctx.ui.confirm("Deck check", "Proceed?");
      ctx.ui.notify(`deck-confirmed:${confirmed}`, "info");
    },
  });
}
