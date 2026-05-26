import { describe, expect, it } from "vitest";

import { parseEngineersJson } from "./engineers";

describe("parseEngineersJson", () => {
  it("returns null for malformed JSON instead of throwing", () => {
    expect(() => parseEngineersJson("{not valid json")).not.toThrow();
    expect(parseEngineersJson("{not valid json")).toBeNull();
  });
});
