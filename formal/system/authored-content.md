# Authored web content and profile navigation

`WEB-CONTENT-001` follows the product owner request of 28 October 2026.

Authored long-form display fields use the shared Markdown renderer. CommonMark,
GitHub-style tables, task lists, strikethrough and ordinary line breaks are
supported. Raw HTML is skipped. URL filtering uses the Markdown library safe
transform; image sources also reject non-HTTP schemes. Rendered tables and code
scroll within the content area. Compact previews use phrasing elements and omit
nested controls or links when their containing card is already interactive.
Editing inputs, source comparisons, diagnostics and identity labels remain literal.

User profile links require a known positive safe-integer Party ID. Names and
avatars never identify an account by themselves. Existing profile visibility and
authentication remain authoritative. Public review responses may include an
optional Party ID. Merch buyer privacy suppresses that ID and the avatar together.
Old responses without an ID remain compatible and do not fabricate a link.

Mentions retain API Unicode-code-point offsets while Markdown positions use
UTF-16 offsets. A text node whose rendered value differs from its source because
of escapes/entities remains intact. Mentions are never interpreted inside
existing links or code. Full parser correctness is an explicit open obligation.

The change does not rewrite stored content, alter consent or privacy preferences,
activate features, or migrate data. Native display behavior is outside this web
change; the paired generated mobile API contract carries the optional fields.
Component tests, browser/layout checks and exact SQL projection checks are scoped
empirical evidence. A protected merge and deployment require the ordinary checks
and independent review; they are not implied by this contract.
