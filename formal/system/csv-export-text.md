# CSV string-cell encoding

`UI-CSV-STRING-PREFIX-001` applies to the event-finance download and merchandise
order export. Both use the same `serializeCsvCell` function.

The encoded-cell policy is:

- For string inputs, prefix one apostrophe when the value starts with optional
  whitespace followed by an ASCII or fullwidth `=`, `+`, `-` or `@`, or when its
  leading whitespace contains a tab, carriage return or line feed.
- Preserve typed numbers, including negative safe integer cents and percentages,
  and booleans. Encode null and undefined as empty cells.
- Quote every cell and double embedded double quotes. Preserve embedded commas
  and line breaks inside the quoted field.

The finance row builder, monetary-precision refusal, query-error/loading refusal,
24-column positions and per-entry currency binding remain unchanged. Merchandise
keeps its existing BOM, CRLF records, column selection and authorization-dependent
finance columns. This change removes the old merchandise behavior that prefixed
typed negative numbers as if they were user-supplied strings.

Event titles, finance concepts and external references can contain formula-like
text under the existing server validation. CSV quoting alone does not distinguish
such text from formula input in spreadsheet applications. Currency is already
validated separately; this repair does not identify currency as an injection
vector or weaken its validation.

The apostrophe policy extends the existing merchandise policy to both exports.
It is an encoding decision, not a universal guarantee about spreadsheet products,
import settings, re-saving, or downstream transformations. The OWASP
[CSV Injection](https://community.owasp.org/attacks/CSV_Injection) discussion
documents these consumer differences. No spreadsheet or formula is executed by
the tests, and no external URL or provider is contacted.

Actual regression evidence reads the Blob created by the mounted event-finance
export button. Separate old-source negative controls observe unsafe title,
concept and external-reference cells; a merchandise negative control observes
typed negative amounts being converted to text. The corrected output is parsed
back to verify all 24 columns, embedded quotes/newlines, negative cents and the
existing USD-summary/EUR-void-entry binding. Scalar controls exercise ASCII and
fullwidth markers, leading controls, ordinary text, numbers, booleans and absent
values. Finite tests establish only the recorded source-bound cases, not complete
implementation refinement or production spreadsheet acceptance.
