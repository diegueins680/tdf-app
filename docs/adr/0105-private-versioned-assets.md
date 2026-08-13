# ADR-0105: Private, versioned, checksummed assets

Status: Accepted — 2026-08-13

## Decision

Commercial and distribution assets use private object storage, multipart upload sessions, short-lived
signed URLs, SHA-256, MIME/signature inspection, malware quarantine, access audit, retention state,
and immutable delivered versions. Database records store provider-neutral object references, never
public browser links or storage credentials.

Google Drive may be an export adapter, not canonical storage. Base64 API uploads and public URLs were
rejected for size, secrecy, integrity, and revocation reasons.
