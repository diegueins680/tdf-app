-- Provider subjects identify authentication credentials, never shared emails.
-- Existing email-based Google access is deliberately not backfilled without proof.
CREATE TABLE IF NOT EXISTS auth_provider_identity (
  issuer text NOT NULL CHECK (issuer='https://accounts.google.com'),
  subject text NOT NULL CHECK (length(subject) BETWEEN 1 AND 255 AND subject !~ '[[:space:][:cntrl:]]'),
  credential_id bigint NOT NULL REFERENCES user_credential(id) ON DELETE RESTRICT,
  verification_method text NOT NULL CHECK (verification_method IN ('new-account','password-confirmed')),
  created_at timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY(issuer,subject)
);
CREATE INDEX IF NOT EXISTS auth_provider_identity_credential_idx ON auth_provider_identity(credential_id);
REVOKE ALL ON auth_provider_identity FROM PUBLIC;
