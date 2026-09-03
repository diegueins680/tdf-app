-- Additive indexes for the minimal Party selector.  The expressions mirror
-- the endpoint's accent-insensitive matching and retain the original values.
-- Rollback: DROP INDEX IF EXISTS party_selector_display_name_idx;
--           DROP INDEX IF EXISTS party_selector_legal_name_idx;
--           DROP INDEX IF EXISTS party_selector_active_username_idx;

CREATE INDEX IF NOT EXISTS party_selector_display_name_idx
  ON party (translate(lower(display_name), 'áàäâéèëêíìïîóòöôúùüûñ', 'aaaaeeeeiiiioooouuuun'));

CREATE INDEX IF NOT EXISTS party_selector_legal_name_idx
  ON party (translate(lower(COALESCE(legal_name, '')), 'áàäâéèëêíìïîóòöôúùüûñ', 'aaaaeeeeiiiioooouuuun'));

CREATE INDEX IF NOT EXISTS party_selector_active_username_idx
  ON user_credential (lower(replace(username, '@', '')))
  WHERE active = TRUE;
