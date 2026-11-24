-- Genre keys are case-folded letters and numbers in any script. Use a
-- language-independent Unicode collation even in databases initialized in C.
ALTER TABLE genre_suggestion
  DROP CONSTRAINT IF EXISTS genre_suggestion_normalized_key_check,
  ADD CONSTRAINT genre_suggestion_normalized_key_check
    CHECK (normalized_key COLLATE pg_catalog."und-x-icu" ~ '^[[:alnum:]]+$'
       AND normalized_key COLLATE pg_catalog."und-x-icu" !~ '[[:upper:]]');
