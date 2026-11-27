-- A shared link can reference a genre absent from this catalog. Keep that
-- selection restrictive while still expanding any known parent genres.
CREATE OR REPLACE FUNCTION taxonomy_genre_closure(selected UUID[])
RETURNS UUID[]
LANGUAGE sql
STABLE
PARALLEL SAFE
AS $$
  WITH RECURSIVE tree(id) AS (
    SELECT id FROM unnest(selected) AS requested(id) WHERE id IS NOT NULL
    UNION
    SELECT child.id FROM genre child JOIN tree ON child.parent_id = tree.id WHERE child.active
  )
  SELECT coalesce(array_agg(id ORDER BY id), '{}'::uuid[]) FROM tree
$$;
