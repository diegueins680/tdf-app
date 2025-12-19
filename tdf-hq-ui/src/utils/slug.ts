export function slugify(value: string, maxLength = 64): string {
  const normalized = value
    .trim()
    .normalize('NFD')
    .replace(/[\u0300-\u036f]/g, '');

  const slug = normalized
    .toLowerCase()
    .replace(/[^a-z0-9]+/g, '-')
    .replace(/-+/g, '-')
    .replace(/^-+/, '')
    .replace(/-+$/, '');

  if (!maxLength || maxLength <= 0) return slug;
  return slug.slice(0, maxLength).replace(/-+$/, '');
}

