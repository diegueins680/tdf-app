import type { Parent, PhrasingContent, Root } from 'mdast';

export interface MarkdownMention { partyId: number; start: number; end: number }

/** API mention offsets count Unicode code points; Markdown positions count UTF-16 units. */
export function markdownMentions(source: string, mentions: readonly MarkdownMention[]) {
  const points = [...source];
  const ranges = mentions.filter((mention) => Number.isSafeInteger(mention.partyId) && mention.partyId > 0
    && Number.isInteger(mention.start) && Number.isInteger(mention.end)
    && mention.start >= 0 && mention.end > mention.start && mention.end <= points.length)
    .map((mention) => ({ ...mention, start: points.slice(0, mention.start).join('').length,
      end: points.slice(0, mention.end).join('').length }))
    .sort((a, b) => a.start - b.start);

  return () => (tree: Root) => {
    const visit = (parent: Parent) => {
      const children = [];
      for (const child of parent.children) {
        if (child.type === 'text') {
          const text = child;
          const start = text.position?.start.offset;
          // Escapes/entities change the source length. Leave those nodes intact.
          if (start === undefined || source.slice(start, start + text.value.length) !== text.value) {
            children.push(child); continue;
          }
          const parts: PhrasingContent[] = [];
          let offset = 0;
          for (const range of ranges) {
            const from = range.start - start; const to = range.end - start;
            if (from < offset || to > text.value.length) continue;
            if (from > offset) parts.push({ type: 'text', value: text.value.slice(offset, from) });
            parts.push({ type: 'link', url: `/perfil/${range.partyId}`,
              children: [{ type: 'text', value: text.value.slice(from, to) }] });
            offset = to;
          }
          if (offset < text.value.length) parts.push({ type: 'text', value: text.value.slice(offset) });
          children.push(...parts);
        } else {
          // Never create nested links, or interpret mentions inside literal code.
          if ('children' in child && child.type !== 'link' && child.type !== 'linkReference') visit(child);
          children.push(child);
        }
      }
      parent.children = children;
    };
    visit(tree);
  };
}
