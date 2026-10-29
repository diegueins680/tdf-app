import { Typography, type TypographyProps } from '@mui/material';
import ReactMarkdown, { defaultUrlTransform, type Components } from 'react-markdown';
import remarkBreaks from 'remark-breaks';
import remarkGfm from 'remark-gfm';
import { markdownMentions, type MarkdownMention } from '../utils/markdownMentions';

interface MarkdownTextProps extends Omit<TypographyProps, 'children' | 'component'> {
  children?: string | null;
  /** Compact previews and text inside an existing paragraph use phrasing elements only. */
  inline?: boolean;
  /** Content inside a clickable card must not introduce nested links or controls. */
  disableLinks?: boolean;
  mentions?: readonly MarkdownMention[];
}

const components: Components = {
  a: ({ href, children, title }) => href ? (
    <a href={href} title={title} onClick={(event) => event.stopPropagation()} {...(/^(?:https?:)?\/\//i.test(href)
      ? { target: '_blank', rel: 'noopener noreferrer' } : {})}>{children}</a>
  ) : <>{children}</>,
  img: ({ src, alt, title }) => src ? <img src={src} alt={alt ?? ''} title={title} loading="lazy" /> : <>{alt}</>,
  table: ({ children }) => <div className="markdown-table"><table>{children}</table></div>,
};

const inlineComponents: Components = {
  ...components,
  p: ({ children }) => <span>{children} </span>,
  h1: 'strong', h2: 'strong', h3: 'strong', h4: 'strong', h5: 'strong', h6: 'strong',
  blockquote: 'span', pre: 'span', ul: 'span', ol: 'span',
  li: ({ children }) => <span>{children} </span>,
  table: 'span', thead: 'span', tbody: 'span', tr: 'span',
  th: ({ children }) => <span>{children} </span>,
  td: ({ children }) => <span>{children} </span>,
  hr: () => <span> · </span>,
  img: ({ alt }) => <>{alt}</>,
  input: () => null,
};

/** Display authored Markdown without evaluating HTML or accepting executable URLs. */
export default function MarkdownText({ children, inline = false, disableLinks = false, mentions = [], sx, ...props }: MarkdownTextProps) {
  return (
    <Typography
      {...props}
      component={inline ? 'span' : 'div'}
      sx={[
        {
          overflowWrap: 'anywhere', whiteSpace: 'normal', minWidth: 0,
          '& p': { my: 1 }, '& > :first-child': { mt: 0 }, '& > :last-child': { mb: 0 },
          // Match the surrounding foreground, including text on colored message bubbles.
          '& a': { color: 'inherit', textDecoration: 'underline' },
          '& img': { maxWidth: '100%', height: 'auto', borderRadius: 1 },
          '& pre': { overflowX: 'auto', p: 1.5, borderRadius: 1, bgcolor: 'action.hover', whiteSpace: 'pre' },
          '& code': { fontFamily: 'monospace', bgcolor: 'action.hover', borderRadius: 0.5, px: 0.4 },
          '& pre code': { p: 0, bgcolor: 'transparent' },
          '& blockquote': { ml: 0, pl: 2, borderLeft: '3px solid', borderColor: 'divider', color: 'text.secondary' },
          '& ul, & ol': { pl: 3 }, '& h1, & h2, & h3, & h4, & h5, & h6': { lineHeight: 1.3, mt: 2, mb: 1 },
          '& .markdown-table': { overflowX: 'auto', maxWidth: '100%' },
          '& table': { borderCollapse: 'collapse' },
          '& th, & td': { border: '1px solid', borderColor: 'divider', px: 1, py: 0.5, textAlign: 'left' },
        },
        ...(Array.isArray(sx) ? sx : [sx]),
      ]}
    >
      <ReactMarkdown
        remarkPlugins={[remarkGfm, remarkBreaks, markdownMentions(children ?? '', mentions)]}
        skipHtml
        components={inline ? inlineComponents : components}
        disallowedElements={disableLinks ? ['a', 'input'] : undefined}
        unwrapDisallowed
        urlTransform={(url, key) => {
          const safe = defaultUrlTransform(url);
          return key === 'src' && /^(?!https?:)[a-z][a-z\d+.-]*:/i.test(safe) ? '' : safe;
        }}
      >{children ?? ''}</ReactMarkdown>
    </Typography>
  );
}
