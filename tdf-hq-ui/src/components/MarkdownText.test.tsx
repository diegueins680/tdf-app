import '@testing-library/jest-dom';
import { cleanup, fireEvent, render, screen } from '@testing-library/react';
import { afterEach, describe, expect, it, jest } from '@jest/globals';
import MarkdownText from './MarkdownText';
import { Card } from '@mui/material';

afterEach(cleanup);

describe('authored Markdown', () => {
  it('renders the artist bio, paragraphs, lists, quotes, code, and GFM', () => {
    const { container } = render(<MarkdownText>{`# Bio

**CATEGAL** mezcla *cumbia* y ~~reggae~~.
Una segunda línea.

- Quito
- Ecuador

> Música tropical

\`sintetizadores\`

\`\`\`js
const ritmo = 1;
\`\`\`

| Ciudad | País |
| --- | --- |
| Quito | Ecuador |

- [x] Publicado`}</MarkdownText>);
    expect(screen.getByRole('heading', { name: 'Bio' })).toBeInTheDocument();
    expect(screen.getByText('CATEGAL').tagName).toBe('STRONG');
    expect(screen.getByText('cumbia').tagName).toBe('EM');
    expect(container.querySelector('del')).toHaveTextContent('reggae');
    expect(container.querySelector('br')).toBeInTheDocument();
    expect(container.querySelectorAll('li')).toHaveLength(3);
    expect(container.querySelector('blockquote')).toHaveTextContent('Música tropical');
    expect(container.querySelector('pre code')).toHaveTextContent('const ritmo = 1;');
    expect(screen.getByRole('table')).toHaveTextContent('Quito');
    expect(screen.getByRole('checkbox')).toBeChecked();
    expect(screen.getByRole('checkbox')).toBeDisabled();
    expect(container.querySelector('p div')).toBeNull();
  });

  it('handles links, autolinks, images, empty values, and plain-text line breaks', () => {
    const { container, rerender } = render(<MarkdownText>{'[Perfil](/perfil/7) https://tdfrecords.net\n![Banda](/band.jpg)'}</MarkdownText>);
    expect(screen.getByRole('link', { name: 'Perfil' })).toHaveAttribute('href', '/perfil/7');
    const external = screen.getByRole('link', { name: 'https://tdfrecords.net' });
    expect(external).toHaveAttribute('target', '_blank');
    expect(external).toHaveAttribute('rel', 'noopener noreferrer');
    expect(screen.getByRole('img', { name: 'Banda' })).toHaveAttribute('loading', 'lazy');
    rerender(<MarkdownText>{'Primera\nSegunda'}</MarkdownText>);
    expect(container.querySelector('br')).toBeInTheDocument();
    rerender(<MarkdownText>{null}</MarkdownText>);
    expect(container.textContent).toBe('');
  });

  it('rejects scripts, HTML handlers, and executable/data URLs', () => {
    const { container } = render(<MarkdownText>{`<script>alert(1)</script>

<img src=x onerror="alert(1)">

[malo](javascript:alert%281%29) [datos](data:text/html,attack) [vb](vbscript:attack)

![peligro](data:image/svg+xml,attack)`}</MarkdownText>);
    expect(container.querySelector('script, iframe, img, [onerror]')).toBeNull();
    expect(container.querySelector('a')).toBeNull();
    expect(container.innerHTML).not.toMatch(/javascript:|vbscript:|data:text/);
  });

  it('keeps compact content valid inside a link without nested controls', () => {
    const { container } = render(<a href="/artista/337"><MarkdownText inline disableLinks>{'# **CATEGAL**\n\n[Escucha](https://example.com)\n\n- Música\n- [x] Lista\n\n![Foto](/photo.jpg)'}</MarkdownText></a>);
    expect(container.querySelectorAll('a')).toHaveLength(1);
    expect(container.querySelector('div, p, h1, ul, li, img, input')).toBeNull();
    expect(container.querySelector('strong')).toHaveTextContent('CATEGAL');
    expect(container).toHaveTextContent('Escucha');
  });

  it.each(['[Escucha](https://example.test/music)', 'https://example.test/music'])(
    'keeps link navigation independent from its enclosing artist card: %s', (content) => {
      const navigateToArtist = jest.fn();
      render(<Card onClick={navigateToArtist}>
        <span>Artist preview</span><MarkdownText inline>{content}</MarkdownText>
      </Card>);
      const click = new MouseEvent('click', { bubbles: true, cancelable: true });
      fireEvent(screen.getByRole('link'), click);
      expect(navigateToArtist).not.toHaveBeenCalled();
      expect(click.defaultPrevented).toBe(false);
      expect(screen.getByRole('link')).toHaveAttribute('target', '_blank');
      fireEvent.click(screen.getByText('Artist preview'));
      expect(navigateToArtist).toHaveBeenCalledTimes(1);
    },
  );

  it('preserves Unicode mention offsets alongside Markdown and never nests links', () => {
    const body = '🎶 **Hola** @Ana y [@Ana](/otro)';
    const start = [...body].indexOf('@');
    const { container } = render(<MarkdownText mentions={[{ partyId: 7, start, end: start + 4 }]}>{body}</MarkdownText>);
    expect(screen.getAllByRole('link', { name: '@Ana' })).toHaveLength(2);
    expect(container.querySelector('a[href="/perfil/7"]')).toHaveTextContent('@Ana');
    expect(container.querySelector('strong')).toHaveTextContent('Hola');
    expect(container.querySelector('a a')).toBeNull();
  });
});
