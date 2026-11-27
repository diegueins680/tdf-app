import glyphUrl from '../assets/tdf-logo-alt.svg';

export interface TicketPass {
  title: string;
  date?: string;
  venue?: string;
  artwork?: string | null;
  serial: string;
  details: string[];
  code: string;
  instruction: string;
  policies: { label: string; text: string }[];
}

export const ticketLogo = glyphUrl;
const ink = '#172620';
const paper = '#fbf8ed';
const accent = '#c9eb85';

// Only anonymous, exportable images may touch the private credential canvas.
function loadImage(url?: string | null): Promise<HTMLImageElement | null> {
  if (!url) return Promise.resolve(null);
  return new Promise((resolve) => {
    const image = new Image();
    const finish = (result: HTMLImageElement | null) => {
      window.clearTimeout(timer);
      image.onload = image.onerror = null;
      resolve(result);
    };
    const timer = window.setTimeout(() => finish(null), 4000);
    image.crossOrigin = 'anonymous';
    image.referrerPolicy = 'no-referrer';
    image.onload = () => finish(image);
    image.onerror = () => finish(null);
    image.src = url;
  });
}

function wrap(ctx: CanvasRenderingContext2D, value: string, width: number): string[] {
  const lines: string[] = [];
  for (const paragraph of value.trim().split(/\n+/)) {
    let line = '';
    for (const word of paragraph.trim().split(/\s+/)) {
      const next = line ? line + ' ' + word : word;
      if (ctx.measureText(next).width <= width) {
        line = next;
        continue;
      }
      if (line) lines.push(line);
      line = '';
      for (const char of word) {
        if (line && ctx.measureText(line + char).width > width) {
          lines.push(line);
          line = '';
        }
        line += char;
      }
    }
    if (line) lines.push(line);
  }
  return lines;
}

/** Compose locally, retaining the QR's original white quiet zone and hard module edges. */
export async function renderTicketPass(pass: TicketPass, qr: HTMLCanvasElement): Promise<string> {
  const [logo, art] = await Promise.all([loadImage(ticketLogo), loadImage(pass.artwork)]);
  const canvas = document.createElement('canvas');
  const context = canvas.getContext('2d');
  if (!context) throw new Error('Canvas unavailable');
  const ctx = context;
  const width = 540;
  const inset = 28;
  const content = width - 2 * inset;
  const draws: (() => void)[] = [];
  let y = 0;
  function block(value: string, font: string, leading: number, color = ink, maxWidth = content) {
    ctx.font = font;
    for (const line of wrap(ctx, value, maxWidth)) {
      const top = y;
      draws.push(() => {
        ctx.font = font;
        ctx.fillStyle = color;
        ctx.fillText(line, inset, top);
      });
      y += leading;
    }
  }

  const masthead = 70;
  const artHeight = art ? 264 : 0;
  y = masthead + artHeight + 23;
  block(pass.title, 'bold 38px Georgia, serif', 46);
  y += 9;
  if (pass.date) block(pass.date, 'bold 19px Arial, sans-serif', 25);
  if (pass.venue) block(pass.venue, '18px Arial, sans-serif', 24);
  y += 25;
  const tear = y;
  const qrSize = 202;
  y += 24;
  const qrTop = y;
  const stubWidth = content - qrSize - 18;
  for (const detail of pass.details) block(detail, 'bold 20px Arial, sans-serif', 27, ink, stubWidth);
  y += 10;
  block(pass.instruction, '16px Arial, sans-serif', 21, ink, stubWidth);
  y = Math.max(y, qrTop + qrSize) + 12;
  block(pass.code, '16px monospace', 21);
  y += 22;
  const policyTop = y;
  for (const policy of pass.policies) {
    y += 18;
    block(policy.label, 'bold 15px Arial, sans-serif', 21, '#405e4c');
    block(policy.text, '16px Arial, sans-serif', 21);
  }
  const height = y + 22;

  canvas.width = width * 2;
  canvas.height = height * 2;
  ctx.scale(2, 2);
  ctx.textBaseline = 'top';
  ctx.fillStyle = paper;
  ctx.fillRect(0, 0, width, height);
  ctx.fillStyle = ink;
  ctx.fillRect(0, 0, width, masthead + artHeight);
  if (logo) ctx.drawImage(logo, inset, 18, 35, 35);
  ctx.fillStyle = paper;
  ctx.font = 'bold 19px Arial, sans-serif';
  ctx.fillText('TDF RECORDS', logo ? 75 : inset, 25);
  ctx.fillStyle = accent;
  ctx.font = 'bold 14px monospace';
  ctx.textAlign = 'right';
  ctx.fillText(pass.serial, width - inset, 28);
  ctx.textAlign = 'left';
  if (art) {
    // Show the complete flyer, including lettering at its edges.
    const ratio = Math.min(width / art.naturalWidth, artHeight / art.naturalHeight);
    const w = art.naturalWidth * ratio;
    const h = art.naturalHeight * ratio;
    ctx.drawImage(art, (width - w) / 2, masthead + (artHeight - h) / 2, w, h);
  }
  ctx.fillStyle = accent;
  ctx.fillRect(0, masthead + artHeight, width, 5);
  ctx.fillStyle = '#ffffff';
  ctx.fillRect(0, tear, width, policyTop - tear);
  ctx.strokeStyle = '#8a9d90';
  ctx.setLineDash([5, 5]);
  ctx.beginPath();
  ctx.moveTo(0, tear);
  ctx.lineTo(width, tear);
  ctx.stroke();
  ctx.imageSmoothingEnabled = false;
  ctx.drawImage(qr, width - inset - qrSize, qrTop, qrSize, qrSize);
  for (const draw of draws) draw();
  return canvas.toDataURL('image/png');
}
