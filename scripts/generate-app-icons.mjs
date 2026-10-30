#!/usr/bin/env node
import { readFile, writeFile } from 'node:fs/promises';
import { fileURLToPath } from 'node:url';
import { chromium } from '@playwright/test';

// Reuse the official logo's existing symbol. Do not redraw it with text,
// fonts or an approximation: the same SVG is the Mobile release source.
const root = new URL('../', import.meta.url);
const source = await readFile(new URL('tdf-hq-ui/src/assets/tdf-isotype.svg', root), 'utf8');
const destination = new URL('tdf-hq-ui/public/', root);
const browser = await chromium.launch({ headless: true });

try {
  const page = await browser.newPage({ deviceScaleFactor: 1 });
  await page.setContent(source);
  const svg = page.locator('svg');
  const cropped = await svg.evaluate((element) => {
    const box = element.viewBox.baseVal;
    // A 60% symbol diameter leaves the same safe padding as Mobile and fits
    // inside the 80% safe circle used by maskable launchers.
    const side = Math.max(box.width, box.height) / 0.6;
    const x = box.x + (box.width - side) / 2;
    const y = box.y + (box.height - side) / 2;
    element.setAttribute('viewBox', `${x} ${y} ${side} ${side}`);
    element.setAttribute('width', '1024');
    element.setAttribute('height', '1024');
    element.setAttribute('role', 'img');
    element.setAttribute('aria-label', 'TDF Records');
    for (const path of element.querySelectorAll('path')) {
      path.setAttribute('fill', '#f8fafc');
    }
    const background = document.createElementNS('http://www.w3.org/2000/svg', 'rect');
    for (const [key, value] of Object.entries({ x, y, width: side, height: side, fill: '#0f172a' })) {
      background.setAttribute(key, String(value));
    }
    element.prepend(background);
    return element.outerHTML;
  });
  await writeFile(new URL('favicon.svg', destination), `${cropped}\n`);

  const sizes = new Map([
    ['tdf-app-icon-1024.png', 1024],
    ['tdf-app-icon-512.png', 512],
    ['tdf-app-icon-192.png', 192],
    ['apple-touch-icon.png', 180],
    ['favicon-32.png', 32],
    ['favicon-16.png', 16],
  ]);
  for (const [name, size] of sizes) {
    await page.setViewportSize({ width: size, height: size });
    await page.setContent(`<style>html,body{margin:0}svg{display:block;width:${size}px;height:${size}px}</style>${cropped}`);
    await page.locator('svg').screenshot({ path: fileURLToPath(new URL(name, destination)) });
  }

  // ICO supports PNG payloads. Keep a 64px fallback for browsers that use ICO.
  await page.setViewportSize({ width: 64, height: 64 });
  await page.setContent(`<style>html,body{margin:0}svg{display:block;width:64px;height:64px}</style>${cropped}`);
  const png = await page.locator('svg').screenshot();
  const header = Buffer.alloc(22);
  header.writeUInt16LE(1, 2);
  header.writeUInt16LE(1, 4);
  header[6] = 64;
  header[7] = 64;
  header.writeUInt16LE(1, 10);
  header.writeUInt16LE(32, 12);
  header.writeUInt32LE(png.length, 14);
  header.writeUInt32LE(header.length, 18);
  await writeFile(new URL('favicon.ico', destination), Buffer.concat([header, png]));
  console.log(`Generated ${sizes.size + 2} official TDF Records icons.`);
} finally {
  await browser.close();
}
