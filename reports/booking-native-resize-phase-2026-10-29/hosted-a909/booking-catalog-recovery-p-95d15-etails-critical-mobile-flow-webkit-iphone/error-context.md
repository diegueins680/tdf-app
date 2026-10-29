# Instructions

- Following Playwright test failed.
- Explain why, be concise, respect Playwright best practices.
- Provide a snippet of code with the fix, if possible.

# Test info

- Name: booking-catalog-recovery.spec.mjs >> public booking recovers an empty catalog without losing contact details @critical @mobile-flow
- Location: e2e/web/booking-catalog-recovery.spec.mjs:143:3

# Error details

```
Error: expect(received).toBeLessThanOrEqual(expected)

Expected: <= 320.3333333333333
Received:    339.640625
```

# Page snapshot

```yaml
- generic [ref=e1]:
  - link "Saltar al contenido principal" [ref=e2]:
    - /url: "#main-content"
  - generic [ref=e4]:
    - link "Saltar al contenido principal" [ref=e5]:
      - /url: "#main-content"
    - banner [ref=e6]:
      - generic [ref=e8]:
        - link "Ir a TDF" [ref=e10]:
          - /url: /tdf
          - img "TDF Records logo"
        - generic [ref=e11]:
          - link "Buscar" [ref=e12] [cursor=pointer]:
            - /url: /buscar
            - img [ref=e13]
          - link "Ingresar" [ref=e15] [cursor=pointer]:
            - /url: /login?redirect=%2Freservar%3Fservice%3Drecording-fixture
          - button "Más opciones" [ref=e16] [cursor=pointer]:
            - img [ref=e17]
    - main [ref=e19]:
      - generic [ref=e20]:
        - generic [ref=e23]:
          - generic [ref=e24]:
            - generic [ref=e25]: Agenda pública
            - generic [ref=e26]:
              - heading "Reserva un servicio con TDF" [level=4] [ref=e27]
              - generic [ref=e29]: Reserva en línea
            - paragraph [ref=e30]: Completa tus datos y agenda el horario que prefieras. Puedes reservar como invitado; crear una cuenta es opcional.
            - paragraph [ref=e31]:
              - text: "Horario del estudio:"
              - strong [ref=e32]: UTC
              - text: ". Tu zona:"
              - strong [ref=e33]: GMT-5
              - text: .
            - paragraph [ref=e34]:
              - text: Precios de referencia en
              - strong [ref=e35]: USD
              - text: ; confirmamos el total contigo antes de agendar.
            - alert [ref=e36]:
              - img [ref=e38]
              - generic [ref=e40]: "Estimado para Grabación de prueba: USD 100 / hour"
            - generic [ref=e41]:
              - generic [ref=e43]: 1. Elige tu servicio y agenda sin crear cuenta
              - generic [ref=e45]: 2. Recibe un ID de reserva
              - generic [ref=e47]: 3. Coordina ajustes por WhatsApp
            - generic [ref=e50]:
              - generic [ref=e51]:
                - heading "¿Ya tienes cuenta?" [level=6] [ref=e52]
                - paragraph [ref=e53]: Inicia sesión para completar tus datos guardados. Si no tienes cuenta, puedes crearla de forma opcional.
              - generic [ref=e54]:
                - link "Iniciar sesión" [ref=e55] [cursor=pointer]:
                  - /url: /login?redirect=%2Freservar%3Fservice%3Drecording-fixture
                - link "Crear cuenta" [ref=e56] [cursor=pointer]:
                  - /url: /login?redirect=%2Freservar%3Fservice%3Drecording-fixture&signup=1
          - generic [ref=e57]:
            - generic [ref=e59]:
              - generic [ref=e60]:
                - img [ref=e61]
                - heading "Datos de contacto" [level=6] [ref=e63]
              - paragraph [ref=e64]: Usa un correo válido para identificar la reserva. Crear una cuenta es opcional.
              - generic [ref=e65]:
                - img [ref=e66]
                - paragraph [ref=e68]: Bloque tentativo en el calendario.
              - generic [ref=e69]:
                - img [ref=e70]
                - paragraph [ref=e73]: Duración estándar de 1h (ajústala si necesitas más tiempo).
              - generic [ref=e74]:
                - img [ref=e75]
                - paragraph [ref=e77]: Añade tu WhatsApp si prefieres coordinar por ahí.
            - generic [ref=e80]:
              - generic [ref=e82]:
                - paragraph [ref=e83]: Paso 2 de 3
                - generic [ref=e85]: Horario
              - generic [ref=e90]:
                - generic [ref=e92]:
                  - generic [ref=e93]:
                    - text: Servicio
                    - generic [ref=e94]: "*"
                  - generic [ref=e95]:
                    - combobox "Servicio Grabación de prueba USD 100 / hour" [ref=e96] [cursor=pointer]:
                      - generic [ref=e97]:
                        - paragraph [ref=e98]: Grabación de prueba
                        - paragraph [ref=e99]: USD 100 / hour
                    - textbox: 11111111-1111-4111-8111-111111111111
                    - img
                    - group:
                      - generic: Servicio *
                  - paragraph [ref=e100]: "Estimado: USD 100 / hour · Moneda: USD"
                - generic [ref=e101]:
                  - generic [ref=e102]:
                    - generic [ref=e103]:
                      - text: Fecha y hora
                      - generic [ref=e104]: "*"
                    - generic [ref=e105]:
                      - textbox "Fecha y hora" [ref=e106]: 2026-10-29T03:00
                      - group:
                        - generic: Fecha y hora *
                    - paragraph [ref=e107]: "Horario del estudio: 08:00 - 22:00 (UTC). Tu zona: 03:00 - 17:00 (GMT-5). Disponible."
                  - generic [ref=e108]:
                    - button "Primer horario hoy" [ref=e109] [cursor=pointer]: Primer horario hoy
                    - button "Mañana" [ref=e110] [cursor=pointer]: Mañana
                - generic [ref=e112]:
                  - generic [ref=e113]:
                    - generic [ref=e114]: Duración (min)
                    - generic [ref=e115]:
                      - spinbutton "Duración (min)" [ref=e116]: "60"
                      - group:
                        - generic: Duración (min)
                    - paragraph [ref=e117]: Máximo 840 min con la hora elegida (cierre 22:00 (UTC)).
                  - generic [ref=e118]:
                    - button "30 min" [ref=e119] [cursor=pointer]:
                      - generic [ref=e120]: 30 min
                    - button "60 min" [ref=e121] [cursor=pointer]:
                      - generic [ref=e122]: 60 min
                    - button "90 min" [ref=e123] [cursor=pointer]:
                      - generic [ref=e124]: 90 min
                    - button "120 min" [ref=e125] [cursor=pointer]:
                      - generic [ref=e126]: 120 min
                - generic [ref=e128]:
                  - paragraph [ref=e129]: Sugerencias rápidas
                  - generic [ref=e130]:
                    - button "Thu 29 03:00 (GMT-5) · Estudio 08:00 (UTC)" [ref=e131] [cursor=pointer]:
                      - generic [ref=e132]: 03:00
                    - button "Thu 29 03:30 (GMT-5) · Estudio 08:30 (UTC)" [ref=e133] [cursor=pointer]:
                      - generic [ref=e134]: 03:30
                    - button "Thu 29 04:00 (GMT-5) · Estudio 09:00 (UTC)" [active] [ref=e135] [cursor=pointer]:
                      - generic [ref=e136]: 04:00
                    - button "Thu 29 04:30 (GMT-5) · Estudio 09:30 (UTC)" [ref=e137] [cursor=pointer]:
                      - generic [ref=e138]: 04:30
                    - button "Thu 29 05:00 (GMT-5) · Estudio 10:00 (UTC)" [ref=e139] [cursor=pointer]:
                      - generic [ref=e140]: 05:00
                    - button "Thu 29 05:30 (GMT-5) · Estudio 10:30 (UTC)" [ref=e141] [cursor=pointer]:
                      - generic [ref=e142]: 05:30
                    - button "Thu 29 06:00 (GMT-5) · Estudio 11:00 (UTC)" [ref=e143] [cursor=pointer]:
                      - generic [ref=e144]: 06:00
                    - button "Thu 29 06:30 (GMT-5) · Estudio 11:30 (UTC)" [ref=e145] [cursor=pointer]:
                      - generic [ref=e146]: 06:30
                    - button "Thu 29 07:00 (GMT-5) · Estudio 12:00 (UTC)" [ref=e147] [cursor=pointer]:
                      - generic [ref=e148]: 07:00
                    - button "Thu 29 07:30 (GMT-5) · Estudio 12:30 (UTC)" [ref=e149] [cursor=pointer]:
                      - generic [ref=e150]: 07:30
                    - button "Thu 29 08:00 (GMT-5) · Estudio 13:00 (UTC)" [ref=e151] [cursor=pointer]:
                      - generic [ref=e152]: 08:00
                    - button "Thu 29 08:30 (GMT-5) · Estudio 13:30 (UTC)" [ref=e153] [cursor=pointer]:
                      - generic [ref=e154]: 08:30
                - generic [ref=e156]:
                  - generic: Notas para el equipo
                  - generic [ref=e157]:
                    - textbox "Notas para el equipo" [ref=e158]:
                      - /placeholder: "Cuéntanos qué necesitas (ej: grabación de voz, mezcla, ensayo, DJ booth, etc.)"
                    - group:
                      - generic: Notas para el equipo
                - generic [ref=e160]:
                  - button "Volver" [ref=e161] [cursor=pointer]
                  - button "Revisar reserva" [ref=e162] [cursor=pointer]
        - region "Reseñas del servicio" [ref=e164]:
          - generic [ref=e166]:
            - generic [ref=e167]:
              - heading "Reseñas del servicio" [level=5] [ref=e168]
              - generic [ref=e169]:
                - img "Aún no hay reseñas":
                  - generic:
                    - generic:
                      - generic:
                        - img
                    - generic:
                      - generic:
                        - img
                    - generic:
                      - generic:
                        - img
                    - generic:
                      - generic:
                        - img
                    - generic:
                      - generic:
                        - img
                    - generic:
                      - generic:
                        - img
                    - generic:
                      - generic:
                        - img
                    - generic:
                      - generic:
                        - img
                    - generic:
                      - generic:
                        - img
                    - generic:
                      - generic:
                        - img
                  - generic:
                    - generic:
                      - generic:
                        - img
                    - generic:
                      - generic:
                        - img
                    - generic:
                      - generic:
                        - img
                    - generic:
                      - generic:
                        - img
                    - generic:
                      - generic:
                        - img
                    - generic:
                      - generic:
                        - img
                    - generic:
                      - generic:
                        - img
                    - generic:
                      - generic:
                        - img
                    - generic:
                      - generic:
                        - img
                    - generic:
                      - generic:
                        - img
                  - generic:
                    - generic:
                      - generic:
                        - img
                    - generic:
                      - generic:
                        - img
                    - generic:
                      - generic:
                        - img
                    - generic:
                      - generic:
                        - img
                    - generic:
                      - generic:
                        - img
                    - generic:
                      - generic:
                        - img
                    - generic:
                      - generic:
                        - img
                    - generic:
                      - generic:
                        - img
                    - generic:
                      - generic:
                        - img
                    - generic:
                      - generic:
                        - img
                  - generic:
                    - generic:
                      - generic:
                        - img
                    - generic:
                      - generic:
                        - img
                    - generic:
                      - generic:
                        - img
                    - generic:
                      - generic:
                        - img
                    - generic:
                      - generic:
                        - img
                    - generic:
                      - generic:
                        - img
                    - generic:
                      - generic:
                        - img
                    - generic:
                      - generic:
                        - img
                    - generic:
                      - generic:
                        - img
                    - generic:
                      - generic:
                        - img
                  - generic:
                    - generic:
                      - generic:
                        - img
                    - generic:
                      - generic:
                        - img
                    - generic:
                      - generic:
                        - img
                    - generic:
                      - generic:
                        - img
                    - generic:
                      - generic:
                        - img
                    - generic:
                      - generic:
                        - img
                    - generic:
                      - generic:
                        - img
                    - generic:
                      - generic:
                        - img
                    - generic:
                      - generic:
                        - img
                    - generic:
                      - generic:
                        - img
                - paragraph [ref=e170]: Aún no hay reseñas
            - alert [ref=e171]:
              - img [ref=e173]
              - generic [ref=e175]: Inicia sesión para publicar una reseña después de completar una interacción.
            - progressbar [ref=e176]:
              - img [ref=e177]
    - contentinfo [ref=e179]:
      - generic [ref=e180]:
        - complementary "TDF Mobile" [ref=e181]:
          - link "TDF Mobile / Testers" [ref=e184] [cursor=pointer]:
            - /url: /app?surface=footer
        - generic [ref=e185]:
          - generic [ref=e186]:
            - heading "TDF Records" [level=6] [ref=e187]
            - paragraph [ref=e188]: Si te atoras, te dejamos una salida clara desde esta página para que sigas avanzando.
            - generic [ref=e189]:
              - link "WhatsApp reservas" [ref=e190] [cursor=pointer]:
                - /url: https://wa.me/593984755301?text=Hola%20quiero%20reservar%20un%20servicio%20en%20TDF%20Records
              - link "Ingresar y autocompletar" [ref=e191] [cursor=pointer]:
                - /url: /login?redirect=%2Freservar%3Fservice%3Drecording-fixture
          - navigation "Explorar TDF" [ref=e192]:
            - generic [ref=e193]: Explorar
            - generic [ref=e194]:
              - link "Buscar" [ref=e195] [cursor=pointer]:
                - /url: /buscar
              - link "Eventos" [ref=e196] [cursor=pointer]:
                - /url: /social/eventos
              - link "TDF" [ref=e197] [cursor=pointer]:
                - /url: /tdf
              - link "TDF Mobile" [ref=e198] [cursor=pointer]:
                - /url: /app?surface=nav
              - link "Servicios" [ref=e199] [cursor=pointer]:
                - /url: /comercio
              - link "Comunidad" [ref=e200] [cursor=pointer]:
                - /url: /fans
              - link "Tienda" [ref=e201] [cursor=pointer]:
                - /url: /marketplace
              - link "Domo" [ref=e202] [cursor=pointer]:
                - /url: /domo-del-pululahua
              - link "Reservar" [ref=e203] [cursor=pointer]:
                - /url: /reservar
              - link "Lanzamientos" [ref=e204] [cursor=pointer]:
                - /url: /records
              - link "Sugerencias" [ref=e205] [cursor=pointer]:
                - /url: /feedback
              - link "Donar" [ref=e206] [cursor=pointer]:
                - /url: /donar
          - generic [ref=e207]:
            - generic [ref=e208]: Gestión de mensajes
            - generic [ref=e209]:
              - link "Consentimiento WhatsApp" [ref=e210] [cursor=pointer]:
                - /url: /whatsapp/consentimiento
              - link "Confirmación WhatsApp" [ref=e211] [cursor=pointer]:
                - /url: /whatsapp/ok
          - link "Ir a login" [ref=e213] [cursor=pointer]:
            - /url: /login?redirect=%2Freservar%3Fservice%3Drecording-fixture
  - tooltip "Thu 29 04:00 (GMT-5) · Estudio 09:00 (UTC)" [ref=e214]:
    - generic [ref=e215]: Thu 29 04:00 (GMT-5) · Estudio 09:00 (UTC)
```

# Test source

```ts
  182 |         }] } });
  183 |       }
  184 |       if (path.includes('/engineers')) return route.fulfill({ json: [] });
  185 |       if (path.includes('/reviews')) return route.fulfill({ json: { items: [], summary: { count: 0, average: 0 }, nextCursor: null } });
  186 |       if (path.includes('/availability')) return route.fulfill({ json: { available: true } });
  187 |       return route.fulfill({ status: 404, json: { error: 'No fixture for this API' } });
  188 |     });
  189 |     await page.goto('/reservar?service=recording-fixture', { waitUntil: 'domcontentloaded' });
  190 |     const name = page.getByRole('textbox', { name: 'Nombre completo', exact: true });
  191 |     const email = page.getByRole('textbox', { name: 'Correo', exact: true });
  192 |     await name.fill('Visitante de prueba');
  193 |     await email.fill('visitante@example.test');
  194 |     // SessionQueryProvider retries 503s three times (1s, 2s, 4s). Begin the
  195 |     // UI assertion after the final synthetic response, rather than spending
  196 |     // its 8s budget on that unrelated backoff. The case keeps its 30s budget.
  197 |     const initialFailure = failure === 'error' ? page.waitForResponse(response =>
  198 |       new URL(response.url()).pathname.replace(/^\/api(?=\/)/, '') === '/services/catalog/public'
  199 |       && response.status() === 503 && requests === 4) : null;
  200 |     releaseInitial();
  201 |     if (initialFailure) await initialFailure;
  202 |     const warning = page.getByRole('alert').filter({ hasText: 'No pudimos cargar el catálogo de servicios' });
  203 |     await expect(warning).toBeVisible();
  204 |     await expect(name).toBeDisabled();
  205 |     const retryButton = warning.getByRole('button', { name: 'Reintentar catálogo', exact: true });
  206 |     await expect(retryButton).toBeEnabled();
  207 |     await page.addScriptTag({ content: axe.source });
  208 |     const violations = await warning.evaluate(async element => (await globalThis.axe.run(element, {
  209 |       runOnly: { type: 'tag', values: ['wcag2a', 'wcag2aa', 'wcag21aa', 'wcag22aa'] },
  210 |     })).violations.filter(v => ['serious', 'critical'].includes(v.impact)).map(v => ({ id: v.id, impact: v.impact })));
  211 |     expect(violations).toEqual([]);
  212 |     const before = requests;
  213 |     recovered = true;
  214 |     await retryButton.focus();
  215 |     await retryButton.press('Enter');
  216 |     await expect(warning.getByRole('button', { name: 'Cargando catálogo…', exact: true })).toBeDisabled();
  217 |     await expect(name).toBeDisabled();
  218 |     await expect.poll(() => requests).toBe(before + 1);
  219 |     expect(writes).toEqual([]);
  220 |     releaseRetry();
  221 |     await expect(warning).toHaveCount(0);
  222 |     await expect(name).toBeEnabled();
  223 |     await expect(name).toHaveValue('Visitante de prueba');
  224 |     await expect(email).toHaveValue('visitante@example.test');
  225 |     await expect(page.getByRole('combobox', { name: /^Servicio\b/ })).toContainText('Grabación de prueba');
  226 |     await page.getByRole('button', { name: 'Continuar', exact: true }).click();
  227 |     await expect(page.getByLabel(/^Fecha y hora/)).toBeVisible();
  228 |     const slots = page.getByText('Sugerencias rápidas', { exact: true }).locator('..').getByRole('button');
  229 |     expect(await slots.count()).toBeGreaterThanOrEqual(3);
  230 |     const slot = slots.nth(2);
  231 |     // Reach the real control by keyboard, with the pointer away. Focus remains
  232 |     // across resize, so a closing hover tooltip cannot hide the defect.
  233 |     await page.mouse.move(0, 0);
  234 |     // Start at the date field in this step, then follow its real Tab order to
  235 |     // the suggestion; avoid an unrelated tour through the header and footer.
  236 |     const dateInput = page.getByLabel(/^Fecha y hora/);
  237 |     await dateInput.focus();
  238 |     await expect(dateInput).toBeFocused();
  239 |     for (let tabs = 0; tabs < 30; tabs++) {
  240 |       await page.keyboard.press('Tab');
  241 |       if (await slot.evaluate(element => element === document.activeElement)) break;
  242 |     }
  243 |     await expect(slot).toBeFocused();
  244 |     const helperText = await slot.getAttribute('aria-label');
  245 |     expect(helperText).toBeTruthy();
  246 |     const tooltip = page.getByRole('tooltip').filter({ hasText: helperText });
  247 |     await expect(tooltip).toBeVisible();
  248 |     await expect(tooltip).toHaveText(helperText);
  249 |     await expect(slot).toHaveAccessibleName(helperText);
  250 |     await observeBookingViewportEvents(page);
  251 |     await page.setViewportSize({ width: 320, height: 740 });
  252 |     // The mobile marker exists at both390 and320px. Bind the measurement to
  253 |     // the requested content viewport, without waiting for overflow to vanish.
  254 |     await expect(page.getByText('Paso 2 de 3', { exact: true })).toBeVisible();
  255 |     await expect.poll(() => page.evaluate(() => {
  256 |       const width = document.documentElement.clientWidth;
  257 |       return width > 0 && width <= 320;
  258 |     })).toBe(true);
  259 |     const viewportMeasurement = await measureBookingViewport(page);
  260 |     const epsilon = 1 / viewportMeasurement.devicePixelRatio;
  261 |     await testInfo.attach('booking-initial-viewport', {
  262 |       body: Buffer.from(JSON.stringify(viewportMeasurement, null, 2)), contentType: 'application/json',
  263 |     });
  264 |     const helper = viewportMeasurement.tooltip;
  265 |     const initialBoundFailure = viewportMeasurement.overflow || !helper || helper.text !== helperText
  266 |       || helper.x < -epsilon || helper.right > viewportMeasurement.clientWidth + epsilon
  267 |       || Math.abs(viewportMeasurement.visualViewport.scale - 1) >= 0.0005;
  268 |     if (initialBoundFailure) await testInfo.attach('booking-viewport-geometry', {
  269 |       body: Buffer.from(JSON.stringify({ requestedViewport: page.viewportSize(), initial: viewportMeasurement,
  270 |         samples: await failedViewportGeometry(page) }, null, 2)),
  271 |       contentType: 'application/json',
  272 |     });
  273 |     await page.evaluate(() => window.__stopBookingViewportObservation?.());
  274 |     expect(viewportMeasurement.compatMode).toBe('CSS1Compat');
  275 |     expect(viewportMeasurement.overflow).toBe(false);
  276 |     // Root overflow has zero tolerance. Overlay edges allow one physical pixel
  277 |     // for Popper's device-pixel rounding, independently of its implementation.
  278 |     expect(viewportMeasurement.visualViewport.scale).toBeCloseTo(1, 3);
  279 |     expect(viewportMeasurement.tooltip).not.toBeNull();
  280 |     expect(viewportMeasurement.tooltip.text).toBe(helperText);
  281 |     expect(viewportMeasurement.tooltip.x).toBeGreaterThanOrEqual(-epsilon);
> 282 |     expect(viewportMeasurement.tooltip.right).toBeLessThanOrEqual(viewportMeasurement.clientWidth + epsilon);
      |                                               ^ Error: expect(received).toBeLessThanOrEqual(expected)
  283 |     await expect(slot).toBeFocused();
  284 |     await expect(slot).toHaveAccessibleName(helperText);
  285 |     // Additional scroll verification follows the immutable initial assertion.
  286 |     // Scroll-event acknowledgment never polls overflow until it disappears.
  287 |     // Reflow can move the focused control below the viewport. Establish the
  288 |     // visible-anchor precondition for this additional scroll check only after
  289 |     // the immutable resize assertion has passed.
  290 |     await slot.evaluate(async element => {
  291 |       element.scrollIntoView({ block: 'center', inline: 'nearest', behavior: 'instant' });
  292 |       await Promise.race([new Promise(resolve => requestAnimationFrame(resolve)),
  293 |         new Promise(resolve => setTimeout(resolve, 250))]);
  294 |     });
  295 |     await expect(slot).toBeInViewport({ ratio: 1 });
  296 |     const beforeScroll = await measureBookingViewport(page);
  297 |     await page.evaluate(() => new Promise((resolve, reject) => {
  298 |       // Popper updates from scroll events in a microtask. A frame callback can
  299 |       // run before that event; observe its delivery and the following task.
  300 |       const onScroll = () => {
  301 |         clearTimeout(timeout);
  302 |         setTimeout(resolve, 0);
  303 |       };
  304 |       const timeout = setTimeout(() => {
  305 |         removeEventListener('scroll', onScroll);
  306 |         reject(new Error('Booking tracking fixture did not receive a scroll event'));
  307 |       }, 8000);
  308 |       addEventListener('scroll', onScroll, { once: true, passive: true });
  309 |       scrollBy(0, 40);
  310 |     }));
  311 |     const afterScroll = await measureBookingViewport(page);
  312 |     await testInfo.attach('booking-tooltip-scroll', {
  313 |       body: Buffer.from(JSON.stringify({ beforeScroll, afterScroll, epsilon }, null, 2)),
  314 |       contentType: 'application/json',
  315 |     });
  316 |     expect(afterScroll.scrollY - beforeScroll.scrollY).toBeGreaterThan(10);
  317 |     expect(afterScroll.anchor.y - beforeScroll.anchor.y).toBeLessThan(-10);
  318 |     expect(afterScroll.anchor.y).toBeGreaterThanOrEqual(0);
  319 |     expect(afterScroll.anchor.y + afterScroll.anchor.height).toBeLessThan(740);
  320 |     expect(afterScroll.tooltip).not.toBeNull();
  321 |     expect(afterScroll.tooltip.text).toBe(helperText);
  322 |     if (afterScroll.tooltip.placement === beforeScroll.tooltip.placement) {
  323 |       const anchorDelta = afterScroll.anchor.y - beforeScroll.anchor.y;
  324 |       const tooltipDelta = afterScroll.tooltip.y - beforeScroll.tooltip.y;
  325 |       expect(Math.abs(anchorDelta - tooltipDelta)).toBeLessThanOrEqual(epsilon + 0.05);
  326 |     }
  327 |     expect(afterScroll.overflow).toBe(false);
  328 |     expect(afterScroll.visualViewport.scale).toBeCloseTo(1, 3);
  329 |     expect(afterScroll.tooltip.x).toBeGreaterThanOrEqual(-epsilon);
  330 |     expect(afterScroll.tooltip.right).toBeLessThanOrEqual(afterScroll.clientWidth + epsilon);
  331 |     await expect(slot).toBeFocused();
  332 |     await expect(slot).toHaveAccessibleName(helperText);
  333 |     expect(errors).toEqual([]);
  334 |     expect(writes).toEqual([]);
  335 |     await page.screenshot({ path: testInfo.outputPath('booking-catalog-recovered.png'), fullPage: true });
  336 |   });
  337 | }
  338 | 
```