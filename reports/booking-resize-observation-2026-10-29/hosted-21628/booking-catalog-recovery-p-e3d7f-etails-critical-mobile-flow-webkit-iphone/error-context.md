# Instructions

- Following Playwright test failed.
- Explain why, be concise, respect Playwright best practices.
- Provide a snippet of code with the fix, if possible.

# Test info

- Name: booking-catalog-recovery.spec.mjs >> public booking recovers an error catalog without losing contact details @critical @mobile-flow
- Location: e2e/web/booking-catalog-recovery.spec.mjs:90:3

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
  120 |         await retry;
  121 |         return route.fulfill({ json: { sceSchemaVersion: 1, sceRevision: 1, sceLocale: 'es', sceItems: [{
  122 |           scId: '11111111-1111-4111-8111-111111111111', scCode: 'recording-fixture',
  123 |           scName: 'Grabación de prueba', scNameEs: 'Grabación de prueba', scNameEn: 'Test recording',
  124 |           scCategoryId: '33333333-3333-4333-8333-333333333333', scKind: 'recording',
  125 |           scPricingModelId: '44444444-4444-4444-8444-444444444444', scPricingModel: 'hourly',
  126 |           scRateCents: 10000, scCurrency: 'USD', scCurrencyId: '55555555-5555-4555-8555-555555555555',
  127 |           scBillingUnit: 'hour', scDefaultDurationMinutes: 60, scRequiresEngineer: false,
  128 |           scDefaultResources: [], scSortOrder: 0, scActive: true,
  129 |         }] } });
  130 |       }
  131 |       if (path.includes('/engineers')) return route.fulfill({ json: [] });
  132 |       if (path.includes('/reviews')) return route.fulfill({ json: { items: [], summary: { count: 0, average: 0 }, nextCursor: null } });
  133 |       if (path.includes('/availability')) return route.fulfill({ json: { available: true } });
  134 |       return route.fulfill({ status: 404, json: { error: 'No fixture for this API' } });
  135 |     });
  136 |     await page.goto('/reservar?service=recording-fixture', { waitUntil: 'domcontentloaded' });
  137 |     const name = page.getByRole('textbox', { name: 'Nombre completo', exact: true });
  138 |     const email = page.getByRole('textbox', { name: 'Correo', exact: true });
  139 |     await name.fill('Visitante de prueba');
  140 |     await email.fill('visitante@example.test');
  141 |     // SessionQueryProvider retries 503s three times (1s, 2s, 4s). Begin the
  142 |     // UI assertion after the final synthetic response, rather than spending
  143 |     // its 8s budget on that unrelated backoff. The case keeps its 30s budget.
  144 |     const initialFailure = failure === 'error' ? page.waitForResponse(response =>
  145 |       new URL(response.url()).pathname.replace(/^\/api(?=\/)/, '') === '/services/catalog/public'
  146 |       && response.status() === 503 && requests === 4) : null;
  147 |     releaseInitial();
  148 |     if (initialFailure) await initialFailure;
  149 |     const warning = page.getByRole('alert').filter({ hasText: 'No pudimos cargar el catálogo de servicios' });
  150 |     await expect(warning).toBeVisible();
  151 |     await expect(name).toBeDisabled();
  152 |     const retryButton = warning.getByRole('button', { name: 'Reintentar catálogo', exact: true });
  153 |     await expect(retryButton).toBeEnabled();
  154 |     await page.addScriptTag({ content: axe.source });
  155 |     const violations = await warning.evaluate(async element => (await globalThis.axe.run(element, {
  156 |       runOnly: { type: 'tag', values: ['wcag2a', 'wcag2aa', 'wcag21aa', 'wcag22aa'] },
  157 |     })).violations.filter(v => ['serious', 'critical'].includes(v.impact)).map(v => ({ id: v.id, impact: v.impact })));
  158 |     expect(violations).toEqual([]);
  159 |     const before = requests;
  160 |     recovered = true;
  161 |     await retryButton.focus();
  162 |     await retryButton.press('Enter');
  163 |     await expect(warning.getByRole('button', { name: 'Cargando catálogo…', exact: true })).toBeDisabled();
  164 |     await expect(name).toBeDisabled();
  165 |     await expect.poll(() => requests).toBe(before + 1);
  166 |     expect(writes).toEqual([]);
  167 |     releaseRetry();
  168 |     await expect(warning).toHaveCount(0);
  169 |     await expect(name).toBeEnabled();
  170 |     await expect(name).toHaveValue('Visitante de prueba');
  171 |     await expect(email).toHaveValue('visitante@example.test');
  172 |     await expect(page.getByRole('combobox', { name: /^Servicio\b/ })).toContainText('Grabación de prueba');
  173 |     await page.getByRole('button', { name: 'Continuar', exact: true }).click();
  174 |     await expect(page.getByLabel(/^Fecha y hora/)).toBeVisible();
  175 |     const slots = page.getByText('Sugerencias rápidas', { exact: true }).locator('..').getByRole('button');
  176 |     expect(await slots.count()).toBeGreaterThanOrEqual(3);
  177 |     const slot = slots.nth(2);
  178 |     // Reach the real control by keyboard, with the pointer away. Focus remains
  179 |     // across resize, so a closing hover tooltip cannot hide the defect.
  180 |     await page.mouse.move(0, 0);
  181 |     // Start at the date field in this step, then follow its real Tab order to
  182 |     // the suggestion; avoid an unrelated tour through the header and footer.
  183 |     const dateInput = page.getByLabel(/^Fecha y hora/);
  184 |     await dateInput.focus();
  185 |     await expect(dateInput).toBeFocused();
  186 |     for (let tabs = 0; tabs < 30; tabs++) {
  187 |       await page.keyboard.press('Tab');
  188 |       if (await slot.evaluate(element => element === document.activeElement)) break;
  189 |     }
  190 |     await expect(slot).toBeFocused();
  191 |     const helperText = await slot.getAttribute('aria-label');
  192 |     expect(helperText).toBeTruthy();
  193 |     const tooltip = page.getByRole('tooltip').filter({ hasText: helperText });
  194 |     await expect(tooltip).toBeVisible();
  195 |     await expect(tooltip).toHaveText(helperText);
  196 |     await expect(slot).toHaveAccessibleName(helperText);
  197 |     await page.setViewportSize({ width: 320, height: 740 });
  198 |     // The mobile marker exists at both390 and320px. Bind the measurement to
  199 |     // the requested content viewport, without waiting for overflow to vanish.
  200 |     await expect(page.getByText('Paso 2 de 3', { exact: true })).toBeVisible();
  201 |     await expect.poll(() => page.evaluate(() => {
  202 |       const width = document.documentElement.clientWidth;
  203 |       return width > 0 && width <= 320;
  204 |     })).toBe(true);
  205 |     const viewportMeasurement = await measureBookingViewport(page);
  206 |     if (viewportMeasurement.overflow) await testInfo.attach('booking-viewport-geometry', {
  207 |       body: Buffer.from(JSON.stringify({ requestedViewport: page.viewportSize(), initial: viewportMeasurement,
  208 |         samples: await failedViewportGeometry(page) }, null, 2)),
  209 |       contentType: 'application/json',
  210 |     });
  211 |     expect(viewportMeasurement.compatMode).toBe('CSS1Compat');
  212 |     expect(viewportMeasurement.overflow).toBe(false);
  213 |     // Root overflow has zero tolerance. Overlay edges allow one physical pixel
  214 |     // for Popper's device-pixel rounding, independently of its implementation.
  215 |     const epsilon = 1 / viewportMeasurement.devicePixelRatio;
  216 |     expect(viewportMeasurement.visualViewport.scale).toBeCloseTo(1, 3);
  217 |     expect(viewportMeasurement.tooltip).not.toBeNull();
  218 |     expect(viewportMeasurement.tooltip.text).toBe(helperText);
  219 |     expect(viewportMeasurement.tooltip.x).toBeGreaterThanOrEqual(-epsilon);
> 220 |     expect(viewportMeasurement.tooltip.right).toBeLessThanOrEqual(viewportMeasurement.clientWidth + epsilon);
      |                                               ^ Error: expect(received).toBeLessThanOrEqual(expected)
  221 |     await expect(slot).toBeFocused();
  222 |     await expect(slot).toHaveAccessibleName(helperText);
  223 |     // Additional scroll verification follows the immutable initial assertion.
  224 |     // Scroll-event acknowledgment never polls overflow until it disappears.
  225 |     // Reflow can move the focused control below the viewport. Establish the
  226 |     // visible-anchor precondition for this additional scroll check only after
  227 |     // the immutable resize assertion has passed.
  228 |     await slot.evaluate(async element => {
  229 |       element.scrollIntoView({ block: 'center', inline: 'nearest', behavior: 'instant' });
  230 |       await Promise.race([new Promise(resolve => requestAnimationFrame(resolve)),
  231 |         new Promise(resolve => setTimeout(resolve, 250))]);
  232 |     });
  233 |     await expect(slot).toBeInViewport({ ratio: 1 });
  234 |     const beforeScroll = await measureBookingViewport(page);
  235 |     await page.evaluate(() => new Promise((resolve, reject) => {
  236 |       // Popper updates from scroll events in a microtask. A frame callback can
  237 |       // run before that event; observe its delivery and the following task.
  238 |       const onScroll = () => {
  239 |         clearTimeout(timeout);
  240 |         setTimeout(resolve, 0);
  241 |       };
  242 |       const timeout = setTimeout(() => {
  243 |         removeEventListener('scroll', onScroll);
  244 |         reject(new Error('Booking tracking fixture did not receive a scroll event'));
  245 |       }, 8000);
  246 |       addEventListener('scroll', onScroll, { once: true, passive: true });
  247 |       scrollBy(0, 40);
  248 |     }));
  249 |     const afterScroll = await measureBookingViewport(page);
  250 |     await testInfo.attach('booking-tooltip-scroll', {
  251 |       body: Buffer.from(JSON.stringify({ beforeScroll, afterScroll, epsilon }, null, 2)),
  252 |       contentType: 'application/json',
  253 |     });
  254 |     expect(afterScroll.scrollY - beforeScroll.scrollY).toBeGreaterThan(10);
  255 |     expect(afterScroll.anchor.y - beforeScroll.anchor.y).toBeLessThan(-10);
  256 |     expect(afterScroll.anchor.y).toBeGreaterThanOrEqual(0);
  257 |     expect(afterScroll.anchor.y + afterScroll.anchor.height).toBeLessThan(740);
  258 |     expect(afterScroll.tooltip).not.toBeNull();
  259 |     expect(afterScroll.tooltip.text).toBe(helperText);
  260 |     if (afterScroll.tooltip.placement === beforeScroll.tooltip.placement) {
  261 |       const anchorDelta = afterScroll.anchor.y - beforeScroll.anchor.y;
  262 |       const tooltipDelta = afterScroll.tooltip.y - beforeScroll.tooltip.y;
  263 |       expect(Math.abs(anchorDelta - tooltipDelta)).toBeLessThanOrEqual(epsilon + 0.05);
  264 |     }
  265 |     expect(afterScroll.overflow).toBe(false);
  266 |     expect(afterScroll.visualViewport.scale).toBeCloseTo(1, 3);
  267 |     expect(afterScroll.tooltip.x).toBeGreaterThanOrEqual(-epsilon);
  268 |     expect(afterScroll.tooltip.right).toBeLessThanOrEqual(afterScroll.clientWidth + epsilon);
  269 |     await expect(slot).toBeFocused();
  270 |     await expect(slot).toHaveAccessibleName(helperText);
  271 |     expect(errors).toEqual([]);
  272 |     expect(writes).toEqual([]);
  273 |     await page.screenshot({ path: testInfo.outputPath('booking-catalog-recovered.png'), fullPage: true });
  274 |   });
  275 | }
  276 | 
```