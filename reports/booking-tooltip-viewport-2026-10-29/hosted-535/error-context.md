# Instructions

- Following Playwright test failed.
- Explain why, be concise, respect Playwright best practices.
- Provide a snippet of code with the fix, if possible.

# Test info

- Name: booking-catalog-recovery.spec.mjs >> public booking recovers an error catalog without losing contact details @critical @mobile-flow
- Location: e2e/web/booking-catalog-recovery.spec.mjs:79:3

# Error details

```
Error: expect(received).toBe(expected) // Object.is equality

Expected: false
Received: true
```

# Page snapshot

```yaml
- generic [active] [ref=e1]:
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
                    - button "Primer horario hoy" [ref=e109] [cursor=pointer]
                    - button "Mañana" [ref=e110] [cursor=pointer]
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
                    - button "Thu 29 04:00 (GMT-5) · Estudio 09:00 (UTC)" [ref=e135] [cursor=pointer]:
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
```

# Test source

```ts
  76  | // All business/session APIs are fixtures. Only local static assets reach the
  77  | // preview server; no reservation, provider request or external traffic is sent.
  78  | for (const failure of ['error', 'empty']) {
  79  |   test(`public booking recovers an ${failure} catalog without losing contact details @critical @mobile-flow`, async ({ page, baseURL }, testInfo) => {
  80  |     const origin = new URL(baseURL).origin;
  81  |     const errors = [], writes = [];
  82  |     let requests = 0, recovered = false, releaseInitial, releaseRetry;
  83  |     const initial = new Promise(resolve => { releaseInitial = resolve; });
  84  |     const retry = new Promise(resolve => { releaseRetry = resolve; });
  85  |     page.on('pageerror', error => errors.push(error.message));
  86  |     await page.addInitScript(() => localStorage.setItem('tdf-hq-ui/locale', 'es'));
  87  |     await page.route('**/*', async route => {
  88  |       const request = route.request(), url = new URL(request.url());
  89  |       if (url.origin !== origin && !/(^|\.)tdfrecords\.net$/.test(url.hostname)) return route.abort();
  90  |       if (request.method() !== 'GET') {
  91  |         writes.push({ method: request.method(), path: url.pathname });
  92  |         return route.abort();
  93  |       }
  94  |       if (!['fetch', 'xhr'].includes(request.resourceType())) return url.origin === origin ? route.continue() : route.abort();
  95  |       const path = url.pathname.replace(/^\/api(?=\/)/, '');
  96  |       if (path === '/session') return route.fulfill({ json: null });
  97  |       if (path === '/health') return route.fulfill({ json: { status: 'ok', db: 'ok' } });
  98  |       if (path === '/services/catalog/public') {
  99  |         requests++;
  100 |         if (!recovered) {
  101 |           await initial;
  102 |           return failure === 'error'
  103 |             ? route.fulfill({ status: 503, json: { error: 'Synthetic catalog outage' } })
  104 |             : route.fulfill({ json: { sceItems: [] } });
  105 |         }
  106 |         await retry;
  107 |         return route.fulfill({ json: { sceSchemaVersion: 1, sceRevision: 1, sceLocale: 'es', sceItems: [{
  108 |           scId: '11111111-1111-4111-8111-111111111111', scCode: 'recording-fixture',
  109 |           scName: 'Grabación de prueba', scNameEs: 'Grabación de prueba', scNameEn: 'Test recording',
  110 |           scCategoryId: '33333333-3333-4333-8333-333333333333', scKind: 'recording',
  111 |           scPricingModelId: '44444444-4444-4444-8444-444444444444', scPricingModel: 'hourly',
  112 |           scRateCents: 10000, scCurrency: 'USD', scCurrencyId: '55555555-5555-4555-8555-555555555555',
  113 |           scBillingUnit: 'hour', scDefaultDurationMinutes: 60, scRequiresEngineer: false,
  114 |           scDefaultResources: [], scSortOrder: 0, scActive: true,
  115 |         }] } });
  116 |       }
  117 |       if (path.includes('/engineers')) return route.fulfill({ json: [] });
  118 |       if (path.includes('/reviews')) return route.fulfill({ json: { items: [], summary: { count: 0, average: 0 }, nextCursor: null } });
  119 |       if (path.includes('/availability')) return route.fulfill({ json: { available: true } });
  120 |       return route.fulfill({ status: 404, json: { error: 'No fixture for this API' } });
  121 |     });
  122 |     await page.goto('/reservar?service=recording-fixture', { waitUntil: 'domcontentloaded' });
  123 |     const name = page.getByRole('textbox', { name: 'Nombre completo', exact: true });
  124 |     const email = page.getByRole('textbox', { name: 'Correo', exact: true });
  125 |     await name.fill('Visitante de prueba');
  126 |     await email.fill('visitante@example.test');
  127 |     // SessionQueryProvider retries 503s three times (1s, 2s, 4s). Begin the
  128 |     // UI assertion after the final synthetic response, rather than spending
  129 |     // its 8s budget on that unrelated backoff. The case keeps its 30s budget.
  130 |     const initialFailure = failure === 'error' ? page.waitForResponse(response =>
  131 |       new URL(response.url()).pathname.replace(/^\/api(?=\/)/, '') === '/services/catalog/public'
  132 |       && response.status() === 503 && requests === 4) : null;
  133 |     releaseInitial();
  134 |     if (initialFailure) await initialFailure;
  135 |     const warning = page.getByRole('alert').filter({ hasText: 'No pudimos cargar el catálogo de servicios' });
  136 |     await expect(warning).toBeVisible();
  137 |     await expect(name).toBeDisabled();
  138 |     const retryButton = warning.getByRole('button', { name: 'Reintentar catálogo', exact: true });
  139 |     await expect(retryButton).toBeEnabled();
  140 |     await page.addScriptTag({ content: axe.source });
  141 |     const violations = await warning.evaluate(async element => (await globalThis.axe.run(element, {
  142 |       runOnly: { type: 'tag', values: ['wcag2a', 'wcag2aa', 'wcag21aa', 'wcag22aa'] },
  143 |     })).violations.filter(v => ['serious', 'critical'].includes(v.impact)).map(v => ({ id: v.id, impact: v.impact })));
  144 |     expect(violations).toEqual([]);
  145 |     const before = requests;
  146 |     recovered = true;
  147 |     await retryButton.focus();
  148 |     await retryButton.press('Enter');
  149 |     await expect(warning.getByRole('button', { name: 'Cargando catálogo…', exact: true })).toBeDisabled();
  150 |     await expect(name).toBeDisabled();
  151 |     await expect.poll(() => requests).toBe(before + 1);
  152 |     expect(writes).toEqual([]);
  153 |     releaseRetry();
  154 |     await expect(warning).toHaveCount(0);
  155 |     await expect(name).toBeEnabled();
  156 |     await expect(name).toHaveValue('Visitante de prueba');
  157 |     await expect(email).toHaveValue('visitante@example.test');
  158 |     await expect(page.getByRole('combobox', { name: /^Servicio\b/ })).toContainText('Grabación de prueba');
  159 |     await page.getByRole('button', { name: 'Continuar', exact: true }).click();
  160 |     await expect(page.getByLabel(/^Fecha y hora/)).toBeVisible();
  161 |     await page.setViewportSize({ width: 320, height: 740 });
  162 |     // The mobile marker exists at both390 and320px. Bind the measurement to
  163 |     // the requested content viewport, without waiting for overflow to vanish.
  164 |     await expect(page.getByText('Paso 2 de 3', { exact: true })).toBeVisible();
  165 |     await expect.poll(() => page.evaluate(() => {
  166 |       const width = document.documentElement.clientWidth;
  167 |       return width > 0 && width <= 320;
  168 |     })).toBe(true);
  169 |     const viewportMeasurement = await measureBookingViewport(page);
  170 |     if (viewportMeasurement.overflow) await testInfo.attach('booking-viewport-geometry', {
  171 |       body: Buffer.from(JSON.stringify({ requestedViewport: page.viewportSize(), initial: viewportMeasurement,
  172 |         samples: await failedViewportGeometry(page) }, null, 2)),
  173 |       contentType: 'application/json',
  174 |     });
  175 |     expect(viewportMeasurement.compatMode).toBe('CSS1Compat');
> 176 |     expect(viewportMeasurement.overflow).toBe(false);
      |                                          ^ Error: expect(received).toBe(expected) // Object.is equality
  177 |     expect(errors).toEqual([]);
  178 |     expect(writes).toEqual([]);
  179 |     await page.screenshot({ path: testInfo.outputPath('booking-catalog-recovered.png'), fullPage: true });
  180 |   });
  181 | }
  182 | 
```