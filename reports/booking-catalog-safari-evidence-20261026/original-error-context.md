# Instructions

- Following Playwright test failed.
- Explain why, be concise, respect Playwright best practices.
- Provide a snippet of code with the fix, if possible.

# Test info

- Name: booking-catalog-recovery.spec.mjs >> public booking recovers an error catalog without losing contact details @critical @mobile-flow
- Location: e2e/web/booking-catalog-recovery.spec.mjs:7:3

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
              - generic [ref=e81]:
                - generic [ref=e83]:
                  - img [ref=e85]
                  - generic [ref=e88]: Contacto
                - generic [ref=e92]:
                  - img [ref=e94]:
                    - generic [ref=e96]: "2"
                  - generic [ref=e98]: Horario
                - generic [ref=e102]:
                  - img [ref=e104]:
                    - generic [ref=e106]: "3"
                  - generic [ref=e108]: Confirmación
              - generic [ref=e109]:
                - generic [ref=e111]:
                  - generic [ref=e112]:
                    - text: Servicio
                    - generic [ref=e113]: "*"
                  - generic [ref=e114]:
                    - combobox "Servicio Grabación de prueba USD 100 / hour" [ref=e115] [cursor=pointer]:
                      - generic [ref=e116]:
                        - paragraph [ref=e117]: Grabación de prueba
                        - paragraph [ref=e118]: USD 100 / hour
                    - textbox: 11111111-1111-4111-8111-111111111111
                    - img
                    - group:
                      - generic: Servicio *
                  - paragraph [ref=e119]: "Estimado: USD 100 / hour · Moneda: USD"
                - generic [ref=e120]:
                  - generic [ref=e121]:
                    - generic [ref=e122]:
                      - text: Fecha y hora
                      - generic [ref=e123]: "*"
                    - generic [ref=e124]:
                      - textbox "Fecha y hora" [ref=e125]: 2026-10-27T03:00
                      - group:
                        - generic: Fecha y hora *
                    - paragraph [ref=e126]: "Horario del estudio: 08:00 - 22:00 (UTC). Tu zona: 03:00 - 17:00 (GMT-5). Disponible."
                  - generic [ref=e127]:
                    - button "Primer horario hoy" [ref=e128] [cursor=pointer]
                    - button "Mañana" [ref=e129] [cursor=pointer]
                - generic [ref=e131]:
                  - generic [ref=e132]:
                    - generic [ref=e133]: Duración (min)
                    - generic [ref=e134]:
                      - spinbutton "Duración (min)" [ref=e135]: "60"
                      - group:
                        - generic: Duración (min)
                    - paragraph [ref=e136]: Máximo 840 min con la hora elegida (cierre 22:00 (UTC)).
                  - generic [ref=e137]:
                    - button "30 min" [ref=e138] [cursor=pointer]:
                      - generic [ref=e139]: 30 min
                    - button "60 min" [ref=e140] [cursor=pointer]:
                      - generic [ref=e141]: 60 min
                    - button "90 min" [ref=e142] [cursor=pointer]:
                      - generic [ref=e143]: 90 min
                    - button "120 min" [ref=e144] [cursor=pointer]:
                      - generic [ref=e145]: 120 min
                - generic [ref=e147]:
                  - paragraph [ref=e148]: Sugerencias rápidas
                  - generic [ref=e149]:
                    - button "Tue 27 03:00 (GMT-5) · Estudio 08:00 (UTC)" [ref=e150] [cursor=pointer]:
                      - generic [ref=e151]: 03:00
                    - button "Tue 27 03:30 (GMT-5) · Estudio 08:30 (UTC)" [ref=e152] [cursor=pointer]:
                      - generic [ref=e153]: 03:30
                    - button "Tue 27 04:00 (GMT-5) · Estudio 09:00 (UTC)" [ref=e154] [cursor=pointer]:
                      - generic [ref=e155]: 04:00
                    - button "Tue 27 04:30 (GMT-5) · Estudio 09:30 (UTC)" [ref=e156] [cursor=pointer]:
                      - generic [ref=e157]: 04:30
                    - button "Tue 27 05:00 (GMT-5) · Estudio 10:00 (UTC)" [ref=e158] [cursor=pointer]:
                      - generic [ref=e159]: 05:00
                    - button "Tue 27 05:30 (GMT-5) · Estudio 10:30 (UTC)" [ref=e160] [cursor=pointer]:
                      - generic [ref=e161]: 05:30
                    - button "Tue 27 06:00 (GMT-5) · Estudio 11:00 (UTC)" [ref=e162] [cursor=pointer]:
                      - generic [ref=e163]: 06:00
                    - button "Tue 27 06:30 (GMT-5) · Estudio 11:30 (UTC)" [ref=e164] [cursor=pointer]:
                      - generic [ref=e165]: 06:30
                    - button "Tue 27 07:00 (GMT-5) · Estudio 12:00 (UTC)" [ref=e166] [cursor=pointer]:
                      - generic [ref=e167]: 07:00
                    - button "Tue 27 07:30 (GMT-5) · Estudio 12:30 (UTC)" [ref=e168] [cursor=pointer]:
                      - generic [ref=e169]: 07:30
                    - button "Tue 27 08:00 (GMT-5) · Estudio 13:00 (UTC)" [ref=e170] [cursor=pointer]:
                      - generic [ref=e171]: 08:00
                    - button "Tue 27 08:30 (GMT-5) · Estudio 13:30 (UTC)" [ref=e172] [cursor=pointer]:
                      - generic [ref=e173]: 08:30
                - generic [ref=e175]:
                  - generic: Notas para el equipo
                  - generic [ref=e176]:
                    - textbox "Notas para el equipo" [ref=e177]:
                      - /placeholder: "Cuéntanos qué necesitas (ej: grabación de voz, mezcla, ensayo, DJ booth, etc.)"
                    - group:
                      - generic: Notas para el equipo
                - generic [ref=e179]:
                  - button "Volver" [ref=e180] [cursor=pointer]
                  - button "Revisar reserva" [ref=e181] [cursor=pointer]
        - region "Reseñas del servicio" [ref=e183]:
          - generic [ref=e185]:
            - generic [ref=e186]:
              - heading "Reseñas del servicio" [level=5] [ref=e187]
              - generic [ref=e188]:
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
                - paragraph [ref=e189]: Aún no hay reseñas
            - alert [ref=e190]:
              - img [ref=e192]
              - generic [ref=e194]: Inicia sesión para publicar una reseña después de completar una interacción.
            - progressbar [ref=e195]:
              - img [ref=e196]
    - contentinfo [ref=e198]:
      - generic [ref=e199]:
        - complementary "TDF Mobile" [ref=e200]:
          - link "TDF Mobile / Testers" [ref=e203] [cursor=pointer]:
            - /url: /app?surface=footer
        - generic [ref=e204]:
          - generic [ref=e205]:
            - heading "TDF Records" [level=6] [ref=e206]
            - paragraph [ref=e207]: Si te atoras, te dejamos una salida clara desde esta página para que sigas avanzando.
            - generic [ref=e208]:
              - link "WhatsApp reservas" [ref=e209] [cursor=pointer]:
                - /url: https://wa.me/593984755301?text=Hola%20quiero%20reservar%20un%20servicio%20en%20TDF%20Records
              - link "Ingresar y autocompletar" [ref=e210] [cursor=pointer]:
                - /url: /login?redirect=%2Freservar%3Fservice%3Drecording-fixture
          - navigation "Explorar TDF" [ref=e211]:
            - generic [ref=e212]: Explorar
            - generic [ref=e213]:
              - link "Buscar" [ref=e214] [cursor=pointer]:
                - /url: /buscar
              - link "Eventos" [ref=e215] [cursor=pointer]:
                - /url: /social/eventos
              - link "TDF" [ref=e216] [cursor=pointer]:
                - /url: /tdf
              - link "TDF Mobile" [ref=e217] [cursor=pointer]:
                - /url: /app?surface=nav
              - link "Servicios" [ref=e218] [cursor=pointer]:
                - /url: /comercio
              - link "Comunidad" [ref=e219] [cursor=pointer]:
                - /url: /fans
              - link "Tienda" [ref=e220] [cursor=pointer]:
                - /url: /marketplace
              - link "Domo" [ref=e221] [cursor=pointer]:
                - /url: /domo-del-pululahua
              - link "Reservar" [ref=e222] [cursor=pointer]:
                - /url: /reservar
              - link "Lanzamientos" [ref=e223] [cursor=pointer]:
                - /url: /records
              - link "Sugerencias" [ref=e224] [cursor=pointer]:
                - /url: /feedback
              - link "Donar" [ref=e225] [cursor=pointer]:
                - /url: /donar
          - generic [ref=e226]:
            - generic [ref=e227]: Gestión de mensajes
            - generic [ref=e228]:
              - link "Consentimiento WhatsApp" [ref=e229] [cursor=pointer]:
                - /url: /whatsapp/consentimiento
              - link "Confirmación WhatsApp" [ref=e230] [cursor=pointer]:
                - /url: /whatsapp/ok
          - link "Ir a login" [ref=e232] [cursor=pointer]:
            - /url: /login?redirect=%2Freservar%3Fservice%3Drecording-fixture
  - tooltip "Tue 27 06:30 (GMT-5) · Estudio 11:30 (UTC)" [ref=e233]:
    - generic [ref=e234]: Tue 27 06:30 (GMT-5) · Estudio 11:30 (UTC)
```

# Test source

```ts
  1  | import { expect, test } from '@playwright/test';
  2  | import axe from 'axe-core';
  3  | 
  4  | // All business/session APIs are fixtures. Only local static assets reach the
  5  | // preview server; no reservation, provider request or external traffic is sent.
  6  | for (const failure of ['error', 'empty']) {
  7  |   test(`public booking recovers an ${failure} catalog without losing contact details @critical @mobile-flow`, async ({ page, baseURL }, testInfo) => {
  8  |     const origin = new URL(baseURL).origin;
  9  |     const errors = [], writes = [];
  10 |     let requests = 0, recovered = false, releaseInitial, releaseRetry;
  11 |     const initial = new Promise(resolve => { releaseInitial = resolve; });
  12 |     const retry = new Promise(resolve => { releaseRetry = resolve; });
  13 |     page.on('pageerror', error => errors.push(error.message));
  14 |     await page.addInitScript(() => localStorage.setItem('tdf-hq-ui/locale', 'es'));
  15 |     await page.route('**/*', async route => {
  16 |       const request = route.request(), url = new URL(request.url());
  17 |       if (url.origin !== origin && !/(^|\.)tdfrecords\.net$/.test(url.hostname)) return route.abort();
  18 |       if (request.method() !== 'GET') {
  19 |         writes.push({ method: request.method(), path: url.pathname });
  20 |         return route.abort();
  21 |       }
  22 |       if (!['fetch', 'xhr'].includes(request.resourceType())) return url.origin === origin ? route.continue() : route.abort();
  23 |       const path = url.pathname.replace(/^\/api(?=\/)/, '');
  24 |       if (path === '/session') return route.fulfill({ json: null });
  25 |       if (path === '/health') return route.fulfill({ json: { status: 'ok', db: 'ok' } });
  26 |       if (path === '/services/catalog/public') {
  27 |         requests++;
  28 |         if (!recovered) {
  29 |           await initial;
  30 |           return failure === 'error'
  31 |             ? route.fulfill({ status: 503, json: { error: 'Synthetic catalog outage' } })
  32 |             : route.fulfill({ json: { sceItems: [] } });
  33 |         }
  34 |         await retry;
  35 |         return route.fulfill({ json: { sceSchemaVersion: 1, sceRevision: 1, sceLocale: 'es', sceItems: [{
  36 |           scId: '11111111-1111-4111-8111-111111111111', scCode: 'recording-fixture',
  37 |           scName: 'Grabación de prueba', scNameEs: 'Grabación de prueba', scNameEn: 'Test recording',
  38 |           scCategoryId: '33333333-3333-4333-8333-333333333333', scKind: 'recording',
  39 |           scPricingModelId: '44444444-4444-4444-8444-444444444444', scPricingModel: 'hourly',
  40 |           scRateCents: 10000, scCurrency: 'USD', scCurrencyId: '55555555-5555-4555-8555-555555555555',
  41 |           scBillingUnit: 'hour', scDefaultDurationMinutes: 60, scRequiresEngineer: false,
  42 |           scDefaultResources: [], scSortOrder: 0, scActive: true,
  43 |         }] } });
  44 |       }
  45 |       if (path.includes('/engineers')) return route.fulfill({ json: [] });
  46 |       if (path.includes('/reviews')) return route.fulfill({ json: { items: [], summary: { count: 0, average: 0 }, nextCursor: null } });
  47 |       if (path.includes('/availability')) return route.fulfill({ json: { available: true } });
  48 |       return route.fulfill({ status: 404, json: { error: 'No fixture for this API' } });
  49 |     });
  50 |     await page.goto('/reservar?service=recording-fixture', { waitUntil: 'domcontentloaded' });
  51 |     const name = page.getByRole('textbox', { name: 'Nombre completo', exact: true });
  52 |     const email = page.getByRole('textbox', { name: 'Correo', exact: true });
  53 |     await name.fill('Visitante de prueba');
  54 |     await email.fill('visitante@example.test');
  55 |     releaseInitial();
  56 |     const warning = page.getByRole('alert').filter({ hasText: 'No pudimos cargar el catálogo de servicios' });
  57 |     await expect(warning).toBeVisible();
  58 |     await expect(name).toBeDisabled();
  59 |     const retryButton = warning.getByRole('button', { name: 'Reintentar catálogo', exact: true });
  60 |     await expect(retryButton).toBeEnabled();
  61 |     await page.addScriptTag({ content: axe.source });
  62 |     const violations = await warning.evaluate(async element => (await globalThis.axe.run(element, {
  63 |       runOnly: { type: 'tag', values: ['wcag2a', 'wcag2aa', 'wcag21aa', 'wcag22aa'] },
  64 |     })).violations.filter(v => ['serious', 'critical'].includes(v.impact)).map(v => ({ id: v.id, impact: v.impact })));
  65 |     expect(violations).toEqual([]);
  66 |     const before = requests;
  67 |     recovered = true;
  68 |     await retryButton.focus();
  69 |     await retryButton.press('Enter');
  70 |     await expect(warning.getByRole('button', { name: 'Cargando catálogo…', exact: true })).toBeDisabled();
  71 |     await expect(name).toBeDisabled();
  72 |     await expect.poll(() => requests).toBe(before + 1);
  73 |     expect(writes).toEqual([]);
  74 |     releaseRetry();
  75 |     await expect(warning).toHaveCount(0);
  76 |     await expect(name).toBeEnabled();
  77 |     await expect(name).toHaveValue('Visitante de prueba');
  78 |     await expect(email).toHaveValue('visitante@example.test');
  79 |     await expect(page.getByRole('combobox', { name: /^Servicio\b/ })).toContainText('Grabación de prueba');
  80 |     await page.getByRole('button', { name: 'Continuar', exact: true }).click();
  81 |     await expect(page.getByLabel(/^Fecha y hora/)).toBeVisible();
  82 |     await page.setViewportSize({ width: 320, height: 740 });
> 83 |     expect(await page.evaluate(() => document.documentElement.scrollWidth > innerWidth)).toBe(false);
     |                                                                                          ^ Error: expect(received).toBe(expected) // Object.is equality
  84 |     expect(errors).toEqual([]);
  85 |     expect(writes).toEqual([]);
  86 |     await page.screenshot({ path: testInfo.outputPath('booking-catalog-recovered.png'), fullPage: true });
  87 |   });
  88 | }
  89 | 
```