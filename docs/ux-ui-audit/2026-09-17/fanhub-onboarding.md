# FanHub: cierre confirmado y recuperación por sesión

Hallazgo confirmado **UX-260917-012**. Baseline: `19d8dc830` (incremento raíz #421).
El componente ocultaba los primeros pasos antes de confirmar el guardado, aceptaba
cualquier respuesta exitosa y distinguía cuentas sólo por partyId. La preferencia
global del modo gestor también accedía a localStorage sin proteger el getter.
Estas condiciones permiten confirmar visualmente algo no guardado y aplicar
respuestas de credenciales anteriores al volver a la misma cuenta.

Se reutiliza el trabajo pertinente de [PR #368](https://github.com/diegueins680/tdf-app/pull/368),
fuente `ff3cd1676a6db7b6ceef56d44407b0846a4f2071`: hook, contratos de respuesta,
pruebas de componente, recorrido sintético y modelo de seguridad. Se integra sobre
el componente actual; no se importa ni mergea su stack de eventos. Se añade aquí
comprobación de progreso temporal con un control negativo sin fairness.

El cierre explícito conserva la guía y muestra estado de guardado hasta recibir
un recibo terminal válido. Un fallo conserva la guía y ofrece reintento. Las
lecturas fallidas ofrecen recuperación sin mostrar datos anteriores. Se captura
la credencial del comando, se separa cada generación de sesión y se impide doble
envío dentro de ella. Visitantes y gestores sólo cierran consejos efímeros: no se
registra una primera acción útil ni finalización de cuenta por ver/cerrar consejos.
No se modifican grants ni autoridad del servidor.

## Criterios y conformidad

| Requisito | Modelo/contrato | Evidencia ejecutable |
|---|---|---|
| FH-01: sólo el cierre explícito envía la operación; sin firstValue inventado | `ConsentOnly`; cuerpo `{}` | render sin efecto, invitado/gestor sin API, doble cierre y API con bearer capturado |
| FH-02: respuesta válida de la sesión actual | `CurrentContext`; Zod del DTO | carga antes de hidratación, rotación de credencial misma cuenta, cambio de cuenta, salida/regreso, unmount |
| FH-03: guía no oculta antes de recibo terminal válido | `TerminalOnly`; contrato de recibo | cuatro recibos inválidos/no terminales, error y reintento, lectura antigua tras confirmación |
| FH-04: un comando pendiente por generación | `SingleFlight` | doble clic en el mismo turno, un solo POST |
| FH-05: error accesible recuperable y sin dependencia de storage | contratos UI ES/EN | 14 recorridos navegador; axe sobre recuperación, teclado, payload inválido, reload sintético; storage bloqueado en componente |
| FH-06: una solicitud termina si su respuesta acaba llegando | `RequestsResolve` bajo `FairSpec` | promesas diferidas resueltas/rechazadas en pruebas; control negativo sin fairness produce lasso infinito |

La conformidad es explícita mediante casos de transición del componente real,
no generación de código desde TLA+. Las seis pruebas preexistentes se conservan.
Sus fixtures se corrigen al DTO actual; dos expectativas que reactivaban errores
de una sesión anterior se sustituyen por comprobar la lectura actual y permitir
otro cierre. Esto implementa FH-02: una respuesta antigua no domina una sesión nueva.
Los harness diferidos esperan que la petición se despache antes de cambiar sesión.
El port inicial falló 19/23 casos sobre el componente anterior, incluyendo diferencias
de nombres accesibles; ese número no representa 19 defectos independientes.

## Comandos y resultados locales (2026-09-17)

```sh
npm run test --workspace=tdf-hq-ui -- --runTestsByPath src/pages/FanHubPage.audit.test.tsx src/pages/FanHubPage.onboarding.test.tsx src/api/session.test.ts
npm run typecheck --workspace=tdf-hq-ui
npm run test:e2e:web -- fanhub-onboarding.spec.mjs
JAVA_TOOL_OPTIONS=-Xmx512m JAVA_BIN=/path/to/java \
TLA2TOOLS_JAR=/path/to/tla2tools-1.7.2.jar \
ALLOY_JAR=/path/to/alloy-6.2.0.jar bash scripts/verify-event-operations-formal.sh
```

Pruebas de componente: 23/23 de conformidad y 6/6 heredadas; API 10/10,
incluido bearer capturado/cuerpo vacío. Suite completa final: **212 suites / 2.031
pruebas**, cero fallos (191,481 s). Typecheck y lint
dirigido terminaron con código 0. Navegador: **14/14**, cinco perfiles; Chromium
desktop/teléfono/tablet y recorrido crítico Firefox/WebKit. API completamente
interceptada con datos sintéticos, incluyendo hosts distintos del frontend; no
se envían credenciales ni escrituras al servicio real. No es prueba de persistencia
real del backend. Se inspeccionó visualmente la captura de teléfono de recuperación.
Axe no encontró violaciones serious/critical en los estados comprobados; no acredita
WCAG completo, lector físico, todas las rutas ni estudios con usuarios.

Modelo FanHub: TLC **215 estados distintos, 1.249 generados, profundidad 12**;
seguridad y propiedad temporal pasan. Cuatro mutantes detectan las invariantes
esperadas, y Spec sin fairness viola RequestsResolve como se esperaba. Runner
ampliado terminó con código 0: diez configuraciones TLC positivas, ocho controles
negativos esperados, Alloy un escenario SAT y ocho checks UNSAT. Toolchain fijado
y comandos completos en [README principal](README.md#verificación-formal-real).

Límites: tres generaciones de sesión, dos slots pendientes, transición atómica,
lectura elegible/oculta/inválida y recibo terminal/no terminal. Weak fairness por
slot supone que red/planificador acaba devolviendo éxito o fallo; no prueba progreso
con una conexión colgada indefinidamente. No modela revocación de cookies entre
pestañas tras despacho, almacenamiento PostgreSQL ni permisos del servidor. Éstos
siguen siendo verificaciones separadas obligatorias.

## Release y rollback

Sin migración ni cambio de formato HTTP; el argumento de credencial de API es
opcional y conserva callers existentes. Revertir sólo este incremento web devuelve
el comportamiento anterior; no borrar progreso de onboarding en servidor. Publicación
pendiente de CI exacto, revisión independiente y coordinación del despliegue activo.
Cerrar esta guía no demuestra primera acción útil ni mejora causal de engagement.

Fuentes primarias consultadas el 2026-09-17: [TanStack Query v5, mutaciones](https://tanstack.com/query/v5/docs/framework/react/guides/mutations) distingue pendiente/error/éxito y advierte sobre orden de resolución; se comprueba la generación del comando antes de aplicar callbacks. [WCAG 2.2, mensajes de estado](https://www.w3.org/WAI/WCAG22/Understanding/status-messages.html) sustenta anunciar guardado/errores sin desplazar el foco. Son criterios de implementación, no resultados de investigación de usuarios.
