# Selector de Party

## Propósito

Una relación con una Party siempre persiste su `partyId` canónico. Las personas
no deben escribir ese identificador para encontrar a otra persona.

`GET /parties/search` es la única API para construir selectores de Party. A
diferencia de `GET /parties`, devuelve una proyección mínima sin correo,
teléfono, notas, identificación fiscal ni contactos de emergencia. Requiere
autenticación y una consulta con dos caracteres alfanuméricos como mínimo. Cada contexto
interno exige en backend el módulo propietario (`CRM`, `Scheduling`,
`Invoicing`, `Catalog`, `Ops` o `Internships`); `event_invitation` y
`social_connection` permiten a una cuenta autenticada buscar exclusivamente
personas con cuenta activa y excluyen siempre al propio actor.
`booking_engineer` aplica además en servidor la misma elegibilidad del write:
sólo Parties persona con asignación canónica `Engineer` activa.
`event_logistics` exige `scopeId=<eventId>` y autoriza únicamente al organizador
o a un miembro editor del equipo logístico de ese evento.

## Contrato y privacidad

- Parámetros: `q`, `context`, `scopeId` cuando el contexto lo exige, `kind`,
  `accountOnly`, `excludePartyId`, `cursor`, `limit`.
- Máximo 20 resultados y 100 exclusiones por solicitud. El cursor es opaco,
  acotado y sólo debe reutilizarse con la misma consulta y filtros.
- Sólo devuelve nombre, username si existe, estado de cuenta, tipo, etiqueta
  secundaria y la URL de avatar disponible. Los contextos públicos no
  buscan ni devuelven el nombre legal usado por CRM.
- La autorización se ejecuta en el backend; los filtros del cliente no
  otorgan descubribilidad adicional.
- El backend reduce `kind` y `accountOnly` a `person`/`true` en contextos
  sociales, aunque un cliente intente ampliar esos filtros.
- Los contextos sociales consumen una cuota agregada por Party autenticada
  (300 búsquedas/día). La telemetría de control conserva sólo scope, hash del
  actor, ventana y contador; nunca el término buscado.
- `GET /parties` se conserva para el CRM administrativo existente y no debe
  usarse para completar campos de relación.
- Las listas sociales autenticadas incluyen los nombres mínimos de las Parties
  ya relacionadas. Esto evita que mobile descargue todo CRM sólo para resolver
  IDs, sin añadir correo, teléfono ni otra PII al contrato.

La implementación actual no tiene un modelo canónico de tenant, visibilidad,
bloqueo o estado de Party. Por eso esos filtros no se simulan en el selector:
la siguiente extensión debe modelarlos en backend antes de exponerlos en la
interfaz.

La identidad de login conserva `UserCredential.username` con constraint de
unicidad exacta. Todos los writers de aplicación normalizan usernames
explícitos o derivados (`trim` y minúsculas) antes de persistir; el selector
normaliza además `@` para comparar sin alterar el valor mostrado. No se añadió
una constraint funcional retroactiva sobre `lower(username)`: antes de hacerlo
debe auditarse el conjunto histórico y resolverse cualquier colisión de manera
explícita, nunca fusionando Parties por nombre o username.

## Excepciones de identificadores técnicos

- `AdminTokenPage` puede mostrar el Party ID como referencia secundaria a
  administradores que inspeccionan un token. No es editable ni sustituye el
  nombre de una persona.
- `AdminUsersPage` puede mostrar `Perfil #…` como desambiguador secundario a
  administradores de cuentas. El ID no es un campo editable ni una opción de
  búsqueda y la fila conserva nombre, username y contacto autorizado.
- `LabelTracksPage` conserva `Party #…` únicamente en el alcance técnico de
  administrador recibido por URL; los usuarios normales ven “Mi artista”.
- `PartnerManagementPage` usa “DDEX Party Identifier”, un identificador del
  estándar DDEX y no la clave canónica `partyId` de TDF.
- El check-out de inventario conserva una referencia histórica libre de
  persona/empresa porque el modelo `AssetCheckout.targetPartyRef` es texto y
  no una relación con `Party`. La interfaz lo identifica como tal y no carga
  el catálogo CRM para sugerir una relación inexistente.
- Expedientes históricos sin nombre ni contacto pueden mostrar `Party #…`
  como referencia interna final. No permiten crear ni reasignar relaciones.
  Actualmente esta excepción concreta existe en `CourseRegistrationsAdminPage`
  para operadores autorizados de academia cuando el expediente no tiene ningún
  dato humano recuperable.

Estas excepciones no autorizan campos de texto de Party ID para usuarios
normales. Cualquier flujo nuevo de asignación debe usar el selector.
`PartyRelationshipMigration.test.ts` mantiene una allowlist cerrada de estas
cuatro superficies técnicas para impedir que aparezcan otras por accidente.

## Web

Usar `UserSelector` para relaciones de persona con cuenta activa:

```tsx
<UserSelector
  value={party}
  onChange={setParty}
  field={{ label: 'Persona a invitar' }}
  search={{ context: 'event_invitation', excludedPartyIds: [currentPartyId] }}
/>
```

El estado del formulario guarda el objeto mínimo sólo para presentarlo, pero
el envío transforma únicamente `party.partyId` al campo de relación del API.
El texto escrito no es una selección válida.

Usar `PartyMultiSelector` cuando el dominio acepte varias relaciones. El
componente conserva las selecciones durante búsquedas posteriores, elimina
duplicados por `partyId` y permite retirar cada chip con teclado:

```tsx
<PartyMultiSelector
  value={parties}
  onChange={setParties}
  field={{ label: 'Contactos', required: true }}
  search={{ context: 'campaign_enrollment', kind: 'any', accountOnly: false }}
/>
```

Los contextos ligados a un recurso deben enviar su alcance explícito; la API
rechaza tanto omitirlo como utilizarlo en un contexto que no lo soporte:

```tsx
<UserSelector
  value={assignee}
  onChange={setAssignee}
  field={{ label: 'Responsable TDF' }}
  search={{ context: 'event_logistics', scopeId: eventId }}
/>
```

No usar estos anti-patrones:

- `TextField label="Party ID"` para una relación de usuario.
- `Parties.list()` seguido de `filter` o `find` en el cliente.
- Mostrar correo, teléfono o ID como etiqueta principal de una opción.
- Persistir `displayName`, `username` o una URL de imagen como relación.

## Accesibilidad y comportamiento

`PartySelector` usa el combobox accesible de MUI, conserva la selección tras
un fallo, comienza a buscar a los dos caracteres, aplica debounce de 300 ms,
cancela la búsqueda anterior y descarta respuestas fuera de orden. Cada opción
usa avatar con iniciales como fallback, nombre, username y una etiqueta
secundaria compacta. MUI proporciona navegación por flechas, Enter, Escape y
gestión de foco. “Ver más resultados” carga el cursor siguiente sin eliminar
opciones previas; el error conserva texto y selección y ofrece un retry
explícito. Las consultas idénticas se deduplican y mantienen 30 segundos en
caché, cuya clave incluye identidad, roles y módulos de la sesión activa.

## Relevancia y rendimiento

La comparación preserva el texto original, pero normaliza mayúsculas, `@`,
tildes, espacios, apóstrofes, guiones y separadores de username para buscar. Acepta términos en distinto orden, nombres
compuestos, apóstrofes y guiones, y una sola edición o transposición después de
los tres primeros caracteres en términos de cuatro o más caracteres. El orden
prioriza username exacto, nombre exacto,
prefijo de username, prefijo de nombre/apellido, coincidencia de todos los
términos, parcial y finalmente aproximada.

La búsqueda está acotada a 401 candidatos por fuente y 20 resultados por
página. Resuelve Parties asociadas a usernames, credenciales y avatares por
lotes: no ejecuta lecturas por resultado. Los índices aditivos de nombre legal,
nombre visible y username normalizados viven en
`tdf-hq/sql/2026-09-02_party_selector_search_indexes.sql` y, cuando la
instalación ya dispone de `pg_trgm`, los índices GIN aditivos de
`2026-09-04_party_selector_trigram_indexes.sql`. La expresión compacta usada
por el prefiltrado SQL y sus índices está en
`2026-09-04_party_selector_punctuation_indexes.sql`; así `oneil` no excluye
`O'Neil` antes del ranking tolerante. El objetivo operativo
sigue siendo p95 menor de 500 ms; debe verificarse con cardinalidad y red de
staging, porque una medición local sin datos representativos no se presenta
como evidencia de producción.

Como control reproducible de plan, PostgreSQL 16 con 50.000 Parties y 50.000
credenciales redujo la consulta de nombre de 58,1 ms (recorrido por PK) a 7,6
ms (bitmap GIN), y la de username de 17,7 ms/42.625 filas descartadas a 2,3 ms
con el índice GIN y orden exacto-estable. Son mediciones locales de plan, no un
p95 de staging; el gate de staging continúa siendo menor de 500 ms.

## Observabilidad sin datos de búsqueda

Web y mobile emiten el mismo contrato de eventos mediante el cliente PostHog
existente, que se convierte en no-op cuando el entorno no tiene una clave:

- `party_selector_search_completed`: plataforma, contexto, página
  inicial/incremental, latencia, cantidad devuelta y existencia de cursor;
- `party_selector_search_no_results`: plataforma y contexto;
- `party_selector_search_failed`: plataforma, contexto, página, latencia y una
  clase acotada (`timeout`, `authorization`, `request`, `server` o `network`);
- `party_selector_search_cancelled`: cancelaciones esperadas por una consulta
  posterior, separadas de los errores;
- `party_selector_avatar_failed`: plataforma, contexto y tipo de Party;
- `party_selector_selection_changed` y `party_selector_selection_failed`:
  plataforma, contexto, modo simple/múltiple y acción cuando corresponde.

Ninguno de estos eventos contiene el término buscado, Party ID, nombre,
username, correo, URL de avatar ni el mensaje devuelto por el servidor. Esto
permite agregar p50/p95, errores, timeouts, búsquedas vacías y fallos de imagen
sin convertir la telemetría en un directorio lateral. La cancelación no se
contabiliza como error y una consulta idéntica deduplicada emite una sola
medición de red.

## Migración y rollback

La ruta es aditiva: los endpoints existentes continúan recibiendo `partyId`.
No hay migración de datos porque este cambio no cambia claves ni columnas. Para
rollback funcional, retirar los consumidores del selector y la ruta nueva; los
registros existentes permanecen intactos. Los índices trigram pueden retirarse
con `2026-09-04_party_selector_trigram_indexes_rollback.sql` sin tocar datos.
Los índices compactos pueden retirarse independientemente con
`2026-09-04_party_selector_punctuation_indexes_rollback.sql`.
No eliminar `GET /parties` hasta que los consumidores administrativos restantes
estén migrados a listados paginados.

## Matriz de auditoría

| Aplicación | Archivo o componente | Flujo | Comportamiento anterior | Riesgo detectado | Solución aplicada | Prueba | Estado |
| --- | --- | --- | --- | --- | --- | --- | --- |
| Web | `SocialEventsPage` | Invitación a evento | Texto `Party ID` | Error de identidad y exposición técnica | `UserSelector`; envía sólo el ID elegido | `PartyRelationshipMigration.test.ts` | Migrado |
| Web | `EventLogisticsPage` | Miembros y responsables | Texto `ID de usuario TDF` y fallback con ID | Relación inválida, datos técnicos visibles y autorización CRM ajena al flujo | `UserSelector` con contexto y alcance de evento autorizados en backend, opción externa separada y fallback sin ID | `PartyRelationshipMigration.test.ts`; prueba backend de propietario/editor | Migrado |
| Mobile | `app/eventDetail.tsx` | Invitar a amigo | Texto `Party ID` | El usuario debía conocer una clave interna | Selector nativo; transmite sólo el ID elegido | `PartyIdentityCopy.test.ts`; pruebas de EventDetail | Migrado |
| Mobile | `app/userProfile.tsx` | Identidad propia | Party ID manual | Suplantación o identidad inconsistente | Se deriva de la sesión; fallback local no editable | `PartyIdentityCopy.test.ts`; `AuthProvider.test.tsx` | Migrado |
| Mobile | `app/(tabs)/social.tsx` | Seguidores y seguidos | Descarga del CRM e IDs visibles | Enumeración y PII excesiva | DTO social mínimo resuelto en lote, sin ID visible | `SocialScreen.test.tsx` | Migrado |
| Mobile | `PartySelector` | Búsqueda e invitación | Sólo primera página y caché no separada por sesión | Resultados incompletos o cruzados | Cursor, retry, cancelación y caché por identidad/permisos | `PartySelector.test.tsx`; `partySelectorApi.test.ts` | Migrado |
| Mobile | `EventMomentCard` / `eventMoments` | Autor de un momento | `Party #…` como fallback visible | Exposición de identificador interno | Nombre humano o fallback “Cuenta TDF” | `PartyIdentityCopy.test.ts` | Migrado |
| Mobile | `app/(tabs)/vcard.tsx` | Intercambio de contacto por QR | Mostraba el Party ID recibido | Exposición técnica innecesaria | El ID queda sólo en el payload y la mutación; la tarjeta muestra identidad humana | `PartyIdentityCopy.test.ts`; `VCardScreen.test.tsx` | Migrado |
| Web | `InternalFeedbackPage` | Asignación | Texto Party ID | Asignación a entidad equivocada | `UserSelector` interno | `PartyRelationshipMigration.test.ts` | Migrado |
| Web | `OperationsControlCenterPage` | Filtros, asignación y bandeja | Textos y etiquetas con Party ID | IDs visibles y entradas inválidas | Selectores remotos; resumen humano sin ID | `OperationsControlCenterPage.test.tsx`; regresión arquitectónica | Migrado |
| Web | `BookingsPage` | Cliente e ingeniero | Catálogo CRM completo y resolución por nombre | PII excesiva, homónimos e ingenieros inelegibles | `PartySelector`; contexto de ingeniero filtra el rol canónico activo en servidor | `BookingsPage.partySelector.test.ts`; pruebas backend de rol | Migrado |
| Web | `LabelArtistsPage` | Enlazar perfil de artista | Lista CRM con Party ID y correo | Enumeración y vínculo ambiguo | `PartySelector` mínimo con ID canónico oculto | `LabelArtistsPage.test.tsx` | Migrado |
| Web | `LabelArtistsPage` | Notas rápidas | Descarga completa para filas paginadas | Transferencia innecesaria de PII | Detalle sólo para artistas visibles, con caché corta | `LabelArtistsPage.test.tsx`; regresión arquitectónica | Migrado |
| Web | `CollaborativeEventCreatorPage` | Colaboradores | Catálogo CRM completo | Descarga total y duplicados | `UserSelector` repetible con exclusiones | `PartyRelationshipMigration.test.ts` | Migrado |
| Web | `LiveSessionIntakePage` | Músico existente | Catálogo CRM completo | PII y vínculo por texto | Selector remoto; detalle tras selección | `PartyRelationshipMigration.test.ts` | Migrado |
| Web | `PaymentsPage` | Contacto, filtro y cliente | Catálogo CRM completo | Exposición financiera/PII, homónimos y exclusión accidental de organizaciones | Selector mínimo para personas u organizaciones; DTO conserva nombre de lectura | `PaymentsPage.partySelector.test.ts` | Migrado |
| Web | `ChatPage` | Nueva conversación y cabecera | ID manual, catálogo y fallbacks `Party #…` | Enumeración e ID técnico visible | Opciones limitadas a amistades; fallback privado | `PartyRelationshipMigration.test.ts` | Migrado |
| Web | `SocialPage` | Añadir amistad | ID manual y fallback `Perfil #…` | Enumeración e identidad ilegible | `UserSelector`; QR/vCard mantiene ID sólo en payload | `PartyRelationshipMigration.test.ts` | Migrado |
| Web | `InventoryPage` | Responsable de check-out | Catálogo CRM sobre referencia libre | Apariencia falsa de relación canónica | Campo de referencia histórica explícito | `PartyRelationshipMigration.test.ts` | Excepción corregida |
| Web | `CampaignAutomationsPage` | Inscribir contactos | Catálogo completo filtrado por teléfono/correo | Descubrimiento de PII y alto consumo | `PartyMultiSelector`; backend valida canal y consentimiento | pruebas de campaña y selector múltiple | Migrado |

Los usos restantes de `GET /parties` pertenecen a vistas administrativas de
directorio, no a campos que crean o modifican relaciones. La auditoría de
catálogos mantiene esas decisiones revisadas.
