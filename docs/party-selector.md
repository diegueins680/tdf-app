# Selector de Party

## Propósito

Una relación con una Party siempre persiste su `partyId` canónico. Las personas
no deben escribir ese identificador para encontrar a otra persona.

`GET /parties/search` es la única API para construir selectores de Party. A
diferencia de `GET /parties`, devuelve una proyección mínima sin correo,
teléfono, notas, identificación fiscal ni contactos de emergencia. Requiere
autenticación, acceso CRM y una consulta de dos caracteres como mínimo.

## Contrato y privacidad

- Parámetros: `q`, `kind`, `accountOnly`, `excludePartyId`, `cursor`, `limit`.
- Máximo 20 resultados y 100 exclusiones por solicitud.
- Sólo devuelve nombre, username si existe, estado de cuenta, tipo, etiqueta
  secundaria y una futura URL de avatar optimizado.
- La autorización se ejecuta en el backend; los filtros del cliente no
  otorgan descubribilidad adicional.
- `GET /parties` se conserva para el CRM administrativo existente y no debe
  usarse para completar campos de relación.

La implementación actual no tiene un modelo canónico de tenant, visibilidad,
bloqueo o estado de Party. Por eso esos filtros no se simulan en el selector:
la siguiente extensión debe modelarlos en backend antes de exponerlos en la
interfaz.

## Web

Usar `UserSelector` para relaciones de persona con cuenta activa:

```tsx
<UserSelector
  label="Persona a invitar"
  value={party}
  onChange={setParty}
  excludedPartyIds={[currentPartyId]}
/>
```

El estado del formulario guarda el objeto mínimo sólo para presentarlo, pero
el envío transforma únicamente `party.partyId` al campo de relación del API.
El texto escrito no es una selección válida.

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
gestión de foco.

## Migración y rollback

La ruta es aditiva: los endpoints existentes continúan recibiendo `partyId`.
No hay migración de datos porque este cambio no cambia claves ni columnas. Para
rollback, retirar los consumidores del selector y la ruta nueva; los registros
existentes permanecen intactos. No eliminar `GET /parties` hasta que los
consumidores administrativos restantes estén migrados a listados paginados.

## Matriz de auditoría

| Módulo | Flujo | Antes | Solución | Estado |
| --- | --- | --- | --- | --- |
| `tdf-hq-ui/SocialEventsPage` | Invitación a evento | Texto `Party ID` | `UserSelector`; envía sólo el ID elegido | Migrado |
| `tdf-hq-ui/EventLogisticsPage` | Añadir miembro | Texto `ID de usuario TDF` | `UserSelector`; envía sólo el ID elegido | Migrado |
| `tdf-hq-ui/EventLogisticsPage` | Responsable de actividad | Texto ID + externo | `UserSelector` y opción externa separada | Migrado |
| `tdf-mobile/eventDetail` | Invitar a amigo | Texto `Party ID` | Selector nativo; transmite sólo el ID elegido | Migrado |
| `tdf-mobile/userProfile` | Identidad propia | Party ID manual | Se deriva de sesión autenticada; compatibilidad local sólo como fallback no editable | Migrado |
| `tdf-hq-ui/InternalFeedbackPage` | Asignación | Texto Party ID | `UserSelector` interno | Migrado |
| `tdf-hq-ui/OperationsControlCenterPage` | Filtros | Dos textos Party ID | Selectores compactos de responsable/cliente | Migrado |
| `tdf-hq-ui/BookingsPage` | Cliente/ingeniero | Catálogo CRM completo y resolución por nombre | `PartySelector` remoto; conserva relaciones históricas y creación de contactos | Migrado |
| `tdf-hq-ui/LabelArtistsPage` | Enlazar perfil de artista | Lista CRM con Party ID y correo | `PartySelector` mínimo; el ID queda oculto y canónico | Migrado |
| `tdf-hq-ui/CollaborativeEventCreator` | Colaboradores | Catálogo CRM completo | Selector múltiple | Pendiente |

Los elementos pendientes se documentan deliberadamente: no deben interpretarse
como migrados. La auditoría completa de rutas está en el cuerpo del PR.
