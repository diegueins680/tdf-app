# Matriz de permisos y record scope

`perfil.*` y `classified.*` son permisos de producto. Las profesiones del perfil nunca participan en
esta evaluación.

| Acción | Anónimo | Autenticado | Manager de perfil | Autor/participante | Moderador/admin |
|---|---:|---:|---:|---:|---:|
| Buscar/leer publicado | sí | sí | sí | sí | sí |
| Crear perfil | no | sí, sujeto a edad/rate limit | n/a | n/a | sí |
| Editar perfil | no | no | `edit` | n/a | permiso explícito |
| Publicar/pausar/archivar perfil | no | no | `publish` | n/a | permiso explícito |
| Administrar managers | no | no | `manage` | n/a | permiso explícito |
| Crear anuncio | no | no | `publish` del perfil autor | n/a | permiso explícito |
| Cambiar estado de anuncio | no | no | `publish` del perfil autor | n/a | moderar según transición |
| Postular perfil | no | no | `contact` del perfil candidato | postulante | moderador solo con propósito auditado |
| Leer postulación | no | no | manager del perfil autor o candidato | sí | permiso explícito + auditoría |
| Invitar/contactar | no | no | `contact` | destinatario decide | permiso explícito |
| Guardar/bloquear/reportar | no | sí | sí | sí | sí |
| Aprobar claim/verificación | no | no | nunca por profesión | no | permiso explícito y segregación |
| Fusionar perfiles/términos | no | no | no | no | permiso explícito + transacción auditada |
| Leer reseña publicada | sí | sí | sí | sí | sí |
| Consultar elegibilidad de reseña | no | no | `edit` del perfil autor | interacción exacta completada/verificada | permiso explícito, sin fabricar interacción |
| Crear reseña | no | no | `edit` del perfil autor + edad/bloqueo/rate limit | interacción exacta elegible | moderar, no inventar elegibilidad |
| Ocultar/eliminar reseña | no | no | no | puede reportar | permiso explícito + decisión auditada |

## Capacidades de manager

- `view_private`: leer configuración privada mínima del perfil.
- `edit`: editar presentación y taxonomías.
- `publish`: publicar/pausar y publicar anuncios.
- `contact`: postular, invitar o aceptar contacto en nombre del perfil.
- `manage`: conceder/revocar managers; no permite concederse roles internos.

El creador recibe grants explícitos en la misma transacción que crea el perfil. Un claim no crea
ningún grant hasta `approved`.
