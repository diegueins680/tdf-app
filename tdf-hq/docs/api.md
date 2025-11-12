# API v1 (TDF HQ — Servant)
Este commit añade `api/openapi.yaml` (semilla) para alinear el backend con la navegación de la UI.
- Módulos: CRM, Estudio, Label, Eventos, Escuela, Finanzas, Operación.
- Seguridad: `bearerAuth` (JWT).
- Servidores: `https://api.tdf.local/v1` y `https://api.tdf.app/v1`.

## Servant (sugerido)
1. Crea módulos `TDF.API` y `TDF.Server`.
2. En `app/Main.hs`, `serve (Proxy :: Proxy TDF.API) server`.
3. Habilita CORS (`wai-cors`) para el origen de la UI.

## Próximos pasos
- Reemplazar respuestas dummy por esquemas reales.
- Añadir query params: `unit_id`, `brand_id`, `site_id`, `q`, `page`, `page_size`.
- Versionar la base como `/v1`.
