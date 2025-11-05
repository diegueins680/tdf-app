
# Backend SOP Enforcement (Haskell/Servant + Persistent)

1) **Models**: agrega `backend/config/models_sop.persistent` a tu archivo `config/models` y ejecuta la migración (Persistent).
2) **Módulos**: copia `backend/src/SOP/*` a tu árbol y expón `SOP.API` dentro de tu `Server` principal (ruta `/projects/:id/...`).
3) **Seeds**: importa `docs/seed-sop-liveSession.json` a tablas `sop_requirement` y `project_stage` (o genera desde tu init).
4) **GitHub Dispatch**: define env `GH_REPO_TOKEN` (PAT con `repo:write`) y `GH_DISPATCH_REPO="diegueins680/TDF"` en el entorno del backend.

**Transición a `Schedule`**: el backend valida evidencias; si todo OK, dispara `repository_dispatch` con `event_type=upload_youtube`.
