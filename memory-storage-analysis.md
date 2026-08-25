# Análisis de Almacenamiento para Recuerdos del Fan Club

## Estado Actual

El proyecto `tdf-label` se despliega en **Fly.io** con la siguiente configuración de almacenamiento:

### Infraestructura Actual
- **Backend**: Haskell (Servant) en Fly.io (`tdf-hq.fly.dev`)
- **Frontend**: React en Cloudflare Pages (`tdf-app.pages.dev`)
- **Base de datos**: PostgreSQL (gestionada por Fly.io)
- **Almacenamiento local**: Volumen montado en `/data/assets` (Fly.io volume `tdf_assets`)
- **Configuración de assets**:
  - `HQ_ASSETS_DIR=/data/assets` (directorio local en el contenedor)
  - `HQ_ASSETS_BASE_URL` (URL pública para servir assets)
  - Endpoints: `/inventory` (Raw) y `/assets/serve` (Raw) para servir archivos estáticos

### Patrones Existentes
- **Inventory/Assets**: Los assets del estudio (fotos de equipos, etc.) se almacenan en el volumen local de Fly.io
- **Event Images**: El módulo `SocialEventsHandlers` ya maneja uploads de imágenes, guardándolas en `assetsRootDir` y sirviéndolas mediante `buildUploadAssetUrl`
- **Avatars/URLs externas**: Los perfiles de artistas y fans usan URLs externas (Spotify, YouTube, etc.) o URLs de avatar pegadas manualmente

## Opciones de Almacenamiento para Fotos/Videos del Fan Club

### Opción 1: URLs Externas (Recomendada para MVP)
**Implementación**: Los fans pegan URLs de fotos/videos alojados en servicios externos (Google Drive, Dropbox, Imgur, etc.)

**Ventajas**:
- Sin costo adicional de infraestructura
- Sin límite de almacenamiento en el servidor
- Sin necesidad de gestionar permisos de acceso
- Fácil de implementar (ya usado para avatares)
- Los fans ya usan Google Drive (integración existente en el proyecto)

**Desventajas**:
- Dependencia de servicios externos
- URLs pueden expirar o cambiar
- No hay control sobre el contenido
- Experiencia de usuario menos fluida

**Implementación actual**: Ya implementada en `FanClubCreateMemoryReq` con `fcmReqMediaUrls: [Text]`

### Opción 2: Almacenamiento Local en Fly.io Volume
**Implementación**: Extender el sistema existente de `assetsRootDir` para guardar uploads de fans

**Ventajas**:
- Control total sobre los archivos
- Sin dependencias externas
- Integración directa con el backend existente

**Desventajas**:
- **Volumen limitado**: Los volúmenes de Fly.io tienen tamaño limitado (empiezan en ~1-10GB)
- **Costo escalable**: Aumenta el costo de Fly.io según crece el volumen
- **Backup necesario**: Hay que implementar backup del volumen
- **Rendimiento**: Fly.io machines pueden tener latencia al servir archivos grandes
- **Límite de tamaño**: Videos pueden llenar rápidamente el volumen

**Implementación requerida**:
- Extender `SocialEventsHandlers` con validación de tipo MIME para videos
- Limitar tamaño de archivo (ej: 10MB por foto, 50MB por video)
- Implementar compresión/thumbnails para fotos

### Opción 3: Cloudflare R2 (Recomendada para Producción)
**Implementación**: Usar Cloudflare R2 (S3-compatible) para almacenar fotos/videos

**Ventajas**:
- **Costo muy bajo**: $0.015/GB/mes (storage), sin costo de egress (salida de datos)
- **Sin límites de tamaño**: Escalable indefinidamente
- **CDN integrado**: Cloudflare CDN global para entrega rápida
- **S3-compatible**: Librerías existentes (`aws-sdk`, `amazonka`)
- **Mismo ecosistema**: El frontend ya está en Cloudflare Pages

**Desventajas**:
- Complejidad adicional de configuración
- Necesita credenciales de Cloudflare
- Posible latencia al subir desde Sudamérica

**Implementación requerida**:
- Agregar dependencias: `amazonka`, `amazonka-s3`
- Configurar credenciales R2 en variables de entorno
- Implementar endpoints de presigned URL para upload directo desde el frontend
- Migrar assets existentes (opcional)

### Opción 4: AWS S3 / DigitalOcean Spaces
**Implementación**: Bucket S3 estándar

**Ventajas**:
- Estándar de la industria
- Amplia documentación y herramientas

**Desventajas**:
- **Costo de egress**: AWS cobra por datos salientes (~$0.09/GB)
- Más costoso que R2 para este caso de uso
- Complejidad similar a R2

## Recomendación

### Fase 1 (Actual/MVP): URLs Externas
Usar el approach actual de URLs externas (`fcmReqMediaUrls`) para lanzar rápidamente. Esto permite:
- Validar la funcionalidad con usuarios reales
- Medir volumen de uso real
- Evitar gastos innecesarios de infraestructura

### Fase 2 (Cuando haya tracción): Cloudflare R2
Migrar a R2 cuando:
- Se tenga >100 usuarios activos subiendo contenido
- El volumen de medios supere 1GB/mes
- Se requiera mejor experiencia de usuario (upload directo)

### Implementación Sugerida para Fase 2
1. **Presigned URLs**: El backend genera URLs firmadas de R2, el frontend sube directamente
2. **Validación**: Backend valida tipo MIME y tamaño antes de generar la URL
3. **Metadatos**: Guardar en PostgreSQL: `r2Key`, `originalName`, `mimeType`, `sizeBytes`
4. **Transformación**: Usar Cloudflare Images para thumbnails/optimización (opcional)

### Límites Recomendados
- **Fotos**: Máx 10MB, formatos: jpg, png, webp
- **Videos**: Máx 100MB, formatos: mp4, mov
- **Por recuerdo**: Máximo 10 archivos
- **Por usuario/mes**: Máximo 50 archivos (para prevenir abuso)

## Cambios Necesarios para Migración a R2 (Fase 2)

### Backend
1. Nuevo módulo `TDF.Storage.R2`:
   - Generar presigned URLs
   - Validar uploads
   - Configurar bucket y credenciales

2. Actualizar `FanClubMemory`:
   - Agregar campo `storageKey Text` (R2 object key)
   - Agregar campo `originalUrl Text Maybe` (para backward compatibility)

3. Nuevos endpoints:
   - `POST /fans/me/clubs/:artistId/memories/presigned-url` - Solicitar URL de upload
   - `POST /fans/me/clubs/:artistId/memories/confirm` - Confirmar upload completado

### Frontend
1. Nuevo componente `MediaUploader`:
   - Subida directa a R2 via presigned URL
   - Barra de progreso
   - Preview de thumbnails

2. Actualizar `FanClubCreateMemoryReq`:
   - Cambiar de URLs a `storageKeys`

### Configuración
```bash
# Variables de entorno nuevas
R2_BUCKET_NAME=tdf-fan-club-media
R2_ACCOUNT_ID=xxx
R2_ACCESS_KEY_ID=xxx
R2_SECRET_ACCESS_KEY=xxx
R2_PUBLIC_URL=https://pub-xxx.r2.dev
```

## Conclusión

Para el lanzamiento inicial, **mantener URLs externas** es la decisión correcta por simplicidad y costo. El modelo de datos actual (`mediaUrls` como `[Text]`) soporta ambos approaches, por lo que la migración a R2 en el futuro no requeriría cambios estructurales grandes.

La implementación actual ya incluye:
- ✅ Entidad `FanClubMemory` con `mediaUrls`
- ✅ UI para pegar URLs de medios
- ✅ Visualización de galería de imágenes
- ✅ Moderación de contenido (hide/delete)

El siguiente paso natural sería implementar el uploader directo a R2 cuando las métricas lo justifiquen.
