# Estado de preparación · 6 de noviembre de 2026, antes de integración

La campaña permanece en borrador, con USD 0 de pauta y sin publicaciones, programaciones externas, mensajes o invitaciones. El paquete importable es una propuesta revisable, no una campaña ya guardada en producción.

## Enlaces y archivos

- Perfil existente: https://www.tdfrecords.net/a/liquid-paper-girl · artista 51. No se creó una cuenta, perfil duplicado ni permiso nuevo.
- PR web/backend: https://github.com/diegueins680/tdf-app/pull/599 · se amplía el PR existente para evitar duplicar el módulo.
- Contratos móviles: https://github.com/diegueins680/TDF-mobile/pull/180 · integrado; merge `d5c718c23b50b83dd2dd76bbb41b835730dbf7a2`. El pin `0f57163cc9c489e22738cac3ff7985d8ca4b647c` pertenece a ese historial.
- Asistente propuesto: `/mi-artista/campanas?artistId=51`, disponible cuando se despliegue. No se presenta esta ruta como una campaña de producción existente.
- Plan: [campaign.json](campaign.json). Calendario, textos, enlaces, cortes candidatos y métricas: [calendar.csv](calendar.csv). Inventario y límites audiovisuales: [assets.json](assets.json).
- Guía reutilizable: [video-campaigns.md](../../artists/video-campaigns.md).

No hay URL nueva de sesión, campaña persistida o vista previa pública: no se ejecutaron operaciones autenticadas de producción. El perfil existente es el destino previsto para el video destacado.

## Comportamiento implementado

Borradores PostgreSQL con revisión y reintento idempotente; aislamiento por artista; permisos actuales de colaboradores del directorio; cuatro pasos con textos editables y calendario relativo de 14 días; guardar, reanudar, duplicar e importar una plantilla; almacenamiento privado recuperable existente, con su cuota; vista previa de video; descarga del plan y registro manual de publicación. Guardar, exportar y elegir una fecha propuesta no publican ni programan.

La publicación del video destacado de TDF es una acción separada sobre la revisión guardada y exige confirmar permisos y revisión técnica. Las redes siguen por exportación y publicación manual. No hay proveedor simulado ni formulario de credenciales en este asistente. La unión con el servicio de proveedores de PR667 queda pendiente; sus campañas ajenas se conservan.

El recorrido público conserva la atribución al volver del registro/inicio de sesión. Seguir requiere una acción explícita y conserva el estado real. Los contadores verifican creaciones reales de cuenta y seguimiento, únicas por campaña/cuenta/tipo; el desglose por pieza usa la primera atribución aceptada. No demuestra causalidad ni mide todos los retornos tardíos. Las reproducciones observables del archivo usan la analítica configurada; iframes y cifras sociales desconocidas no se inventan.

## Validación y límites

- Web completa anterior a las dos correcciones de revisión: 354 suites y 4.001 pruebas aprobadas con dependencias del lockfile. Después de corregir la generación de redes faltantes y validar identificadores compatibles con atribución: 46 pruebas enfocadas aprobadas, build y lint aprobados.
- Backend completo tras la revisión: 4.225 casos, cero fallos y 23 pendientes explícitos con Stack/GHC9.10.3. La prueba específica del permiso de colaboradores sí pasó contra PostgreSQL16; aquella ejecución tuvo 22 pendientes.
- Navegador: 18 casos aprobados en Chrome de escritorio y emulación Android de 360 px, con APIs sintéticas. No equivale a dispositivo Android físico, cuentas sociales conectadas o escrituras en producción.
- PostgreSQL16 aislado: persistencia entre conexiones, dos espacios de artista, migración repetible, escrituras CAS concurrentes y deduplicación de conversiones. Bases propias eliminadas y clúster propio detenido.
- Política finita: 16.384 combinaciones y 13 controles que retiran una guarda. Es evidencia acotada, no prueba universal de SQL, autenticación o concurrencia.
- Móvil: 98 suites, 653 pruebas, tipos, lint y release check con Node20.19.4; contratos aditivos, sin cambios de runtime o dependencias.

La máquina agotó el disco durante una copia de compilación. Se liberaron solo dependencias temporales reproducibles de este checkout y se repitió Stack con éxito. Los intentos fallidos se conservan en la evidencia privada; no se cuentan como aprobados. La ejecución web usó Node24.13.1 y Chrome instalado; CI conserva sus versiones canónicas.

## Pendientes que impiden completar el lanzamiento

1. **Video ausente.** Falta una ruta o descarga accesible del adjunto de unos 10:10. Las medidas históricas 1080p/610,08 s y seis intervalos no son una inspección actual. Las búsquedas conectadas por nombre exacto de archivo, artista y «Liquid» no devolvieron archivos. El volumen Videos rechaza la lectura incluso fuera del sandbox. No hay MP4, portadas o Stories entregados en esta ejecución; faltan escucha completa, selección musical/visual, subtítulos si hay voz y render final. No sustituir por el liveset histórico de 47:05.
2. **Identidad y derechos.** Faltan enlaces oficiales cruzados, imagen autorizada, ubicación, géneros, fecha y créditos. Permanecen sin inventar; no se conceden derechos por coincidencia de nombre.
3. **Proveedores.** Faltan conexión autorizada de cada artista, permisos y aprobación/auditoría del producto correspondiente. La documentación oficial y el recorrido manual están en la guía. No se conectaron ni desconectaron cuentas ajenas.
4. **Integración protegida y despliegue.** El PR principal necesita sus controles y revisión actuales. `ops/hetzner/README.md` declara pendiente el ejecutor de releases: la preparación informa `executionAllowed: false` y la vía directa está retirada. No se elude ese bloqueo mediante SSH, Compose, Caddy o flags. Merge, despliegue y activación son hitos distintos; no hay evidencia de despliegue de esta función.

La siguiente revisión puede aprobar textos y calendario, pero no debe considerar publicables los archivos ausentes. Ninguna migración de esta entrega siembra campañas ni activa envíos.

## Correcciones de revisión

Preparar una red nueva añade únicamente sus piezas y conserva las ediciones de las otras redes, incluso al quitar y volver a seleccionar un canal. La generación repetida no duplica piezas y evita colisiones con identificadores importados. Importación, validación de documentos y backend exigen identificadores ASCII de letras, números, guiones o guiones bajos, compatibles con la atribución y el retorno de autenticación. Ambos casos tienen pruebas de regresión.

CI detectó y se corrigieron dos requisitos de verificación: checkout completo para consultar la procedencia histórica de migraciones, y revisión explícita de las tres adiciones del registro Cabal. Las pruebas de procedencia se conservan; los predicados verificados no cambian. La ejecución formal fresca pasa y su decisión de release permanece BLOCKED.

El objetivo principal es editable y persistente, con registros y seguidores del artista en TDF como valor inicial. El plan importado conserva su objetivo y el editor lo muestra al revisar. Los objetivos libres no habilitan métricas, gasto o acciones adicionales. La validación final enfocada cubre 47 pruebas web; el backend conserva la compatibilidad de documentos anteriores sin ese campo.

Este documento conserva la preparación anterior al merge: consultar el PR para los checks y el estado final. La revisión del objetivo requirió repetir una compilación que coincidió con una edición de fuente; aquel fallo de `null` queda conservado y no se promueve a prueba aprobada. La calificación final exige la repetición con fuente congelada y CI del commit publicado.

## Calificación local final

La repetición con fuente congelada termina con 4.226 casos de backend, cero fallos y 23 pendientes. Las correcciones de importación y perfiles no públicos terminan con 52 pruebas web enfocadas, build/lint y 20 recorridos de navegador aprobados. La importación permite revisar una actualización contra la revisión guardada o crear una copia con ID y enlaces nuevos, sin sobrescribir automáticamente. La autorización del servidor permite preparar un borrador no público; no se amplía la visibilidad. El control estricto admite 1.403 candidatos: los 1.402 anteriores y un registro técnico nuevo de claves DTO revisado expresamente. Los checks hospedados finales y la revisión protegida deben consultarse en el PR.
