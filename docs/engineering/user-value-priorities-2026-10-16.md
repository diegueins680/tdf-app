# Prioridades de valor para usuarios — 16 de octubre de 2026

Base revisada: `main` a71e3b9ae5a6ac6e80588aa120a70829071b3ce4. Se contrastaron
los prompts anteriores de acceso/registro, Comunidad, búsqueda, navegación,
carrito, cursos, eventos/anuncios, testers, privacidad y verificación, con el
código y los 42 PRs abiertos observados durante esta revisión. Las referencias
son una fotografía de esa consulta; no certifican disponibilidad en producción.

No hay métricas actuales de conversión o uso para calcular un ROI numérico.
La prioridad combina alcance, gravedad del bloqueo, proximidad a una acción útil,
confianza en la reproducción y esfuerzo/dependencias de entrega.

| Prioridad | Trabajo | Valor para usuarios y evidencia | Próxima entrega |
| --- | --- | --- | --- |
| P0 | Completar la entrega protegida y sus requisitos de recuperación | Una mejora en una rama no beneficia al usuario hasta llegar al sistema servido. `ops/hetzner/README.md` y `docs/main-integration-admission.md` gobiernan publicación y admisión. No se ha inspeccionado producción en esta tarea. | Mantener FIFO, revisión, checks y migraciones; calificar el ejecutor de entrega antes de desplegar. |
| P0 | Integrar las reparaciones de privacidad y dinero ya preparadas | #580 limita RAG a cursos públicos; #584 controla captura de Operations; #551 consolida ledger, términos y protección de pagos. Duplicar estas ramas aumenta el riesgo de perder reparaciones. | Reconciliar con main, resolver revisión y ejecutar gates vigentes, sin activar proveedores ni cobrar para verificar. |
| P1 | Acotar la lectura completa de respuestas web | `auth.ts` y `client.ts` retiraban el temporizador al recibir cabeceras; un cuerpo detenido dejaba acceso, registro o consultas esperando. Las lecturas fallidas podían convertirse en respuestas vacías. | Reparación independiente con pruebas de transporte y cancelación; contrato `SYS-WEB-REQUEST-001`. |
| P1 | Recuperar carritos tras fallos de lectura | `MarketplacePage` mostraba el estado vacío cuando fallaba la consulta, incluso con productos guardados. Eso confunde ausencia de datos con ausencia de productos. | Estado de error y reintento explícitos, conservación de id/count, bloqueo del checkout mientras los datos no estén confirmados. |
| P1 | Entregar reserva pública, búsqueda de artistas y mensajes ya desarrollados | #552 contiene reserva/navegación; #576 búsqueda de Comunidad y scroll; #578 avisos internos y respuestas de chat. Coinciden directamente con los prompts existentes. | Integrar cada candidato con evidencia fresca y preservar sus fuentes. #577 sigue separando correo/push de los avisos internos. |
| P2 | Medir activación y recoger feedback de testers | La entrada mínima, Comunidad inicial, `/app`, carrito visible e inscripción en diálogo ya tienen implementación y pruebas en main. No deben reimplementarse a partir del texto antiguo del prompt. | Verificar disponibilidad real por plataforma y medir primer seguimiento, reserva, inscripción y reporte útil; sin anuncios falsos de disponibilidad. |
| P2 | Ampliar automatización de contenidos y Hora Libre | Dependen de identidad, consentimiento, perfiles y entrega fiables. No superan a fallos reproducidos de entrada/compra en esta iteración. | Confirmar lo ya implementado, formalizar lo pendiente y entregar después de los bloqueos anteriores. |

## Alcance de esta iteración

Implementar las dos reparaciones P1 nuevas en PRs separados. No fusionar PRs
ajenos ni alterar su estado, no activar funciones cerradas y no tratar una
validación local como prueba de despliegue. Cada PR debe indicar validación
completada, limitaciones y su base exacta. La revisión no equivale a una auditoría
exhaustiva de todos los módulos ni a conformidad matemática del sistema completo.

## Revisión del sprint de activación — 24 de octubre de 2026

La base protegida observada es `4dac175d42616b5b2f4a3e8faebb83692cd2e6b0`.
Ya incorpora reserva, búsqueda de Comunidad y avisos internos; no corresponde
reconstruir #552/#576/#578. La reparación de lectura HTTP continúa en #594.
Su reconciliación conserva las 213 migraciones, el backend, las dependencias y
Mobile `8adefc5e8b0377b7a07cee99ff1ddd14c3ebb3d0` de esa base. Conserva también
todos los requisitos de main y añade únicamente `SYS-WEB-REQUEST-001`.

La inspección de producción, de solo lectura, observó el backend `7ab3444` y
188 migraciones. La web pública incorpora `4dac175`; esto no demuestra que sus
operaciones estén disponibles en el backend anterior. El catálogo público
devuelve Verde 70 mientras la búsqueda del directorio usada por Comunidad no lo
devuelve. El plan local de release enumera 25 migraciones pendientes y mantiene
`executionAllowed: false`: siguen pendientes el ejecutor protegido, recuperación
coordinada y la decisión de retención de captura de Operations. No se desplegó.

Criterio de aceptación del incremento #594: una lectura detenida después de
recibir cabeceras termina dentro del presupuesto original de 30 segundos,
libera la actividad pendiente y permite la recuperación existente; una lectura
interrumpida no se presenta como éxito vacío ni provoca repetir autenticación.
Las regresiones seleccionadas fallan sobre el transporte original y pasan sobre
la reparación. Una comprobación adicional con Fetch nativo y HTTP loopback
confirma el vencimiento después de cabeceras sin repetir la solicitud.

Estas verificaciones de transporte no certifican publicación de un perfil,
seguimiento persistido, retención ni una relación activada en producción. El
primer valor registrado por onboarding, un favorito del directorio y un follow
del sistema social de eventos son métricas distintas del follow canónico de
`/fans/me/follows`. Tampoco una repetición de `artist_followed` demuestra una
relación nueva. La medición VAR requiere identidad válida y deduplicación por
pareja, además de separar cuentas sintéticas, personal y evidencia incompleta.

Para un piloto humano siguen vigentes el protocolo de Quito y sus consentimientos.
La meta 3/5 en 15 minutos es una hipótesis; no hay resultados de participantes en
este incremento. CI, revisión independiente del nuevo head, admisión FIFO, merge
y verificación del despliegue se registran separadamente de la validación local.
