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
