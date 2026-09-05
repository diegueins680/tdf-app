# Protocolo de staging, accesibilidad y piloto — Reputación contextual v1

> **Estado:** borrador operativo. No autoriza activar producción; requiere las
> aprobaciones de Producto/Legal, Operaciones y Moderación.

## 1. Objetivo

Validar en staging que reputación contextual es segura, accesible, explicable y
operable antes de habilitar un piloto reducido y consentido. La reputación
pública permanece oculta hasta superar las fases y umbrales de este protocolo.

## 2. Precondiciones

- Aprobaciones vigentes de consentimiento, retención, Moderación y runbook de
  operaciones.
- Feature flag apagado por defecto y grupos piloto definidos sin discriminación.
- Migraciones checksum-pinned disponibles, snapshot/backup verificable y
  rollback ensayado.
- Worker, cola, dashboards, DLQ y on-call disponibles en staging.
- Datos sintéticos diversos: cuentas nuevas/antiguas, roles, ciudades, muestras
  pequeñas/grandes, empates, exclusiones, antigüedad y fraude simulado.

## 3. Validación funcional de staging

Ejecutar y guardar evidencia de:

1. Elegibilidad por reserva, orden, servicio y otras fuentes verificables; no
   permitir autoevaluación, duplicado por dirección ni cuenta bloqueada.
2. Borrador, edición dentro de plazo, historial y rechazo al superar plazo.
3. Orden de categorías, pesos ROC exactamente 100%, orden monotónico y modo
   avanzado normalizado.
4. Comparaciones de 3--10 perfiles comparables, empates, exclusiones y casos de
   uno/dos participantes como evaluación individual no comparativa.
5. Separación estricta: ranking privado/preferencias/señales no verificadas no
   cambian agregado, badge, búsqueda ni API pública.
6. Idempotencia, concurrencia, reintentos, eventos fuera de orden y recuperación
   de DLQ.
7. Apelación sintética, exclusión provisional, recalculo acotado y auditoría de
   acceso administrativo con motivo.
8. Exportación/eliminación sintética según política aprobada, sin identidad de
   terceros.

## 4. Accesibilidad y usabilidad

Probar en web y dispositivo móvil físico con teclado, lector de pantalla y red
lenta. El drag-and-drop no puede ser el único mecanismo.

| Área | Evidencia mínima |
| --- | --- |
| Teclado | foco visible, mover arriba/abajo, asignar posición, empate, excluir, deshacer |
| Lector de pantalla | nombres accesibles, avatar con iniciales, instrucciones, `aria-live` y confirmación de guardado |
| Táctil | arrastre o controles equivalentes, objetivos táctiles y recuperación ante error |
| Visual | WCAG 2.2 AA, contraste, no depender solo de color, movimiento reducido |
| Idioma | ES/EN sin mezcla de texto ni formato de porcentajes erróneo |
| Red | carga progresiva, estado offline/error, reintento seguro y borrador persistente |

Registrar hallazgos como bloqueador, alto, medio o bajo. Un bloqueador de
accesibilidad, identidad o privacidad impide pasar de fase.

## 5. Fases del piloto

### Fase A — lectura interna

Activar el flag solo para personal autorizado en staging. Validar dashboards,
conteos, muestra mínima y ausencia de señales privadas en agregados.

### Fase B — piloto consentido sin publicación amplia

Incluir un grupo pequeño que haya aceptado explícitamente. Habilitar creación de
evaluaciones y preferencias; mantener rankings públicos y posiciones exactas
deshabilitados. Soporte y Moderación deben tener turnos y ruta de escalamiento.

### Fase C — visibilidad limitada

Tras dos semanas estables, mostrar agregados consentidos donde exista muestra
suficiente. Tratar la reputación como una señal entre relevancia, ubicación,
disponibilidad y contexto; nunca como orden único.

## 6. Métricas y criterios cuantitativos

Medir sin PII innecesaria:

- error de escritura, latencia, reintentos, DLQ y determinismo;
- inicio/finalización/abandono por paso;
- categorías usadas/no aplicables, empate y exclusión;
- cobertura de señales verificadas, tamaño de muestra y confianza;
- estabilidad de score, apelaciones, alertas de fraude y accesibilidad;
- impacto agregado sobre descubrimiento, reservas y contratación.

**Aceptar fase:** cero fuga de identidad, cero ranking privado en agregado,
DLQ atendida, rollback ensayado y métricas dentro de presupuesto aprobado.

**Pausar inmediatamente:** error de write >1% durante 15 minutos, fuga de
identidad, variación no explicada >10 puntos, fraude sin capacidad de revisión,
DLQ sin dueño o bloqueador WCAG 2.2 AA.

## 7. Rollback y comunicación

1. Apagar `CONTEXTUAL_REPUTATION_ENABLED` para el grupo/ambiente afectado.
2. Detener nuevos consumidores/escritores, preservar evidencia y registrar
   incidente; no borrar reseñas heredadas ni auditorías.
3. Ocultar proyecciones afectadas o restaurar la última versión válida.
4. Informar a participantes de manera proporcional, sin revelar datos de otros.
5. Corregir y reproducir en staging con un nuevo `run_id` antes de reabrir.

## 8. Cierre del pendiente 4

El pendiente se cierra al adjuntar evidencia de todas las pruebas de staging,
sesiones con lector de pantalla y dispositivo físico, ensayo de rollback, dos
semanas de piloto estable y aprobaciones de Producto, Seguridad, Moderación y
Operaciones para la siguiente fase.

## 9. Control de versión

- **Versión:** 0.1 (borrador)
- **Propietario:** QA + Accesibilidad + Operaciones
- **Revisión:** antes de añadir participantes reales
