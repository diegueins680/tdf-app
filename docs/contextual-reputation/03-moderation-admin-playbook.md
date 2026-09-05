# Playbook de Moderación y Administración — Reputación contextual v1

> **Estado:** borrador. No habilita el feature flag ni concede permisos fuera
> del RBAC aprobado.

## Propósito

Administrar reputación contextual sin revelar autores de evaluaciones ni mezclar
preferencias o rankings privados con reputación pública. Las decisiones deben
ser proporcionales, explicables, revisables y auditadas.

Reglas no negociables:

- Rankings privados, preferencias y señales no verificadas no afectan búsquedas,
  badges, recomendaciones ni reputación pública.
- La evidencia individual se consulta solo para fraude o disputas, con permiso,
  motivo y auditoría.
- Una alerta no es un veredicto: ninguna sanción se aplica automáticamente.
- Se rechazan categorías y decisiones basadas en atributos sensibles o
  protegidos.
- Un score no genera automáticamente bloqueo, comisión, precio o pérdida de
  acceso.

## Roles y separación de funciones

| Rol | Puede hacer | No puede hacer |
| --- | --- | --- |
| Moderador de categorías | Revisar, aprobar, archivar o fusionar categorías | Ver evidencia individual sin caso |
| Investigador de fraude | Revisar señales ligadas a un caso | Auto-sancionar o cambiar fórmula |
| Gestor de apelaciones | Aplicar medidas provisionales y resolver casos | Resolver su propio caso |
| Administrador de reputación | Configurar umbrales, versión y recálculos controlados | Acceder a evidencia sin motivo |
| Auditor | Consultar auditorías y agregados | Mutar categorías, casos o resultados |

Aplicar mínimo privilegio, revisión periódica de asignaciones y doble control
para acciones de alto impacto cuando el modelo de permisos lo soporte.

## Categorías

Las propuestas de usuarios empiezan privadas. Antes de aprobar una categoría,
Moderación verifica nombre/descripción/idioma, duplicación exacta o semántica,
riesgo ofensivo o discriminatorio, roles y contexto aplicables, y adopción
mínima configurada.

Resultados permitidos: aprobar, solicitar cambios, fusionar con existente,
mantener privada, rechazar o archivar. Registrar siempre motivo, actor y versión
de regla. Archivar detiene nuevas evaluaciones, pero preserva historial y
significado de versión. No reescribir históricos para inventar equivalencias.

Rechazar categorías que evalúen raza, etnia, nacionalidad, religión, sexo,
identidad de género, orientación sexual, discapacidad, condición médica, edad u
otro atributo protegido; también apariencia, popularidad o represalia personal.

## Evidencia individual y auditoría

Antes de abrir señales individuales, crear o seleccionar un caso y escoger
motivo: apelación, brigading, represalia, interacción ficticia, abuso de
categoría, seguridad u obligación legal. Auditar actor, permiso, motivo,
objetivo, consultas y decisión. Mostrar el mínimo necesario: estado verificable,
contexto y versión. Nunca revelar identidad del evaluador al sujeto evaluado ni
en APIs/exportaciones públicas.

## Fraude, reportes y apelaciones

Alertas admisibles: cuentas relacionadas, antigüedad inusual, volumen anómalo,
intercambio coordinado, patrón circular, brigading, represalia o interacción
ficticia. Triage: confirmar interacción verificable, revisar duplicados y
contexto, buscar explicación legítima, asignar severidad y decidir no acción,
observación, exclusión provisional, información adicional o escalamiento.

Estados del caso: recibido, en revisión, información requerida, medida
provisional, resuelto y cerrado. Durante una apelación creíble se puede excluir
provisionalmente una señal de nuevos agregados y ocultar la proyección afectada.
La medida es reversible, no identifica al evaluador y al resolver dispara un
recalculo idempotente acotado.

Objetivos propuestos: acuse en 7 días, revisión inicial en 14 y resolución
ordinaria en 30, salvo investigación compleja.

## Resultados públicos, badges y operaciones sensibles

Un resultado público exige consentimiento, muestra mínima y señales
verificadas. No mostrar posiciones exactas en poblaciones pequeñas. Los badges
proceden de reglas publicadas y verificables; nunca son comprables.

| Operación | Requisito | Auditoría |
| --- | --- | --- |
| Cambiar fórmula | Administrador + aprobación Producto/Datos | versión y simulación |
| Lanzar recálculo | Runbook, alcance y `run_id` | iniciador y resultados |
| Excluir señal | Caso de apelación/fraude | motivo, duración y revisor |
| Consultar evidencia | Caso y motivo obligatorio | actor, objetivo y consultas |
| Cambiar flag | Operaciones y rollout aprobado | ambiente, motivo y rollback |

Nunca borrar auditorías. Simular cambios de fórmula antes de publicarlos.

## Incidentes y criterio de salida

Escalar a Seguridad/Privacidad ante fuga de identidad, ranking privado expuesto,
acceso indebido, categoría discriminatoria publicada o fraude de alto riesgo.
Contener apagando la superficie/flag si hace falta, preservar evidencia y no
atribuir culpa antes de investigar.

El pendiente queda cerrado al aprobar y probar RBAC; aprobar taxonomía ES/EN;
disponer formularios de caso, motivo y auditoría; ensayar fraude/apelaciones con
datos sintéticos; aprobar reglas de badges/rankings y capacitar Moderación,
Soporte y on-call.

## Control de versión

- **Versión:** 0.1 (borrador)
- **Propietario:** Moderación + Trust & Safety
- **Revisión:** antes del piloto consentido
