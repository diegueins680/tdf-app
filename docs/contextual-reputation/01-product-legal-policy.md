# Política de Producto, Privacidad y Moderación — Reputación contextual v1

> **Estado:** borrador para aprobación. Este documento define decisiones de
> producto y requisitos operativos; no sustituye la revisión jurídica aplicable
> a cada jurisdicción.

## 1. Propósito y decisión de alcance

La reputación contextual ayuda a contratar, reservar, colaborar y descubrir
profesionales sin convertir opiniones personales en una medida pública de
popularidad. Distingue tres productos que tienen reglas y superficies de datos
separadas:

| Producto | Quién puede verlo | Fuente permitida | ¿Afecta reputación pública? |
| --- | --- | --- | --- |
| Reputación pública | Usuarios autorizados según visibilidad elegida | Interacciones verificadas y moderadas | Sí |
| Preferencias personales | Solo su propietario | Prioridades elegidas por la persona | No |
| Rankings privados | Solo su propietario | Comparaciones privadas, incluso no verificadas | No |

No se publicará una puntuación como objetiva si procede de preferencias,
comparaciones privadas o muestra insuficiente. Antes de tres señales verificadas
válidas, se mostrará **“Reputación en formación”** en vez de un número 0--100.

## 2. Reglas de producto obligatorias

1. Solo una interacción comprobable puede crear una evaluación que afecte la
   reputación pública. La relación debe identificar contexto, participantes,
   dirección y evidencia de finalización.
2. Cada interacción admite como máximo una evaluación válida por cada parte y
   dirección. Nadie puede evaluarse a sí mismo.
3. La evaluación puede editarse hasta la fecha límite configurada. Se conserva
   la versión anterior y la auditoría; la corrección posterior se tramita por
   apelación.
4. Las comparaciones solo se habilitan entre tres y diez perfiles de rol y
   contexto materialmente comparables. Con uno o dos se ofrece evaluación
   individual estructurada; con más de diez se segmenta.
5. La persona puede marcar una categoría como no aplicable o declarar que no
   tiene información suficiente. Esta acción no es una valoración negativa.
6. Categorías, insignias y resultados públicos no pueden usar ni inferir raza,
   etnia, nacionalidad, religión, sexo, identidad de género, orientación sexual,
   discapacidad, condición médica u otra característica protegida.
7. Ningún score puede causar automáticamente pérdida de acceso, cambio de
   comisiones, precios, sanción, prioridad contractual ni otra consecuencia
   sensible.

## 3. Consentimiento y transparencia

### 3.1 Momentos de consentimiento

Se requiere una acción afirmativa separada para:

- Participar en el piloto de reputación contextual.
- Mostrar una puntuación agregada, badges o tendencia en el perfil público.
- Incluirse en rankings públicos por categoría, rol, ciudad, servicio o período.
- Recibir solicitudes y recordatorios de valoración, cuando la preferencia de
  notificaciones no lo cubra ya de forma válida.

El consentimiento para visibilidad pública no es requisito para contratar,
reservar, usar el directorio ni emitir una evaluación verificable. Retirarlo
oculta resultados futuros de las superficies públicas sin borrar por defecto la
evidencia cuya conservación sea necesaria para seguridad, fraude, auditoría o
cumplimiento, conforme a la sección 6.

### 3.2 Texto de interfaz propuesto

**Título:** “Comparte tu reputación contextual”

**Resumen:** “Mostraremos solo resultados agregados de interacciones verificadas.
Tus comparaciones personales y la identidad de quien evalúa no son públicas. Tu
perfil mostrará ‘Reputación en formación’ hasta contar con evidencia suficiente.”

**Controles independientes:**

- `[ ]` Acepto participar en el piloto de reputación contextual.
- `[ ]` Permito mostrar mi reputación agregada y badges verificables en mi
  perfil público.
- `[ ]` Permito ser considerado para rankings públicos cuando exista muestra
  suficiente.

Enlace obligatorio: “Cómo se calcula, cómo apelar y cómo ejercer mis derechos”.
El flujo debe guardar versión del texto, idioma, fecha, actor y estado de cada
consentimiento; no se permite una casilla preseleccionada.

## 4. Datos, finalidad y acceso

| Clase de datos | Finalidad | Visibilidad |
| --- | --- | --- |
| Evidencia de interacción verificable | Elegibilidad, prevención de fraude y auditoría | Sistema y personal autorizado con motivo |
| Evaluación/ranking individual | Cálculo agregado, apelaciones y detección de abuso | Privada; nunca se publica el autor o posición individual |
| Preferencia y ranking privado | Compatibilidad o organización personal | Solo propietario |
| Agregado público | Decisión contextual y descubrimiento | Según consentimiento y umbral de muestra |
| Auditoría de moderación | Investigación, apelación y seguridad | RBAC estricto |

Los administradores solo pueden acceder a señales individuales para fraude o
disputa. Cada acceso exige permiso, motivo seleccionable o escrito, objetivo y
registro inmutable de auditoría. Las consultas públicas y de perfil no deben
revelar `source_id`, identidad del evaluador, grupos pequeños, ni inferencias
sobre atributos sensibles.

## 5. Categorías, moderación y equidad

Las categorías oficiales se administran en base de datos y se versionan. Una
propuesta creada por usuarios comienza privada y solo puede hacerse pública
cuando Moderación confirme, como mínimo:

1. Lenguaje no ofensivo, no discriminatorio y sin atributo sensible.
2. No duplicación exacta o semántica con una categoría oficial.
3. Contexto, roles y dirección de evaluación definidos.
4. Adopción mínima y aprobación administrativa documentadas.
5. Traducciones ES/EN revisadas y descripción comprensible.

Las categorías archivadas dejan de ser elegibles para nuevas evaluaciones pero
mantienen su historial y significado de versión. Un rechazo de categoría debe
guardar motivo y ofrecer una ruta de reporte si fue incorrecto.

## 6. Conservación, exportación y eliminación

**Decisión pendiente de Legal:** completar los plazos por jurisdicción antes de
activar el flag. Hasta entonces no se activa el piloto público.

Propuesta operativa para revisar:

- Borradores y rankings privados: eliminar al solicitarlos, salvo bloqueo legal
  documentado.
- Consentimientos y auditorías de acceso: conservar durante el plazo legal de
  defensa y cumplimiento aplicable.
- Evidencia de interacción, apelaciones y señales de fraude: retención limitada
  al período aprobado para seguridad y resolución de disputas.
- Agregados públicos: recalcular o retirar al ocultar visibilidad; no conservar
  snapshots públicos identificables innecesarios.

La exportación debe incluir evaluaciones emitidas, preferencias, rankings
privados, consentimientos, apelaciones y datos agregados propios en formato
legible. No incluirá identidad ni datos personales de otros evaluadores. Una
solicitud de eliminación/anominización debe crear caso auditable, aplicar el
alcance aprobado, recalcular agregados cuando proceda y comunicar el resultado.

## 7. Reportes, apelaciones y medidas provisionales

Cualquier persona puede reportar una señal, categoría, badge o agregado. El
flujo debe permitir motivo, evidencia opcional y estado: `recibida`, `en
revisión`, `información requerida`, `resuelta` y `cerrada`.

Durante una apelación creíble, Moderación puede excluir provisionalmente la
señal discutida de nuevos agregados y ocultar resultados derivados cuando sea
necesario para evitar daño. No se aplican sanciones automáticas basadas solo en
un modelo de fraude. Las decisiones contienen motivo, política/versiones
aplicadas, responsable autorizado, fecha y opción de revisión.

Objetivos de servicio propuestos para aprobar:

- Acuse de recibo: 7 días calendario.
- Revisión inicial: 14 días calendario.
- Resolución ordinaria: 30 días calendario, salvo investigación compleja.

## 8. Prevención de abuso

El sistema puede crear alertas para cuentas relacionadas, cuentas nuevas,
volumen anormal, intercambio coordinado, brigading, represalias, ciclos y
transacciones ficticias. Las alertas son señales de revisión, no veredictos.

Los controles mínimos son límite de frecuencia, idempotencia, elegibilidad por
interacción, topes por evaluador, muestra mínima, decaimiento moderado,
exclusión de señales disputadas y cola humana de revisión. Está prohibido vender
badges, scores o posiciones.

## 9. Aprobaciones requeridas

| Responsable | Decisión | Evidencia de aprobación |
| --- | --- | --- |
| Producto | Alcance, copy, umbrales de visibilidad y piloto | Ticket o decisión firmada |
| Legal/Privacidad | Base jurídica, jurisdicciones, plazos de retención y derechos | Revisión fechada de esta versión |
| Moderación/Trust & Safety | Taxonomía, criterios sensibles, apelaciones y SLA | Playbook aprobado |
| Seguridad | RBAC, auditoría de accesos y respuesta a incidentes | Revisión de controles |
| Operaciones | Grupo piloto, soporte y ruta de rollback | Runbook y responsable on-call |

Se bloquea la activación pública mientras falte cualquiera de estas aprobaciones.

## 10. Criterios de salida para el pendiente 1

El pendiente de Producto y Legal se considera cerrado cuando:

1. La tabla de aprobaciones está completa para la versión vigente.
2. Se aprueban los plazos concretos de conservación y el proceso de derechos.
3. El texto de consentimiento ES/EN queda aprobado y publicado en términos y
   configuración de producto.
4. El flujo registra consentimiento granular, retiro y versión del texto.
5. Moderación y soporte reciben el playbook de reportes/apelaciones.
6. Se realiza una prueba de exportación y eliminación con evidencia auditada.

## 11. Control de versión

- **Versión:** 0.1 (borrador)
- **Propietario:** Producto + Legal/Privacidad
- **Próxima revisión:** antes de habilitar staging con participantes reales
- **Cambios materiales:** consentimiento, retención, visibilidad pública,
  categorías sensibles, apelaciones o consecuencias automatizadas requieren
  nueva aprobación y versión.
