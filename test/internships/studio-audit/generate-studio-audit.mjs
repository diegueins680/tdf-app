#!/usr/bin/env node

/**
 * Canonical generator for Stuart's studio-management audit inventory and test plan.
 * Engineering metadata is English; all executable instructions rendered to the intern are Spanish.
 */
import { mkdir, readFile, writeFile } from 'node:fs/promises';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

const here = path.dirname(fileURLToPath(import.meta.url));
const repo = path.resolve(here, '../../..');
const docsDir = path.join(repo, 'docs/internships/studio-audit');

const moduleConfig = {
  foundation: { name: 'Acceso, cuenta y experiencia transversal', code: 'FND', day: 1, evidence: ['tdf-hq-ui/src/routes/protectedRoutes.tsx', 'tdf-hq/src/TDF/API.hs', 'tdf-hq/src/TDF/Server.hs'] },
  crm: { name: 'CRM, contactos y oportunidades', code: 'CRM', day: 2, evidence: ['tdf-hq-ui/src/pages/PartiesPage.tsx', 'tdf-hq-ui/src/pages/LeadsPage.tsx', 'tdf-hq/src/TDF/API.hs'] },
  scheduling: { name: 'Calendario, salas, recursos y reservas', code: 'SCH', day: 3, evidence: ['tdf-hq-ui/src/pages/BookingsPage.tsx', 'tdf-hq-ui/src/pages/RoomsPage.tsx', 'tdf-hq/src/TDF/API/Rooms.hs', 'tdf-hq/src/TDF/Server.hs'] },
  sessions: { name: 'Órdenes y sesiones de grabación', code: 'SES', day: 4, evidence: ['tdf-hq-ui/src/pages/OrdersPage.tsx', 'tdf-hq/src/TDF/API/Sessions.hs', 'tdf-hq/src/TDF/API.hs'] },
  commerce: { name: 'Cotizaciones, facturación y pagos', code: 'PAY', day: 5, evidence: ['tdf-hq-ui/src/pages/PaymentsPage.tsx', 'tdf-hq/src/TDF/API/Payments.hs', 'tdf-hq/src/TDF/Commerce/ServiceBookings.hs'] },
  inventory: { name: 'Inventario, equipos y mantenimiento', code: 'INV', day: 6, evidence: ['tdf-hq-ui/src/pages/InventoryPage.tsx', 'tdf-hq-ui/src/pages/ReservasEquipoPage.tsx', 'tdf-hq/src/TDF/API/Inventory.hs'] },
  reporting: { name: 'Operación, recepción y reportes', code: 'OPS', day: 6, evidence: ['tdf-hq-ui/src/pages/ReportsPage.tsx', 'tdf-hq-ui/src/pages/OperationsControlCenterPage.tsx', 'tdf-hq-ui/src/pages/PaymentsPage.tsx'] },
  live: { name: 'TDF Live Sessions', code: 'LIV', day: 7, evidence: ['tdf-hq-ui/src/pages/LiveSessionIntakePage.tsx', 'tdf-hq-ui/src/pages/LiveSessionPublicPage.tsx', 'tdf-hq/src/TDF/ServerLiveSessions.hs'] },
  integrations: { name: 'Notificaciones e integraciones', code: 'INT', day: 7, evidence: ['tdf-hq/src/TDF/Email/Service.hs', 'tdf-hq/src/TDF/WhatsApp/Service.hs', 'tdf-hq-ui/src/pages/CalendarSyncPage.tsx'] },
  domo: { name: 'Dependencias compartidas con Domo', code: 'DOM', day: 8, evidence: ['tdf-hq-ui/src/pages/TdfDomoCampaignPage.tsx', 'tdf-hq/src/TDF/API.hs', 'tdf-hq/docs/openapi/api.yaml'] },
  school: { name: 'Dependencias compartidas con Escuela', code: 'EDU', day: 8, evidence: ['tdf-hq-ui/src/pages/ClassesPage.tsx', 'tdf-hq-ui/src/pages/TrialLessonsPage.tsx', 'tdf-hq/src/TDF/Trials/API.hs'] },
  ddex: { name: 'Datos de estudio relacionados con DDEX', code: 'DDX', day: 8, evidence: ['tdf-hq/src/TDF/API/DDEX.hs', 'tdf-hq/src/TDF/Server/DDEX.hs', 'tdf-hq/src/TDF/DDEX/Validation.hs'] },
  permissions: { name: 'Roles, permisos y acceso directo', code: 'SEC', day: 9, evidence: ['tdf-hq/assets/feature-registry.json', 'tdf-hq/src/TDF/Catalog/Security.hs', 'docs/feature-discoverability-audit/2026-08-21/role-module-feature-action-platform-matrix.csv'] },
  internship: { name: 'Prácticas y reportes internos', code: 'QAR', day: 9, evidence: ['tdf-hq-ui/src/pages/InternshipsPage.tsx', 'tdf-hq-ui/src/pages/InternAuditPlanPage.tsx', 'tdf-hq-ui/src/pages/InternalFeedbackPage.tsx', 'tdf-hq/src/TDF/ServerInternAudit.hs', 'tdf-hq/src/TDF/ServerFeedback.hs'] },
};

// id, feature, scope, implementation, platform, role, mode, route, criticality, optional evidence
const featureRows = {
  foundation: [
    ['auth.login', 'Inicio de sesión válido e inválido', 'dependency', 'implemented_accessible', 'web_native', 'Intern', 'security', '/login', 'critical'],
    ['auth.session', 'Persistencia y vencimiento de sesión', 'dependency', 'implemented_accessible', 'web_native', 'Intern', 'security', '/inicio', 'critical'],
    ['auth.logout', 'Cierre de sesión y bloqueo de retroceso', 'dependency', 'implemented_accessible', 'web_native', 'Intern', 'security', '/inicio', 'high'],
    ['account.state', 'Cuenta activa, suspendida o incompleta', 'dependency', 'implemented_inaccessible', 'web_native', 'Admin de pruebas', 'security', '/inicio', 'critical'],
    ['navigation.discovery', 'Menú, buscador y descubrimiento de funciones', 'dependency', 'implemented_accessible', 'web_native', 'Intern', 'read', '/inicio', 'high'],
    ['navigation.direct-url', 'Acceso por URL directa y recarga', 'dependency', 'implemented_accessible', 'web_native', 'Intern', 'security', '/estudio/calendario', 'high'],
    ['localization.es-en', 'Consistencia entre español e inglés', 'shared', 'partially_implemented', 'web_native', 'Intern', 'read', '/inicio', 'medium'],
    ['responsive.layout', 'Diseño adaptable y web móvil', 'shared', 'implemented_accessible', 'web_fallback', 'Intern', 'read', '/estudio/calendario', 'high'],
    ['accessibility.baseline', 'Teclado, foco, etiquetas, contraste y escala de texto', 'shared', 'partially_implemented', 'web_native', 'Intern', 'read', '/inicio', 'high'],
  ],
  crm: [
    ['crm.customers', 'Crear, editar y consultar clientes ficticios', 'dependency', 'implemented_inaccessible', 'web_native', 'Reception', 'mutation', '/crm/contactos', 'high'],
    ['crm.customer-search', 'Buscar, filtrar y exportar contactos', 'dependency', 'implemented_inaccessible', 'web_native', 'Reception', 'read', '/crm/contactos', 'medium'],
    ['crm.customer-duplicates', 'Prevenir o identificar clientes duplicados', 'dependency', 'partially_implemented', 'web_native', 'Reception', 'mutation', '/crm/contactos', 'high'],
    ['crm.companies', 'Empresas y personas de contacto', 'shared', 'implemented_inaccessible', 'web_native', 'Reception', 'mutation', '/crm/empresas', 'medium'],
    ['crm.leads', 'Crear y calificar leads para servicios de estudio', 'direct', 'implemented_inaccessible', 'web_only', 'StudioManager', 'mutation', '/crm/leads', 'high'],
    ['crm.opportunities', 'Oportunidades y siguiente acción comercial', 'direct', 'partially_implemented', 'web_only', 'StudioManager', 'workflow', '/crm/leads', 'high'],
    ['crm.pipeline', 'Mover oportunidades por el pipeline', 'direct', 'implemented_inaccessible', 'web_native', 'StudioManager', 'workflow', '/estudio/pipelines', 'high'],
  ],
  scheduling: [
    ['studio.calendar', 'Calendario del estudio', 'direct', 'implemented_inaccessible', 'web_native', 'Reception', 'read', '/estudio/calendario', 'critical'],
    ['studio.rooms', 'Crear y mantener salas', 'direct', 'implemented_inaccessible', 'web_native', 'StudioManager', 'mutation', '/estudio/salas', 'critical'],
    ['studio.resources', 'Recursos asociados a salas', 'direct', 'implemented_inaccessible', 'web_native', 'StudioManager', 'mutation', '/estudio/salas', 'critical'],
    ['studio.availability', 'Disponibilidad de salas y recursos', 'direct', 'implemented_inaccessible', 'web_native', 'Reception', 'workflow', '/estudio/calendario', 'critical'],
    ['studio.service-types', 'Tipos y definiciones de servicio', 'direct', 'implemented_inaccessible', 'web_only', 'StudioManager', 'mutation', '/estudio/servicios', 'high'],
    ['studio.internal-booking', 'Crear una reserva interna', 'direct', 'implemented_inaccessible', 'web_native', 'Reception', 'mutation', '/estudio/calendario', 'critical'],
    ['studio.booking-edit', 'Editar horario, sala, cliente y servicio', 'direct', 'implemented_inaccessible', 'web_native', 'Reception', 'mutation', '/estudio/calendario', 'critical'],
    ['studio.booking-cancel', 'Cancelar una reserva sin borrar el historial', 'direct', 'implemented_inaccessible', 'web_native', 'Reception', 'workflow', '/estudio/calendario', 'critical'],
    ['studio.booking-conflict-room', 'Conflicto simultáneo de sala', 'direct', 'implemented_inaccessible', 'web_native', 'Reception', 'conflict', '/estudio/calendario', 'critical'],
    ['studio.booking-conflict-resource', 'Conflicto simultáneo de recurso', 'direct', 'partially_implemented', 'web_native', 'Reception', 'conflict', '/estudio/calendario', 'critical'],
    ['studio.booking-concurrency', 'Dos usuarios reservando al mismo tiempo', 'direct', 'implemented_but_not_documented', 'web_native', 'Reception', 'conflict', '/estudio/calendario', 'critical'],
    ['studio.public-booking', 'Reserva pública de estudio', 'direct', 'implemented_accessible', 'web_native', 'Visitante', 'public', '/reservar', 'critical'],
    ['studio.public-tracking', 'Seguimiento público seguro de la reserva', 'direct', 'implemented_accessible', 'web_fallback', 'Visitante', 'public', '/reservas/orden/:bookingId', 'high'],
    ['studio.public-invalid-token', 'Token inválido o de otra reserva', 'direct', 'implemented_accessible', 'web_fallback', 'Visitante', 'security', '/reservas/orden/:bookingId', 'critical'],
    ['studio.dj-booth', 'Reserva de DJ booth', 'direct', 'implemented_accessible', 'web_native', 'Visitante', 'public', '/dj-booth', 'medium'],
  ],
  sessions: [
    ['studio.orders', 'Crear y editar orden de estudio', 'direct', 'implemented_inaccessible', 'web_only', 'Reception', 'mutation', '/estudio/ordenes', 'critical'],
    ['studio.order-status', 'Aprobar y cambiar estado de orden', 'direct', 'implemented_inaccessible', 'web_only', 'StudioManager', 'workflow', '/estudio/ordenes', 'high'],
    ['studio.recording-session', 'Crear sesión de grabación desde reserva u orden', 'direct', 'implemented_inaccessible', 'web_fallback', 'Engineer', 'workflow', '/estudio/ordenes', 'critical'],
    ['studio.session-status', 'Preparación, ejecución y cierre de sesión', 'direct', 'implemented_inaccessible', 'web_fallback', 'Engineer', 'workflow', '/estudio/ordenes', 'critical'],
    ['studio.session-input-list', 'Lista de entradas, canales y microfonía', 'direct', 'implemented_inaccessible', 'native_contextual', 'Engineer', 'mutation', '/input-list/:id', 'high'],
    ['studio.session-artists', 'Artistas asociados a la sesión', 'direct', 'implemented_inaccessible', 'web_fallback', 'Engineer', 'mutation', '/estudio/ordenes', 'high'],
    ['studio.session-musicians', 'Músicos, instrumentos y roles', 'direct', 'implemented_inaccessible', 'web_fallback', 'Engineer', 'mutation', '/estudio/ordenes', 'high'],
    ['studio.session-engineers', 'Ingeniero y asistente de sesión', 'direct', 'implemented_inaccessible', 'web_fallback', 'StudioManager', 'mutation', '/estudio/ordenes', 'high'],
    ['studio.session-files', 'Carpeta, grabaciones y entregables de sesión', 'direct', 'partially_implemented', 'web_fallback', 'Engineer', 'workflow', '/estudio/ordenes', 'high'],
    ['studio.session-reopen', 'Reabrir una sesión cerrada con autorización', 'direct', 'partially_implemented', 'web_only', 'StudioManager', 'security', '/estudio/ordenes', 'high'],
  ],
  commerce: [
    ['commerce.packages', 'Paquetes y horas prepagadas', 'direct', 'implemented_inaccessible', 'web_fallback', 'StudioManager', 'mutation', '/estudio/servicios', 'high'],
    ['commerce.package-balance', 'Consumo y saldo de horas', 'direct', 'partially_implemented', 'web_fallback', 'Reception', 'workflow', '/estudio/ordenes', 'critical'],
    ['commerce.quotations', 'Cotización de servicios de estudio', 'direct', 'implemented_inaccessible', 'web_only', 'StudioManager', 'workflow', '/propuestas', 'high'],
    ['commerce.quote-acceptance', 'Aceptación o rechazo de cotización', 'direct', 'implemented_inaccessible', 'web_fallback', 'Customer de pruebas', 'workflow', '/propuestas', 'high'],
    ['commerce.invoices', 'Generar y consultar factura', 'dependency', 'implemented_inaccessible', 'web_only', 'Finance', 'workflow', '/finanzas/pagos', 'critical'],
    ['commerce.manual-payment', 'Registrar pago manual', 'direct', 'implemented_inaccessible', 'web_only', 'Finance', 'mutation', '/finanzas/pagos', 'critical'],
    ['commerce.datafast', 'Checkout Datafast sandbox', 'direct', 'implemented_inaccessible', 'web_fallback', 'Customer de pruebas', 'integration', '/reservar', 'critical'],
    ['commerce.paypal', 'Checkout PayPal sandbox', 'direct', 'implemented_inaccessible', 'web_fallback', 'Customer de pruebas', 'integration', '/reservar', 'critical'],
    ['commerce.payment-confirmation', 'Confirmación verificable del pago', 'direct', 'implemented_inaccessible', 'web_fallback', 'Finance', 'integration', '/finanzas/pagos', 'critical'],
    ['commerce.payment-failure', 'Pago rechazado o proveedor no disponible', 'direct', 'implemented_inaccessible', 'web_fallback', 'Customer de pruebas', 'integration', '/reservar', 'critical'],
    ['commerce.abandoned-checkout', 'Checkout abandonado sin cobro ni reserva falsa', 'direct', 'implemented_inaccessible', 'web_fallback', 'Customer de pruebas', 'integration', '/reservar', 'critical'],
    ['commerce.payment-idempotency', 'Doble clic, reintento e idempotencia', 'dependency', 'implemented_but_not_documented', 'web_fallback', 'Customer de pruebas', 'integration', '/reservar', 'critical'],
    ['commerce.refunds', 'Reembolso total o parcial en sandbox', 'direct', 'partially_implemented', 'web_only', 'Finance', 'integration', '/finanzas/pagos', 'critical'],
    ['commerce.reconciliation', 'Conciliación con eventos del proveedor', 'dependency', 'implemented_but_not_documented', 'web_only', 'Admin de pruebas', 'integration', '/admin/commerce/provider-events', 'critical'],
    ['commerce.failed-events', 'Eventos fallidos, reintento y dead letters', 'dependency', 'implemented_inaccessible', 'web_only', 'Admin de pruebas', 'integration', '/admin/commerce/provider-events', 'critical'],
  ],
  inventory: [
    ['inventory.assets', 'Crear, editar y consultar equipos', 'direct', 'implemented_inaccessible', 'web_native', 'StudioManager', 'mutation', '/operacion/inventario', 'high'],
    ['inventory.availability', 'Disponibilidad de equipo para una reserva', 'direct', 'implemented_inaccessible', 'web_native', 'Reception', 'workflow', '/operacion/inventario', 'critical'],
    ['inventory.reservations', 'Reservar equipo para sesión', 'direct', 'implemented_inaccessible', 'web_native', 'Reception', 'mutation', '/operacion/reservas-equipo', 'critical'],
    ['inventory.checkout-return', 'Entrega y devolución con condición', 'direct', 'implemented_inaccessible', 'web_native', 'Reception', 'workflow', '/operacion/reservas-equipo', 'high'],
    ['inventory.rentals', 'Alquiler de equipo y relación con orden', 'shared', 'implemented_inaccessible', 'web_native', 'StudioManager', 'workflow', '/marketplace/orders', 'high'],
    ['inventory.scan', 'Escaneo con token firmado', 'direct', 'implemented_accessible', 'mobile_web', 'Reception', 'security', '/inventario/scan/:token', 'high'],
    ['inventory.maintenance', 'Abrir, actualizar y cerrar mantenimiento', 'direct', 'implemented_inaccessible', 'web_native', 'Maintenance', 'workflow', '/operacion/inventario', 'high'],
    ['inventory.unavailable', 'Equipo en mantenimiento no reservable', 'direct', 'implemented_inaccessible', 'web_native', 'Reception', 'conflict', '/operacion/reservas-equipo', 'critical'],
    ['inventory.audit', 'Movimientos y auditoría de activos', 'dependency', 'implemented_but_not_documented', 'web_native', 'StudioManager', 'read', '/operacion/inventario', 'high'],
  ],
  reporting: [
    ['operations.dashboard', 'Panel de operación del estudio', 'direct', 'implemented_inaccessible', 'web_only', 'StudioManager', 'read', '/dashboard/operations', 'high'],
    ['operations.reception', 'Flujo de recepción: llegada, sala, saldo y entrega', 'direct', 'partially_implemented', 'web_only', 'Reception', 'workflow', '/dashboard/operations', 'critical'],
    ['operations.engineer', 'Flujo del ingeniero antes, durante y después', 'direct', 'partially_implemented', 'web_fallback', 'Engineer', 'workflow', '/estudio/ordenes', 'critical'],
    ['reports.operational', 'Reportes operativos del estudio', 'direct', 'implemented_inaccessible', 'web_only', 'StudioManager', 'read', '/estudio/reportes', 'high'],
    ['reports.financial', 'Reportes financieros relacionados con estudio', 'dependency', 'implemented_inaccessible', 'web_only', 'Finance', 'read', '/finanzas/creador-reporte-cuenta', 'high'],
    ['reports.export', 'Exportación y consistencia de totales', 'dependency', 'implemented_inaccessible', 'web_only', 'StudioManager', 'read', '/estudio/reportes', 'high'],
  ],
  live: [
    ['live.public-registration', 'Registro público para TDF Live Sessions', 'direct', 'implemented_accessible', 'web_fallback', 'Artist de pruebas', 'public', '/live-sessions/registro', 'high'],
    ['live.intake-admin', 'Revisión del intake de Live Sessions', 'direct', 'implemented_inaccessible', 'web_only', 'StudioManager', 'workflow', '/estudio/live-sessions', 'high'],
    ['live.musicians-songs', 'Músicos, instrumentos y canciones del intake', 'direct', 'implemented_inaccessible', 'web_only', 'StudioManager', 'mutation', '/estudio/live-sessions', 'high'],
    ['live.files-consent', 'Archivos, consentimiento y límites de carga', 'direct', 'implemented_accessible', 'web_fallback', 'Artist de pruebas', 'security', '/live-sessions/registro', 'critical'],
    ['live.domo-campaign', 'Campaña de TDF Sessions en Domo', 'shared', 'implemented_inaccessible', 'web_only', 'StudioManager', 'workflow', '/estudio/campanas/tdf-sessions-domo', 'medium'],
  ],
  integrations: [
    ['notifications.booking-email', 'Confirmación de reserva por email de pruebas', 'dependency', 'implemented_inaccessible', 'none', 'Sistema', 'integration', '/reservar', 'critical'],
    ['notifications.whatsapp', 'WhatsApp con transporte simulado', 'dependency', 'partially_implemented', 'none', 'Sistema', 'integration', '/whatsapp/ok', 'high'],
    ['notifications.calendar', 'Sincronización de calendario en sandbox', 'dependency', 'implemented_inaccessible', 'web_only', 'Admin de pruebas', 'integration', '/configuracion/integraciones/calendario', 'high'],
    ['notifications.failure', 'Fallo de notificación sin perder la operación', 'dependency', 'implemented_but_not_documented', 'none', 'Sistema', 'integration', '/reservar', 'critical'],
    ['notifications.no-real-recipient', 'Bloqueo de destinatarios y secretos reales', 'dependency', 'implemented_but_not_documented', 'none', 'Admin de pruebas', 'security', '/configuracion/preferencias', 'critical'],
  ],
  domo: [
    ['domo.shared-resources', 'Recursos y fechas compartidos con el estudio', 'shared', 'partially_implemented', 'web_only', 'StudioManager', 'conflict', '/domo-del-pululahua', 'high'],
    ['domo.quotations', 'Cotización Domo vinculada a reserva', 'shared', 'implemented_accessible', 'web_only', 'Customer de pruebas', 'public', '/domo-del-pululahua', 'high'],
    ['domo.deposit', 'Depósito PayPal sandbox de Domo', 'shared', 'implemented_accessible', 'web_only', 'Customer de pruebas', 'integration', '/domo-del-pululahua', 'critical'],
    ['domo.hold-expiry', 'Vencimiento de hold sin bloquear estudio', 'shared', 'implemented_but_not_documented', 'web_only', 'Customer de pruebas', 'conflict', '/domo-del-pululahua', 'critical'],
    ['domo.operations', 'Visibilidad operativa de la reserva Domo', 'shared', 'partially_implemented', 'web_only', 'StudioManager', 'workflow', '/estudio/calendario', 'high'],
  ],
  school: [
    ['school.shared-rooms', 'Salas compartidas con clases', 'shared', 'partially_implemented', 'web_only', 'Reception', 'conflict', '/escuela/clases', 'high'],
    ['school.calendar', 'Clases visibles en calendario compartido', 'shared', 'partially_implemented', 'web_only', 'Reception', 'workflow', '/escuela/clases', 'high'],
    ['school.instructors', 'Instructor y disponibilidad', 'shared', 'implemented_inaccessible', 'web_only', 'Reception', 'workflow', '/escuela/profesores', 'medium'],
    ['school.trial-reservation', 'Reserva de clase de prueba', 'shared', 'implemented_inaccessible', 'web_only', 'Reception', 'workflow', '/escuela/trial-lessons', 'high'],
    ['school.payment', 'Pago de clase que comparte finanzas', 'shared', 'partially_implemented', 'web_only', 'Finance', 'integration', '/finanzas/pagos', 'high'],
  ],
  ddex: [
    ['ddex.session-origin', 'Origen de grabación y sesión', 'dependency', 'partially_implemented', 'web_native', 'LabelRep de pruebas', 'read', '/label/ddex', 'high'],
    ['ddex.participants', 'Participantes, roles e instrumentos', 'dependency', 'partially_implemented', 'web_native', 'LabelRep de pruebas', 'read', '/label/ddex/documents/:id', 'high'],
    ['ddex.equipment', 'Equipo y detalles técnicos pertinentes', 'dependency', 'partially_implemented', 'web_native', 'LabelRep de pruebas', 'read', '/label/ddex/documents/:id', 'medium'],
    ['ddex.recordings-assets', 'Grabaciones y activos originados en estudio', 'dependency', 'partially_implemented', 'web_native', 'LabelRep de pruebas', 'read', '/label/ddex/documents/:id', 'high'],
    ['ddex.rights-metadata', 'Metadatos de derechos vinculados a la grabación', 'dependency', 'partially_implemented', 'web_native', 'LabelRep de pruebas', 'read', '/label/ddex/documents/:id', 'high'],
    ['ddex.validation', 'Validación y mensajes de error DDEX', 'dependency', 'implemented_inaccessible', 'web_native', 'LabelRep de pruebas', 'workflow', '/label/ddex/documents/:id', 'high'],
    ['ddex.import', 'Importación y resolución de conflictos DDEX', 'dependency', 'documented_not_implemented', 'none', 'LabelRep de pruebas', 'read', '/label/ddex/documents/:id/import', 'medium'],
  ],
  permissions: [
    ['roles.reception', 'Permisos mínimos de Reception', 'dependency', 'implemented_inaccessible', 'web_fallback', 'Reception', 'security', '/estudio/calendario', 'critical'],
    ['roles.engineer', 'Permisos mínimos de Engineer', 'dependency', 'implemented_inaccessible', 'web_fallback', 'Engineer', 'security', '/estudio/ordenes', 'critical'],
    ['roles.studio-manager', 'Permisos de Studio Manager', 'dependency', 'implemented_inaccessible', 'web_fallback', 'StudioManager', 'security', '/estudio/calendario', 'critical'],
    ['roles.intern', 'Intern no puede ampliar sus propios permisos', 'dependency', 'implemented_accessible', 'web_fallback', 'Intern', 'security', '/practicas', 'critical'],
    ['roles.direct-api', 'La API rechaza operaciones sin permiso', 'dependency', 'implemented_inaccessible', 'none', 'Intern', 'security', '/api', 'critical'],
    ['roles.temporary-access', 'Solicitud y aprobación temporal de permiso', 'dependency', 'implemented_accessible', 'web_only', 'Intern', 'workflow', '/practicas', 'critical'],
    ['roles.expiry', 'Vencimiento del permiso temporal', 'dependency', 'partially_implemented', 'web_only', 'Intern', 'security', '/practicas', 'critical'],
  ],
  internship: [
    ['intern.clock', 'Registrar entrada y salida', 'dependency', 'implemented_accessible', 'web_fallback', 'Intern', 'workflow', '/practicas', 'high'],
    ['intern.task-privacy', 'Sólo el pasante correcto ve la tarea', 'dependency', 'implemented_accessible', 'web_fallback', 'Intern', 'security', '/practicas', 'critical'],
    ['intern.protected-fields', 'Intern no edita campos protegidos', 'dependency', 'implemented_accessible', 'web_fallback', 'Intern', 'security', '/practicas/tareas/:taskId', 'critical'],
    ['intern.audit-cases', 'Lista estructurada de casos', 'dependency', 'implemented_accessible', 'web_fallback', 'Intern', 'workflow', '/practicas/auditorias/:planId', 'critical'],
    ['intern.executions', 'Ejecuciones e historial de retest', 'dependency', 'implemented_accessible', 'web_fallback', 'Intern', 'workflow', '/practicas/auditorias/:planId', 'critical'],
    ['intern.auto-progress', 'Avance calculado automáticamente', 'dependency', 'implemented_accessible', 'web_fallback', 'Intern', 'workflow', '/practicas/auditorias/:planId', 'critical'],
    ['intern.completion-gate', 'Bloqueo de finalización incompleta', 'dependency', 'implemented_accessible', 'web_fallback', 'Intern', 'security', '/practicas/auditorias/:planId', 'critical'],
    ['intern.daily-summary', 'Resumen de cada jornada', 'dependency', 'implemented_accessible', 'web_fallback', 'Intern', 'workflow', '/practicas/auditorias/:planId', 'high'],
    ['intern.final-summary', 'Resumen final generado y conclusiones', 'dependency', 'implemented_accessible', 'web_fallback', 'Intern', 'workflow', '/practicas/auditorias/:planId', 'high'],
    ['feedback.public-compatibility', 'Formulario público existente', 'shared', 'implemented_accessible', 'web_native', 'Visitante', 'public', '/feedback', 'critical'],
    ['feedback.internal-draft', 'Crear y editar borrador interno', 'dependency', 'implemented_accessible', 'web_fallback', 'Intern', 'mutation', '/feedback/interno/nuevo', 'critical'],
    ['feedback.internal-submit', 'Enviar y seguir reporte interno', 'dependency', 'implemented_accessible', 'web_fallback', 'Intern', 'workflow', '/feedback/interno', 'critical'],
    ['feedback.private-scope', 'Privacidad entre reporteros', 'dependency', 'implemented_accessible', 'web_fallback', 'Intern', 'security', '/feedback/interno', 'critical'],
    ['feedback.admin-triage', 'Confirmar, priorizar, asignar y resolver', 'dependency', 'implemented_inaccessible', 'web_fallback', 'Manager', 'workflow', '/feedback/interno', 'critical'],
    ['feedback.comments-info', 'Comentarios y solicitud de información', 'dependency', 'implemented_accessible', 'web_fallback', 'Intern', 'workflow', '/feedback/interno/:reportId', 'high'],
    ['feedback.evidence', 'Múltiples evidencias y enlaces de video', 'dependency', 'implemented_accessible', 'web_fallback', 'Intern', 'security', '/feedback/interno/:reportId', 'critical'],
    ['feedback.duplicates', 'Advertencia y consolidación de duplicados', 'dependency', 'implemented_accessible', 'web_fallback', 'Manager', 'workflow', '/feedback/interno/:reportId', 'high'],
    ['feedback.retest', 'Solicitud y registro de retest', 'dependency', 'implemented_accessible', 'web_fallback', 'Intern', 'workflow', '/feedback/interno/:reportId', 'critical'],
    ['feedback.audit-history', 'Historial auditable de cambios', 'dependency', 'implemented_accessible', 'web_fallback', 'Manager', 'read', '/feedback/interno/:reportId', 'critical'],
    ['feedback.export', 'Búsqueda, filtros y exportación CSV/JSON', 'dependency', 'implemented_inaccessible', 'web_fallback', 'Manager', 'read', '/feedback/interno', 'high'],
    ['feedback.github-promotion', 'Enlace administrativo a issue de GitHub', 'dependency', 'implemented_inaccessible', 'web_fallback', 'Manager', 'security', '/feedback/interno/:reportId', 'high'],
  ],
};

const platformLabels = {
  web_native: ['native_mobile', 'mobile_web_fallback'],
  web_only: ['web_only', 'not_available_on_mobile'],
  web_fallback: ['mobile_web_fallback'],
  native_contextual: ['native_mobile', 'mobile_web_fallback'],
  mobile_web: ['mobile_web_fallback'],
  none: ['not_available_on_mobile'],
};

const implementationLabels = {
  implemented_accessible: ['implemented_and_accessible'],
  implemented_inaccessible: ['implemented_but_inaccessible_to_intended_role'],
  partially_implemented: ['partially_implemented'],
  implemented_but_not_documented: ['implemented_but_not_documented'],
  documented_not_implemented: ['documented_but_not_implemented'],
};

const scopeLabels = {
  direct: 'directly_within_studio_management_scope',
  dependency: 'required_dependency_of_studio_workflow',
  shared: 'shared_with_another_tdf_business_area',
  out: 'out_of_scope',
};

const fixturesByModule = {
  foundation: 'INT-STUART-01 y cuentas sintéticas con sesión válida, expirada e inactiva.',
  crm: 'CUST-STUDIO-001, ART-STUDIO-001 y LEAD-STUDIO-001; todos ficticios y marcados AUDIT-2026.',
  scheduling: 'ROOM-AUDIT-A, ROOM-AUDIT-B, RES-MIC-001, SVC-GRAB-001 y ventanas horarias fijas de staging.',
  sessions: 'ORDER-AUDIT-001, SESSION-AUDIT-001, ART-STUDIO-001, ENG-STUDIO-001 y MUSICIAN-STUDIO-001.',
  commerce: 'PKG-AUDIT-10H, QUOTE-AUDIT-001, INVOICE-AUDIT-001 y medios sandbox aprobados/rechazados.',
  inventory: 'ASSET-AUDIT-MIC-001, ASSET-AUDIT-CABLE-001 y MAINT-AUDIT-001.',
  reporting: 'Reservas, sesiones, cobros e inventario ficticios con prefijo AUDIT-2026.',
  live: 'LIVE-AUDIT-001, artistas, músicos, instrumentos, canciones y archivos pequeños ficticios.',
  integrations: 'Buzón sink audit@invalid.example, WhatsApp mock, calendario sandbox y claves ficticias.',
  domo: 'DOMO-QUOTE-AUDIT-001, fecha fija, hold corto y PayPal sandbox.',
  school: 'CLASS-AUDIT-001, TEACHER-AUDIT-001 y ROOM-AUDIT-B.',
  ddex: 'ERN ficticio DDEX-AUDIT-001 sin música real ni distribución.',
  permissions: 'INT-STUART-01, INT-OTHER-01, RECEPTION-AUDIT, ENGINEER-AUDIT y MANAGER-AUDIT.',
  internship: 'Proyecto, tarea, casos y reportes AUDIT-2026 creados sólo en la base aislada de pruebas.',
};

const inventory = [];
for (const [moduleKey, rows] of Object.entries(featureRows)) {
  const config = moduleConfig[moduleKey];
  for (const [id, feature, scope, implementation, platform, role, mode, route, criticality, extraEvidence = []] of rows) {
    const implementationClassifications = implementationLabels[implementation];
    inventory.push({
      featureId: id,
      module: config.name,
      feature,
      scopeClassification: scopeLabels[scope],
      implementationClassifications,
      intendedInternAccess: implementation === 'implemented_accessible'
        ? 'available_with_intern_or_public_test_identity'
        : implementation === 'documented_not_implemented'
          ? 'not_executable_documented_limitation'
          : 'requires_approved_temporary_permission_or_scoped_staging_persona',
      documentationClassification: implementation === 'implemented_but_not_documented'
        ? 'implemented_but_not_documented'
        : implementation === 'documented_not_implemented'
          ? 'documented_but_not_implemented'
          : 'implementation_and_route_evidence_documented',
      platformClassifications: platformLabels[platform],
      role,
      mode,
      route,
      criticality,
      applicable: implementation !== 'documented_not_implemented',
      evidence: [...config.evidence, 'tdf-hq/assets/feature-registry.json', ...extraEvidence],
    });
  }
}

inventory.push(
  { featureId: 'out.distribution-publishing', module: 'Scope exclusions', feature: 'Publicar o distribuir música real', scopeClassification: scopeLabels.out, implementationClassifications: ['implemented_and_accessible_to_authorized_label_roles'], intendedInternAccess: 'prohibited', documentationClassification: 'documented_scope_exclusion', platformClassifications: ['web_only', 'native_mobile'], role: 'LabelRep', mode: 'prohibited', route: '/label/releases', criticality: 'critical', applicable: false, evidence: ['tdf-hq/assets/feature-registry.json', 'docs/persona-testing/README.md'] },
  { featureId: 'out.production-mutations', module: 'Scope exclusions', feature: 'Modificar datos de producción', scopeClassification: scopeLabels.out, implementationClassifications: ['prohibited_for_assignment'], intendedInternAccess: 'prohibited', documentationClassification: 'documented_scope_exclusion', platformClassifications: ['not_applicable'], role: 'Intern', mode: 'prohibited', route: null, criticality: 'critical', applicable: false, evidence: ['docs/internships/studio-audit/STUART-GUIDE.es.md'] },
  { featureId: 'out.real-communications', module: 'Scope exclusions', feature: 'Contactar clientes o destinatarios reales', scopeClassification: scopeLabels.out, implementationClassifications: ['prohibited_for_assignment'], intendedInternAccess: 'prohibited', documentationClassification: 'documented_scope_exclusion', platformClassifications: ['not_applicable'], role: 'Intern', mode: 'prohibited', route: null, criticality: 'critical', applicable: false, evidence: ['docs/internships/studio-audit/SECURITY-PRIVACY.md'] },
  { featureId: 'out.real-payments', module: 'Scope exclusions', feature: 'Cobrar o reembolsar dinero real', scopeClassification: scopeLabels.out, implementationClassifications: ['prohibited_for_assignment'], intendedInternAccess: 'prohibited', documentationClassification: 'documented_scope_exclusion', platformClassifications: ['not_applicable'], role: 'Intern', mode: 'prohibited', route: null, criticality: 'critical', applicable: false, evidence: ['docs/internships/studio-audit/SECURITY-PRIVACY.md'] },
);

const actionByMode = {
  read: 'Consulta la pantalla, usa búsqueda o filtros, abre un registro y compara lo visible después de recargar.',
  mutation: 'Crea o modifica únicamente el registro ficticio indicado, guarda una sola vez y vuelve a abrirlo.',
  workflow: 'Completa el flujo con los datos ficticios, comprueba cada cambio de estado y vuelve a cargar.',
  public: 'Abre una ventana privada, completa el flujo público con identidad ficticia y usa el enlace seguro de seguimiento.',
  integration: 'Confirma que el proveedor muestre SANDBOX/MOCK, ejecuta el escenario autorizado y revisa el evento persistido.',
  conflict: 'Prepara dos registros o sesiones concurrentes con la misma sala, recurso o fecha e intenta confirmar ambos.',
  security: 'Prueba primero la interfaz y luego la URL directa; registra el código de respuesta sin forzar ni evadir controles.',
};

const expectedByMode = {
  read: 'La información aparece completa, consistente y comprensible; filtros, recarga y navegación conservan el estado esperado.',
  mutation: 'La operación se guarda una sola vez, se ve al recargar y no altera ningún registro fuera del escenario ficticio.',
  workflow: 'Cada transición válida queda persistida; una transición inválida se rechaza con una explicación recuperable.',
  public: 'El visitante sólo ve su operación mediante el token correcto; no se exponen datos internos ni de otras personas.',
  integration: 'No existe cargo ni comunicación real; el resultado sandbox/mock, importe, moneda, referencia y estado coinciden.',
  conflict: 'Sólo una operación incompatible se confirma; la otra recibe un mensaje claro y no deja datos parciales.',
  security: 'La acción no autorizada se rechaza tanto en la interfaz como en la API y no cambia datos persistidos.',
};

const testCases = [];
const counters = new Map();
for (const item of inventory.filter((entry) => entry.applicable)) {
  const moduleKey = Object.entries(moduleConfig).find(([, config]) => config.name === item.module)?.[0];
  if (!moduleKey) continue;
  const config = moduleConfig[moduleKey];
  const next = (counters.get(moduleKey) ?? 0) + 1;
  counters.set(moduleKey, next);
  const strong = item.criticality === 'critical' || ['integration', 'conflict', 'security'].includes(item.mode);
  testCases.push({
    stableId: `STU-${config.code}-${String(next).padStart(3, '0')}`,
    featureId: item.featureId,
    module: item.module,
    feature: item.feature,
    userRole: item.role,
    objective: `Comprobar ${item.feature.toLocaleLowerCase('es')}.`,
    businessPurpose: `Asegurar que ${item.feature.toLocaleLowerCase('es')} permita operar el estudio de forma clara, segura y trazable.`,
    preconditions: `Entorno staging confirmado. Cuenta o persona de pruebas ${item.role} autorizada para este caso. Transporte externo en sandbox/mock. Si falta el permiso, no uses otra cuenta: marca Bloqueado y solicita acceso desde Prácticas.`,
    requiredTestData: fixturesByModule[moduleKey],
    environment: 'staging',
    platform: item.platformClassifications.includes('native_mobile') ? 'Web y móvil según clasificación' : item.platformClassifications.includes('mobile_web_fallback') ? 'Web de escritorio y web móvil' : 'Web',
    browserOrDevice: item.platformClassifications.includes('native_mobile') ? 'Chrome estable y dispositivo Android/iOS de pruebas' : 'Chrome estable; viewport 390×844 cuando corresponda',
    language: 'Español; repetir etiquetas principales en inglés cuando exista selector',
    detailedSteps: `1. Verifica que la barra del entorno indique staging y que la identidad sea ficticia.\n2. Abre ${item.route || 'la superficie indicada en la tarea'} desde la navegación; anota si no puedes encontrarla.\n3. ${actionByMode[item.mode]}\n4. Recarga y comprueba el resultado visible y el estado persistido indicado.\n5. Repite una vez la acción principal o usa Atrás/Adelante para detectar duplicados o estado obsoleto.\n6. Limpia únicamente los datos AUDIT-2026 creados por este caso.`,
    expectedResult: expectedByMode[item.mode],
    expectedPersistedState: `Existe como máximo un cambio válido asociado a ${item.featureId}; conserva actor, fecha y relaciones ficticias, sin tocar producción.`,
    expectedNotificationsOrSideEffects: item.mode === 'integration' ? 'Sólo outbox, buzón sink o proveedor sandbox; nunca una notificación, cargo o publicación real.' : 'Sólo efectos internos documentados; cualquier notificación usa transporte de pruebas.',
    cleanupInstructions: 'Usa la limpieza idempotente del escenario AUDIT-2026. No borres registros compartidos ni corrijas manualmente un fallo para ocultarlo.',
    criticality: item.criticality,
    resultStatus: 'pending',
    evidenceRequirements: strong ? 'strong' : 'light',
    scheduleDay: config.day,
    estimatedMinutes: strong ? 8 : 5,
    exploratory: false,
    evidence: item.evidence,
  });
}

const edgeScenarios = [
  ['studio.internal-booking', 'Campos vacíos o inválidos en una reserva', 'Reception', 'Enviar sin cliente, sala, servicio o fecha y luego con fechas invertidas.', 'Se señalan campos concretos, el foco llega al primer error y no se guarda una reserva parcial.', 'critical'],
  ['studio.internal-booking', 'Doble clic al guardar reserva', 'Reception', 'Haz doble clic rápido en Guardar y repite la misma solicitud.', 'Existe una sola reserva y una respuesta recuperable.', 'critical'],
  ['studio.booking-concurrency', 'Reserva concurrente desde dos sesiones', 'Reception', 'Confirma el mismo horario desde dos navegadores separados.', 'Una sola confirmación; la perdedora recibe conflicto y datos actuales.', 'critical'],
  ['studio.booking-edit', 'Datos obsoletos al editar una reserva', 'Reception', 'Abre la reserva en dos sesiones, cambia una y guarda la otra.', 'No se sobrescribe silenciosamente el cambio más reciente.', 'critical'],
  ['studio.availability', 'Recurso deja de estar disponible antes de confirmar', 'Reception', 'Selecciona un recurso y márcalo ocupado desde la segunda sesión antes de confirmar.', 'La confirmación se rechaza y ofrece una ruta de recuperación.', 'critical'],
  ['auth.session', 'Sesión expira durante un formulario', 'Intern', 'Deja expirar la sesión y luego intenta guardar.', 'Se solicita iniciar sesión de nuevo sin mostrar éxito falso ni duplicar datos.', 'critical'],
  ['responsive.layout', 'Conectividad interrumpida al guardar', 'Intern', 'Simula offline justo al guardar y restablece la conexión.', 'El estado informa fallo, conserva lo escrito y permite reintento seguro.', 'high'],
  ['responsive.layout', 'Respuesta lenta y acción repetida', 'Intern', 'Aplica latencia y pulsa una segunda vez.', 'Se muestra progreso, se bloquea el doble envío y no hay duplicados.', 'high'],
  ['navigation.discovery', 'Atrás y adelante después de editar', 'Intern', 'Guarda, usa Atrás y Adelante y recarga.', 'No reaparece estado anterior como si fuera actual.', 'medium'],
  ['commerce.datafast', 'Pago Datafast rechazado', 'Customer de pruebas', 'Usa la tarjeta sandbox de rechazo.', 'No se registra pago confirmado ni reserva pagada; se explica cómo reintentar.', 'critical'],
  ['commerce.paypal', 'PayPal cancelado por el usuario', 'Customer de pruebas', 'Abandona o cancela la aprobación sandbox.', 'Orden pendiente/abandonada sin captura ni confirmación falsa.', 'critical'],
  ['commerce.payment-idempotency', 'Callback o webhook de pago duplicado', 'Admin de pruebas', 'Reproduce dos veces el mismo evento firmado de pruebas.', 'Se procesa una vez y queda trazabilidad del duplicado.', 'critical'],
  ['commerce.payment-confirmation', 'Importe o moneda de pago no coincide', 'Admin de pruebas', 'Usa el fixture de evento con importe o moneda diferente.', 'Se rechaza la confirmación y se marca para revisión sin acreditar saldo.', 'critical'],
  ['commerce.reconciliation', 'Pago llega después de vencer el hold', 'Finance', 'Usa el evento sandbox tardío.', 'No se ocupa una fecha ya liberada; queda una excepción conciliable.', 'critical'],
  ['commerce.refunds', 'Reembolso falla en proveedor', 'Finance', 'Usa la respuesta sandbox fallida.', 'El sistema no marca reembolsado y permite seguimiento seguro.', 'critical'],
  ['inventory.reservations', 'Equipo reservado dos veces', 'Reception', 'Reserva el mismo activo para sesiones solapadas.', 'La segunda reserva se rechaza sin cambiar la primera.', 'critical'],
  ['inventory.scan', 'Token de escaneo vencido o alterado', 'Reception', 'Cambia un carácter y usa un token vencido.', 'Respuesta genérica, sin datos del activo ni mutación.', 'critical'],
  ['inventory.checkout-return', 'Devolución con daño o accesorio faltante', 'Reception', 'Registra condición distinta con evidencia ficticia.', 'Se conserva condición inicial/final y se crea seguimiento de mantenimiento.', 'high'],
  ['live.files-consent', 'Archivo prohibido, enorme o con nombre peligroso', 'Artist de pruebas', 'Prueba tipo no permitido, exceso de tamaño y nombre ../secreto.', 'Cada archivo se rechaza antes de persistir y no sale de la ruta autorizada.', 'critical'],
  ['feedback.evidence', 'Adjunto interno con tipo, tamaño o nombre inválido', 'Intern', 'Prueba ejecutable, archivo mayor al límite y nombre con traversal.', 'Se rechaza; no queda archivo huérfano y otro usuario no puede descargarlo.', 'critical'],
  ['feedback.private-scope', 'Otro pasante abre reporte por URL directa', 'Intern', 'Con INT-OTHER-01 abre el UUID del reporte de INT-STUART-01.', 'Recibe no encontrado y no ve metadatos, comentarios ni evidencia.', 'critical'],
  ['feedback.admin-triage', 'Pasante intenta fijar severidad o prioridad', 'Intern', 'Envía PATCH directo con severidad administrativa y prioridad.', 'La API responde 403 y conserva los valores anteriores.', 'critical'],
  ['feedback.duplicates', 'Dos hallazgos similares', 'Intern', 'Crea dos borradores con título, módulo, función y texto similares.', 'Se advierte el posible duplicado sin fusionar ni borrar datos.', 'high'],
  ['feedback.retest', 'Retest fallido conserva ejecución anterior', 'Intern', 'Registra retest fallido después de Listo para retest.', 'Se añade nueva ejecución; el historial anterior permanece inmutable.', 'critical'],
  ['intern.completion-gate', 'Intento de completar con caso crítico pendiente', 'Intern', 'Solicita estado final con un caso crítico pendiente.', 'Backend y UI impiden cierre y enumeran la condición faltante.', 'critical'],
  ['intern.completion-gate', 'Intento de completar con fallo sin reporte', 'Intern', 'Deja un caso Fallido sin reporte vinculado e intenta cerrar.', 'El cierre se rechaza hasta vincular un reporte trazable.', 'critical'],
  ['intern.completion-gate', 'Excepción administrativa justificada', 'Manager', 'Registra justificación y aprobación de excepción en la base aislada.', 'Actor, fecha y justificación quedan auditados; el pasante no puede aprobarla.', 'critical'],
  ['roles.direct-api', 'Acceso directo sin permiso a crear sala', 'Intern', 'Envía la misma solicitud que usa la UI sin permiso Scheduling.', '403 sin fila creada ni datos confidenciales.', 'critical'],
  ['roles.expiry', 'Permiso temporal ya vencido', 'Intern', 'Usa la fecha simulada posterior al fin del permiso.', 'La UI oculta/bloquea y la API rechaza independientemente.', 'critical'],
  ['localization.es-en', 'Error y validación cambian de idioma', 'Intern', 'Provoca el mismo error en español e inglés.', 'Texto comprensible, sin claves crudas ni mezcla involuntaria de idiomas.', 'medium'],
  ['accessibility.baseline', 'Navegación completa sólo con teclado', 'Intern', 'Recorre menú, formulario, diálogo, error y acción principal con Tab/Shift+Tab/Enter/Escape.', 'Foco visible, orden lógico, sin trampas y errores asociados a campos.', 'high'],
  ['accessibility.baseline', 'Contraste y zoom al 200%', 'Intern', 'Activa zoom 200% y modo de alto contraste.', 'Contenido y acciones siguen visibles sin desplazamiento bidimensional innecesario.', 'high'],
  ['responsive.layout', 'Viewport móvil y rotación', 'Intern', 'Prueba 390×844, rota y vuelve atrás.', 'No se pierde contenido, estado ni acción primaria.', 'high'],
  ['feedback.public-compatibility', 'Feedback público conserva consentimiento', 'Visitante', 'Intenta enviar sin consentimiento y luego con consentimiento.', 'Sin consentimiento se rechaza; con consentimiento persiste como feedback público legible.', 'critical'],
  ['notifications.failure', 'Email de pruebas falla después de guardar', 'Sistema', 'Configura el sink para devolver fallo.', 'La operación principal no se pierde y el fallo queda trazable/reintentable.', 'critical'],
];

for (const [featureId, feature, role, scenario, expected, criticality] of edgeScenarios) {
  const inventoryItem = inventory.find((item) => item.featureId === featureId);
  if (!inventoryItem) throw new Error(`Unknown edge-scenario feature: ${featureId}`);
  const moduleKey = Object.entries(moduleConfig).find(([, config]) => config.name === inventoryItem.module)?.[0];
  const config = moduleConfig[moduleKey];
  const next = (counters.get(moduleKey) ?? 0) + 1;
  counters.set(moduleKey, next);
  testCases.push({
    stableId: `STU-${config.code}-${String(next).padStart(3, '0')}`,
    featureId,
    module: inventoryItem.module,
    feature,
    userRole: role,
    objective: feature,
    businessPurpose: 'Comprobar el camino de error o condición límite sin comprometer datos ni ocultar el fallo.',
    preconditions: 'Staging aislado, datos AUDIT-2026 y transporte sandbox/mock confirmados. Dos sesiones sólo cuando el caso lo pide.',
    requiredTestData: fixturesByModule[moduleKey],
    environment: 'staging',
    platform: inventoryItem.platformClassifications.join(', '),
    browserOrDevice: 'Chrome estable; segundo perfil o dispositivo de pruebas cuando se requiera concurrencia',
    language: 'Español',
    detailedSteps: `1. Confirma staging y toma nota del estado inicial.\n2. ${scenario}\n3. Espera la respuesta completa sin repetir más de una vez.\n4. Recarga y comprueba el estado persistido.\n5. Si aparece riesgo para producción, personas, dinero o seguridad, detente y contacta a Diego.`,
    expectedResult: expected,
    expectedPersistedState: 'No hay datos parciales, duplicados ni cambios fuera del fixture; el intento y su resultado son trazables cuando corresponde.',
    expectedNotificationsOrSideEffects: 'Sólo transporte de pruebas. Ninguna persona real, proveedor real ni sistema de producción recibe efectos.',
    cleanupInstructions: 'Limpia sólo el fixture AUDIT-2026 mediante el script idempotente.',
    criticality,
    resultStatus: 'pending',
    evidenceRequirements: 'strong',
    scheduleDay: config.day,
    estimatedMinutes: 8,
    exploratory: false,
    evidence: inventoryItem.evidence,
  });
}

for (const [moduleKey, config] of Object.entries(moduleConfig)) {
  const next = (counters.get(moduleKey) ?? 0) + 1;
  counters.set(moduleKey, next);
  testCases.push({
    stableId: `STU-${config.code}-${String(next).padStart(3, '0')}`,
    featureId: `exploratory.${moduleKey}`,
    module: config.name,
    feature: `Exploración libre: ${config.name}`,
    userRole: 'Intern con permiso temporal aprobado cuando corresponda',
    objective: 'Explorar el área sin seguir una ruta exacta y evaluar si refleja el trabajo real del estudio.',
    businessPurpose: 'Encontrar problemas de descubrimiento, comprensión, consistencia, eficiencia, accesibilidad o recuperación que un guion puede omitir.',
    preconditions: 'Casos guionados principales del módulo ejecutados; staging y datos ficticios confirmados.',
    requiredTestData: fixturesByModule[moduleKey],
    environment: 'staging',
    platform: 'Web de escritorio y móvil disponible; comparar app nativa cuando exista',
    browserOrDevice: 'Chrome estable y dispositivo móvil de pruebas',
    language: 'Español e inglés cuando exista la opción',
    detailedSteps: '1. Intenta encontrar las funciones sin usar el enlace directo del caso.\n2. Completa una tarea realista eligiendo tu propio recorrido.\n3. Evalúa etiquetas, momento de la información, número de pasos, consistencia y recuperación de errores.\n4. Compara escritorio, web móvil y app nativa cuando existan.\n5. Registra por separado cada error, sugerencia, idea, pregunta, problema de accesibilidad, permiso, rendimiento o contenido.',
    expectedResult: 'La exploración queda resumida aunque no se encuentre ningún error. No encontrar o no comprender una función es un resultado válido y debe reportarse.',
    expectedPersistedState: 'Sólo datos AUDIT-2026; observaciones vinculadas al módulo, tarea y caso exploratorio.',
    expectedNotificationsOrSideEffects: 'Ningún efecto real; proveedores y comunicaciones permanecen en sandbox/mock.',
    cleanupInstructions: 'No repares ni ocultes fallos. Limpia solamente datos ficticios creados por la exploración.',
    criticality: 'medium',
    resultStatus: 'pending',
    evidenceRequirements: 'light',
    scheduleDay: config.day,
    estimatedMinutes: 20,
    exploratory: true,
    exploratoryCharter: '¿Puedes encontrar la función? ¿Las palabras son claras? ¿El flujo se parece al trabajo del estudio? ¿Explica qué pasó y cómo recuperarse? ¿Hay pasos innecesarios? ¿Web y móvil coinciden? ¿Qué idea lo haría más rápido, claro, seguro o útil?',
    evidence: config.evidence,
  });
}

const requiredCaseFields = [
  'stableId', 'module', 'feature', 'userRole', 'objective', 'businessPurpose', 'preconditions',
  'requiredTestData', 'environment', 'platform', 'browserOrDevice', 'language', 'detailedSteps',
  'expectedResult', 'expectedPersistedState', 'expectedNotificationsOrSideEffects',
  'cleanupInstructions', 'criticality', 'resultStatus', 'evidenceRequirements',
  'estimatedMinutes',
];
for (const testCase of testCases) {
  for (const field of requiredCaseFields) {
    if (testCase[field] === undefined || testCase[field] === '') throw new Error(`${testCase.stableId} missing ${field}`);
  }
}
if (new Set(testCases.map((item) => item.stableId)).size !== testCases.length) throw new Error('Duplicate stable test-case ID');

const schedule = [
  { day: 1, week: 1, focus: 'Onboarding, prohibiciones, staging, cuentas, entrada/salida y acceso.', hours: '2–3' },
  { day: 2, week: 1, focus: 'CRM, clientes, leads y pipelines de servicios de estudio.', hours: '2–3' },
  { day: 3, week: 1, focus: 'Calendario, salas, recursos, disponibilidad y reservas.', hours: '2–3' },
  { day: 4, week: 1, focus: 'Órdenes, sesiones, participantes, listas de entradas y equipos.', hours: '2–3' },
  { day: 5, week: 1, focus: 'Paquetes, cotizaciones, facturas y pagos sandbox. Revisión de punto medio.', hours: '2–3' },
  { day: 6, week: 2, focus: 'Inventario, mantenimiento, operación y reportes.', hours: '2–3' },
  { day: 7, week: 2, focus: 'Live Sessions, notificaciones e integraciones simuladas.', hours: '2–3' },
  { day: 8, week: 2, focus: 'Dependencias Domo, Escuela y DDEX; límites de alcance.', hours: '2–3' },
  { day: 9, week: 2, focus: 'Móvil, accesibilidad, idiomas, permisos, casos límite y retests.', hours: '2–3' },
  { day: 10, week: 2, focus: 'Cerrar cobertura, responder aclaraciones, informe final y demostración.', hours: '2–3' },
];

const taskSummaryMarkdown = await readFile(path.join(docsDir, 'TASK-DESCRIPTION.es.md'), 'utf8');
const reusableGuideMarkdown = await readFile(path.join(docsDir, 'STUART-GUIDE.es.md'), 'utf8');
const taskDescriptionMarkdown = `${taskSummaryMarkdown.trim()}\n\n---\n\n${reusableGuideMarkdown.trim()}\n`;
const draft = {
  title: 'Auditoría funcional y de experiencia del manejo del estudio',
  activationStatus: 'draft',
  assignmentStatus: 'draft',
  notificationsEnabled: false,
  assignee: {
    status: 'production-identity-verified-runtime-only',
    partyId: null,
    email: null,
    displayName: 'Stewart Moreira',
    repositoryStoresProductionIdentifiers: false,
    runtimeExactMatchRequired: true,
  },
  environment: 'staging',
  durationDaysFromActivation: 14,
  expectedEffortHours: { minimum: 20, maximum: 30 },
  midpointPercent: 50,
  finalReviewAndDemonstrationRequired: true,
  principalAssignment: {
    title: 'Ejecutar la auditoría funcional y de experiencia del manejo del estudio',
    descriptionMarkdown: taskDescriptionMarkdown,
    reusableGuide: 'docs/internships/studio-audit/STUART-GUIDE.es.md',
  },
  testCaseCount: testCases.length,
  applicableFeatureCount: inventory.filter((item) => item.applicable).length,
  inventoryCount: inventory.length,
  schedule,
  completionCriteria: [
    'Todos los casos aplicables tienen resultado registrado.',
    'Cada caso fallido tiene un reporte vinculado.',
    'Casos críticos y fallidos tienen evidencia suficiente.',
    'Bloqueos, retests solicitados y no aplicables están documentados.',
    'Cada jornada tiene resumen y registro de entrada/salida.',
    'El informe final está enviado.',
    'Manager/Admin autoriza la finalización o registra una excepción justificada.',
  ],
};

const stuartAccountDraft = {
  status: 'existing-account-verified-runtime-only',
  exactIdentityRequired: true,
  displayName: 'Stewart Moreira',
  requestedName: 'Stuart',
  spellingDifferenceVerified: true,
  email: null,
  partyId: null,
  accountActive: true,
  observedActiveRoles: ['Intern', 'Reception', 'Customer', 'Fan'],
  observedEffectiveModules: ['Internships', 'CRM', 'Packages', 'Scheduling'],
  requiredPermanentRolesForAssignment: ['Intern'],
  requiredPermanentModulesForAssignment: ['Internships'],
  temporaryModules: [],
  selfServiceRoleGrantAllowed: false,
  permissionRequestWorkflowRequired: true,
  approverRoles: ['Manager', 'Admin'],
  leastPrivilegeReviewRequiredBeforeActivation: true,
  activationRequiresSeparateApproval: true,
  notes: 'Production identity was verified read-only on 2026-08-23. Exact party ID and email are deliberately omitted from source control and must be supplied at runtime. Do not create a duplicate account or broaden production access for staging tests.',
};

const csv = (rows) => {
  if (!rows.length) return '';
  const headers = Object.keys(rows[0]);
  const quote = (value) => `"${String(Array.isArray(value) ? value.join(' | ') : value ?? '').replaceAll('"', '""')}"`;
  return `${headers.map(quote).join(',')}\n${rows.map((row) => headers.map((header) => quote(row[header])).join(',')).join('\n')}\n`;
};

const inventoryCsv = inventory.map((item) => ({ ...item, implementationClassifications: item.implementationClassifications.join(' | '), platformClassifications: item.platformClassifications.join(' | '), evidence: item.evidence.join(' | ') }));
const casesCsv = testCases.map((item) => ({ stableId: item.stableId, module: item.module, feature: item.feature, userRole: item.userRole, environment: item.environment, platform: item.platform, language: item.language, criticality: item.criticality, resultStatus: item.resultStatus, evidenceRequirements: item.evidenceRequirements, scheduleDay: item.scheduleDay, estimatedMinutes: item.estimatedMinutes, exploratory: item.exploratory }));

await mkdir(docsDir, { recursive: true });
await Promise.all([
  writeFile(path.join(here, 'studio-feature-inventory.json'), `${JSON.stringify(inventory, null, 2)}\n`),
  writeFile(path.join(here, 'test-cases.json'), `${JSON.stringify(testCases, null, 2)}\n`),
  writeFile(path.join(here, 'draft-project.json'), `${JSON.stringify(draft, null, 2)}\n`),
  writeFile(path.join(here, 'draft-stuart-account.json'), `${JSON.stringify(stuartAccountDraft, null, 2)}\n`),
  writeFile(path.join(docsDir, 'studio-feature-inventory.csv'), csv(inventoryCsv)),
  writeFile(path.join(docsDir, 'test-case-index.csv'), csv(casesCsv)),
  writeFile(path.join(docsDir, 'generated-summary.json'), `${JSON.stringify({ inventoryCount: inventory.length, applicableFeatureCount: draft.applicableFeatureCount, testCaseCount: testCases.length, exploratoryCaseCount: testCases.filter((item) => item.exploratory).length, strongEvidenceCaseCount: testCases.filter((item) => item.evidenceRequirements === 'strong').length, estimatedExecutionHours: Math.round((testCases.reduce((total, item) => total + item.estimatedMinutes, 0) / 60) * 10) / 10, byModule: Object.fromEntries(Object.values(moduleConfig).map((config) => [config.name, testCases.filter((item) => item.module === config.name).length])) }, null, 2)}\n`),
]);

console.log(`Generated ${inventory.length} inventory rows and ${testCases.length} test cases.`);
