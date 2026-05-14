import { useEffect, useMemo, useState } from 'react';
import {
  Alert,
  Box,
  Button,
  Card,
  CardContent,
  CardHeader,
  Checkbox,
  Chip,
  CircularProgress,
  Dialog,
  DialogActions,
  DialogContent,
  DialogTitle,
  FormControl,
  FormHelperText,
  Grid,
  InputLabel,
  MenuItem,
  Paper,
  Select,
  ListItemText,
  Stack,
  Table,
  TableBody,
  TableCell,
  TableContainer,
  TableHead,
  TableRow,
  Typography,
} from '@mui/material';
import type { SxProps, Theme } from '@mui/material/styles';
import RefreshIcon from '@mui/icons-material/Refresh';
import AutoFixHighIcon from '@mui/icons-material/AutoFixHigh';
import { useMutation, useQuery, useQueryClient, type QueryClient } from '@tanstack/react-query';
import { AdminApi } from '../api/admin';
import { Health } from '../utilities/health';
import type { AdminConsoleCard, AdminUserDTO, AdminUserStatus, AuditLogEntry, RoleKey } from '../api/types';
import { ROLE_OPTIONS, formatRoleList, normalizeRoleList } from '../constants/roles';
import { normalizeRoles } from '../config/menu';

const ADMIN_REFRESH_QUERY_KEYS = [
  ['admin', 'health'],
  ['admin', 'console'],
  ['admin', 'users'],
  ['admin', 'audit'],
] as const;
const BUILT_IN_ADMIN_CARD_IDS = [
  'access-control',
  'access-authorization',
  'authorization',
  'authorization-management',
  'authz',
  'user-management',
  'users',
  'account-management',
  'account-administration',
  'accounts',
  'admin-accounts',
  'workspace-accounts',
  'team-management',
  'team-members',
  'member-management',
  'members',
  'cuentas',
  'cuentas-admin',
  'cuentas-administrativas',
  'gestion-cuentas',
  'gestion-de-cuentas',
  'administracion-cuentas',
  'administracion-de-cuentas',
  'miembros',
  'miembros-equipo',
  'miembros-del-equipo',
  'gestion-equipo',
  'gestion-del-equipo',
  'audit',
  'audit-log',
  'audit-logs',
  'activity-logs',
  'event-logs',
  'system-logs',
  'workspace-logs',
  'change-logs',
  'registros-auditoria',
  'registros-de-auditoria',
  'registros-actividad',
  'registros-de-actividad',
  'registros-eventos',
  'registros-de-eventos',
  'registros-sistema',
  'registros-del-sistema',
  'service-health',
  'service-readiness',
  'backend-health',
  'backend-readiness',
  'backend-status',
  'server-health',
  'server-readiness',
  'server-status',
  'system-health',
  'system-readiness',
  'system-status',
  'health-checks',
  'service-health-checks',
  'service-checks',
  'dependency-checks',
  'system-checks',
  'readiness-checks',
  'health',
  'api-readiness',
  'api-checks',
  'api',
  'database-readiness',
  'database-checks',
  'db-readiness',
  'database',
  'db',
  'base-de-datos',
  'disponibilidad-servicio',
  'disponibilidad-sistema',
  'disponibilidad-api',
  'disponibilidad-base-datos',
  'demo-seed',
  'demo-data',
  'demo-fixtures',
  'sample-data',
  'sample-fixtures',
  'example-data',
  'seed-data',
  'test-data',
  'test-fixtures',
  'datos-prueba',
  'datos-de-prueba',
  'datos-test',
  'fixtures-demo',
  'fixtures-prueba',
  'seed',
  'first-run',
  'getting-started',
  'quick-start',
  'start-here',
  'primeros-pasos',
  'inicio-rapido',
  'admin-console',
  'admin-center',
  'admin-hub',
  'admin-home',
  'admin-landing',
  'admin-panel',
  'admin-navigation',
  'admin-shortcuts',
  'quick-links',
  'quicklinks',
  'quick-actions',
  'navigation-shortcuts',
  'refresh-panel',
  'panel-refresh',
  'admin-refresh',
  'refresh-admin-panel',
  'reload-panel',
  'reload-admin-panel',
  'actualizar-panel',
  'refrescar-panel',
  'actualizar-consola',
  'refrescar-consola',
  'atajos-navegacion',
  'atajos-de-navegacion',
  'accesos-rapidos',
  'acciones-rapidas',
  'administration',
  'administration-center',
  'administration-hub',
  'administration-home',
  'administration-landing',
  'centro-administracion',
  'centro-administrativo',
  'hub-administrativo',
] as const;
const BUILT_IN_ADMIN_CARD_TITLES = [
  'consola de administracion',
  'centro de administracion',
  'centro administrativo',
  'hub administrativo',
  'inicio de administracion',
  'inicio administrativo',
  'panel de administracion',
  'administracion',
  'accesos rapidos',
  'atajos de navegacion',
  'admin navigation',
  'admin shortcuts',
  'quick links',
  'quick actions',
  'navigation shortcuts',
  'acciones rapidas',
  'actualizar panel',
  'refrescar panel',
  'actualizar consola',
  'refrescar consola',
  'refresh panel',
  'refresh admin panel',
  'reload panel',
  'reload admin panel',
  'panel refresh',
  'admin refresh',
  'admin console',
  'admin center',
  'admin hub',
  'admin home',
  'admin landing',
  'admin panel',
  'admin overview',
  'administrative dashboard',
  'administration',
  'administration center',
  'administration hub',
  'administration home',
  'administration landing',
  'administrative overview',
  'workspace overview',
  'workspace summary',
  'resumen del espacio de trabajo',
  'resumen del workspace',
  'resumen administrativo',
  'resumen de administracion',
  'resumen operativo administrativo',
  'panel administrativo',
  'dashboard administrativo',
  'primeros pasos',
  'inicio rapido',
  'guia inicial',
  'bienvenida',
  'guia de bienvenida',
  'getting started',
  'quick start',
  'start here',
  'onboarding',
  'admin getting started',
  'admin quick start',
  'admin onboarding',
  'admin setup guide',
  'administration onboarding',
  'administration setup guide',
  'configuracion',
  'configuracion del sistema',
  'configuracion del espacio de trabajo',
  'configuration',
  'system configuration',
  'settings',
  'preferencias',
  'preferences',
  'ajustes',
  'workspace configuration',
  'workspace settings',
  'system settings',
  'estado del servicio',
  'salud del servicio',
  'estado del sistema',
  'salud',
  'estado de api',
  'estado de la api',
  'salud de api',
  'salud de la api',
  'estado de base de datos',
  'estado de la base de datos',
  'salud de base de datos',
  'salud de la base de datos',
  'service health',
  'service readiness',
  'service status',
  'backend health',
  'backend readiness',
  'backend status',
  'server health',
  'server readiness',
  'server status',
  'system health',
  'system readiness',
  'system status',
  'health checks',
  'service health checks',
  'service checks',
  'dependency checks',
  'system checks',
  'readiness checks',
  'api health',
  'api status',
  'api readiness',
  'api checks',
  'api',
  'api and database',
  'api and db',
  'api database',
  'api db',
  'api y base de datos',
  'api y db',
  'api connection',
  'api connectivity',
  'database health',
  'database',
  'base de datos',
  'db',
  'database connection',
  'database connectivity',
  'database readiness',
  'database status',
  'database checks',
  'db health',
  'db connection',
  'db connectivity',
  'db readiness',
  'db status',
  'disponibilidad del servicio',
  'disponibilidad del sistema',
  'estado del backend',
  'estado backend',
  'salud del backend',
  'salud backend',
  'disponibilidad del backend',
  'disponibilidad backend',
  'estado del servidor',
  'estado servidor',
  'salud del servidor',
  'salud servidor',
  'disponibilidad del servidor',
  'disponibilidad servidor',
  'disponibilidad de api',
  'disponibilidad de la api',
  'disponibilidad de base de datos',
  'disponibilidad de la base de datos',
  'conexion api',
  'conexion de api',
  'conexion de la api',
  'conexion con api',
  'conexion base de datos',
  'conexion de base de datos',
  'conexion de la base de datos',
  'conexion con base de datos',
  'conexion db',
  'conexion de db',
  'conectividad api',
  'conectividad de api',
  'conectividad base de datos',
  'conectividad de base de datos',
  'health',
  'datos de demostracion',
  'datos de ejemplo',
  'demo data',
  'demo seed',
  'demo fixtures',
  'demo workspace',
  'sample data',
  'sample fixtures',
  'example data',
  'seed data',
  'test data',
  'test fixtures',
  'datos de prueba',
  'datos de pruebas',
  'datos de test',
  'fixtures de demo',
  'fixtures de prueba',
  'usuarios y roles',
  'usuarios y permisos',
  'permisos de usuarios',
  'gestion de usuarios',
  'administracion de usuarios',
  'usuarios',
  'gestion de cuentas',
  'administracion de cuentas',
  'cuentas',
  'cuentas admin',
  'cuentas administrativas',
  'cuentas del equipo',
  'admin accounts',
  'account management',
  'account administration',
  'accounts',
  'workspace accounts',
  'team accounts',
  'team management',
  'team members',
  'team member management',
  'member management',
  'members',
  'miembros del equipo',
  'miembros',
  'gestion del equipo',
  'gestion de miembros',
  'users and roles',
  'users and permissions',
  'user permissions',
  'user management',
  'user administration',
  'users',
  'roles y permisos',
  'roles y accesos',
  'accesos y roles',
  'roles',
  'asignacion de roles',
  'asignaciones de roles',
  'gestion de roles',
  'roles and permissions',
  'roles and access',
  'access and roles',
  'role assignments',
  'role review',
  'role management',
  'access overview',
  'user access overview',
  'access review',
  'permission review',
  'permissions review',
  'resumen de accesos',
  'resumen de acceso de usuarios',
  'revision de accesos',
  'revision de permisos',
  'revision de roles',
  'control de acceso',
  'gestion de accesos',
  'administracion de accesos',
  'acceso de usuarios',
  'acceso administrativo',
  'access control',
  'access management',
  'user access',
  'admin access',
  'team access',
  'team permissions',
  'team roles',
  'acceso del equipo',
  'accesos del equipo',
  'permisos del equipo',
  'roles del equipo',
  'seguridad de acceso',
  'seguridad de accesos',
  'seguridad y permisos',
  'access security',
  'security and permissions',
  'security permissions',
  'access authorization',
  'authorization',
  'authorization management',
  'authorization administration',
  'authz',
  'autorizacion',
  'autorizaciones',
  'gestion de autorizaciones',
  'administracion de autorizaciones',
  'gestion de permisos',
  'administracion de permisos',
  'permisos',
  'permisos de acceso',
  'permissions',
  'permission management',
  'permissions management',
  'access permissions',
  'matriz de accesos',
  'matriz de permisos',
  'matriz de roles',
  'access matrix',
  'permission matrix',
  'permissions matrix',
  'role matrix',
  'roles matrix',
  'perfil de acceso',
  'perfiles de acceso',
  'perfiles y permisos',
  'access profile',
  'access profiles',
  'auditoria reciente',
  'historial de auditoria',
  'registro de auditoria',
  'registros de auditoria',
  'historial de actividad',
  'registro de actividad',
  'registros de actividad',
  'registro de eventos',
  'registros de eventos',
  'registro del sistema',
  'registros del sistema',
  'actividad reciente',
  'historial de cambios',
  'registro de cambios',
  'registros de cambios',
  'bitacora de cambios',
  'bitacoras de cambios',
  'bitacora del sistema',
  'bitacoras del sistema',
  'cambios recientes',
  'auditoria',
  'recent audit',
  'recent audits',
  'recent activity',
  'activity feed',
  'recent changes',
  'event history',
  'event log',
  'event logs',
  'activity history',
  'activity log',
  'activity logs',
  'system log',
  'system logs',
  'workspace log',
  'workspace logs',
  'audit trail',
  'admin audit trail',
  'administrative audit trail',
  'change history',
  'change log',
  'change logs',
  'admin activity',
  'audit history',
  'audit log',
  'audit logs',
  'audit',
] as const;
const ADMIN_CONSOLE_PLACEHOLDER_BODY_FRAGMENTS = [
  'estamos trabajando en esta vista',
  'proximamente encontraras la funcionalidad completa aqui',
  'proximamente aqui se podra',
  'si necesitas priorizar esta seccion',
  'comparte los requisitos con el equipo de producto',
  'working on this view',
  'full functionality will be available here soon',
  'coming soon',
  'proximamente',
  'not implemented yet',
  'not implemented',
  'this view is not ready',
  'this workflow is not ready',
  'this module is not ready',
  'this section is not ready',
  'this area is not ready',
  'not ready for this workspace',
  'not ready in this workspace',
  'under construction',
  'work in progress',
  'to be determined',
  'placeholder',
  'lorem ipsum',
  'sample content goes here',
  'sample copy goes here',
  'dummy content goes here',
  'dummy copy goes here',
  'content goes here',
  'copy goes here',
  'replace this copy',
  'replace with real content',
  'tbd',
  'pendiente de implementacion',
  'aun no esta lista',
  'aun no esta listo',
  'todavia no esta lista',
  'todavia no esta listo',
  'esta seccion no esta lista',
  'esta area no esta lista',
  'en desarrollo',
  'en construccion',
  'debe administrarse desde un flujo dedicado',
  'deben administrarse desde un flujo dedicado',
  'should be managed from a dedicated flow',
  'quedara separado',
  'will be separated',
  'usa el menu lateral',
  'abre desde el menu lateral',
  'abre este modulo desde el menu lateral',
  'abre este flujo desde el menu lateral',
  'abre desde configuracion',
  'abre este modulo desde configuracion',
  'abre este flujo desde configuracion',
  'usa configuracion para abrir',
  'use the side navigation',
  'open from the side navigation',
  'open this from the side navigation',
  'open from settings',
  'open this module from settings',
  'open this workflow from settings',
  'use settings to open',
  'no actions available',
  'no available actions',
  'no action required',
  'no action needed',
  'no admin action required',
  'no administrative action required',
  'read-only preview',
  'read only preview',
  'preview-only',
  'preview only',
  'view-only',
  'view only',
  'reference only',
  'informational only',
  'if you need to prioritize this section',
  'share the requirements with the product team',
  'no data available',
  'no data to display',
  'no records available',
  'no records to display',
  'no records to show',
  'no records yet',
  'no records found',
  'no results yet',
  'no results found',
  'no matches',
  'no matches found',
  'no matching records',
  'no matching results',
  'nothing to show',
  'nothing here',
  'nothing here yet',
  'no information available',
  'no information to display',
  'no entries',
  'no entries available',
  'no entries found',
  'no entries yet',
  'no items to display',
  'no items to show',
  'no items yet',
  'no items found',
  'no elements available',
  'no content available',
  'no content',
  'no rows',
  'no rows to display',
  'no rows to show',
  'empty state',
  'select a record to view details',
  'select a row to view details',
  'select an item to view details',
  'choose a record to view details',
  'choose an item to view details',
  'selecciona un registro para ver detalles',
  'selecciona una fila para ver detalles',
  'selecciona un elemento para ver detalles',
  'elige un registro para ver detalles',
  'elige un elemento para ver detalles',
  'n/a',
  'not applicable',
  'no aplica',
  'sin informacion disponible',
  'no hay informacion disponible',
  'no hay informacion para mostrar',
  'sin entradas',
  'no hay entradas',
  'sin contenido disponible',
  'no hay contenido disponible',
  'sin contenido',
  'no hay contenido',
  'nada que mostrar',
  'no hay nada que mostrar',
  'no hay usuarios administrables',
  'no hay eventos de auditoria',
  'sin datos disponibles',
  'sin elementos disponibles',
  'sin elementos para mostrar',
  'sin filas',
  'sin registros',
  'sin resultados',
  'sin coincidencias',
  'no hay filas',
  'no hay registros',
  'no hay resultados',
  'no hay coincidencias',
  'no hay datos disponibles',
  'no hay datos para mostrar',
  'no hay elementos disponibles',
  'no hay elementos para mostrar',
  'aun no hay datos',
  'todavia no hay datos',
  'access denied',
  'not authorized',
  'unauthorized',
  'forbidden',
  'you do not have permission',
  'you don\'t have permission',
  'you cannot access',
  'insufficient permissions',
  'permission required',
  'requires permission',
  'requires additional permissions',
  'contact support to enable',
  'contact support before',
  'ask support to enable',
  'ask your administrator to enable',
  'contact your administrator to enable',
  'requires admin',
  'admin access required',
  'admin role required',
  'temporarily unavailable',
  'unavailable',
  'not available',
  'not yet available',
  'feature disabled',
  'feature is disabled',
  'feature flag disabled',
  'feature flag is disabled',
  'not enabled',
  'not enabled yet',
  'acceso denegado',
  'acceso restringido',
  'no autorizado',
  'no tienes acceso',
  'no tienes permiso',
  'no tienes permisos',
  'sin permisos suficientes',
  'permiso requerido',
  'permisos requeridos',
  'contacta soporte para habilitar',
  'contacta al administrador para habilitar',
  'pide al administrador que habilite',
  'sin acciones disponibles',
  'sin accion requerida',
  'sin acciones requeridas',
  'no requiere accion',
  'no requiere acciones',
  'no se requiere accion',
  'vista de solo lectura',
  'solo de referencia',
  'solo informativo',
  'requiere rol admin',
  'requiere rol de admin',
  'requiere rol administrador',
  'requiere permisos de administrador',
  'solo administradores',
  'temporalmente no disponible',
  'no esta disponible',
  'no disponible',
  'funcionalidad deshabilitada',
  'esta funcionalidad esta deshabilitada',
  'esta funcion esta deshabilitada',
  'modulo deshabilitado',
  'no habilitado',
  'no habilitada',
  'no esta habilitado',
  'no esta habilitada',
  'not configured',
  'not configured yet',
  'not set up',
  'not set up yet',
  'connection required',
  'requires connection',
  'connect an account to enable',
  'connect account to enable',
  'connect your account to enable',
  'connect a provider to enable',
  'connect this integration to enable',
  'not connected',
  'configuration required',
  'requires configuration',
  'needs configuration',
  'must be configured',
  'setup required',
  'requires setup',
  'conexion requerida',
  'requiere conexion',
  'conecta una cuenta para habilitar',
  'conectar una cuenta para habilitar',
  'conecta un proveedor para habilitar',
  'conectar un proveedor para habilitar',
  'conecta esta integracion para habilitar',
  'conectar esta integracion para habilitar',
  'no conectado',
  'no conectada',
  'configuracion requerida',
  'requiere configuracion',
  'necesita configuracion',
  'debe configurarse',
  'no configurado',
  'no esta configurado',
  'sin configurar',
  'aun no esta configurado',
  'todavia no esta configurado',
] as const;
const ADMIN_CONSOLE_PLACEHOLDER_BODY_EXACT_KEYS = new Set([
  'no activity',
  'no activity to display',
  'no activity yet',
  'no audit events',
  'no audit events found',
  'no audit events yet',
  'no admin users found',
  'no registered users',
  'no registered users yet',
  'no administrative audit events yet',
  'no registered audit events',
  'no registered audit events yet',
  'no changes',
  'no changes found',
  'no changes yet',
  'no data',
  'no recent changes',
  'no recent changes found',
  'no recent changes yet',
  'no users',
  'no users found',
  'no users yet',
  'no workflows configured',
  'no workflows configured yet',
  'no workflows available',
  'no workflows available yet',
  'no workflows yet',
  'no recent activity',
  'sin actividad',
  'sin actividad reciente',
  'sin cambios',
  'sin cambios recientes',
  'sin eventos de auditoria',
  'sin eventos de auditoria aun',
  'sin usuarios',
  'sin usuarios aun',
  'aun no hay cambios',
  'aun no hay cambios recientes',
  'aun no hay usuarios administrables',
  'aun no hay usuarios registrados',
  'aun no hay eventos registrados',
  'sin datos',
  'sin flujos configurados',
  'sin flujos disponibles',
  'aun no hay eventos de auditoria',
  'aun no hay usuarios',
  'aun no hay flujos configurados',
  'aun no hay flujos disponibles',
  'todavia no hay eventos de auditoria',
  'todavia no hay usuarios',
  'todavia no hay cambios',
  'todavia no hay cambios recientes',
  'todavia no hay flujos configurados',
  'todavia no hay flujos disponibles',
  'no hay cambios',
  'no hay cambios recientes',
  'no hay usuarios registrados',
  'no hay eventos registrados',
  'no hay flujos configurados',
  'no hay flujos configurados aun',
  'no hay flujos configurados todavia',
  'no hay flujos disponibles',
  'no hay flujos disponibles aun',
  'no hay flujos disponibles todavia',
]);
const ADMIN_CONSOLE_PLACEHOLDER_TITLE_EXACT_KEYS = new Set([
  'coming soon',
  'disabled',
  'empty state',
  'en desarrollo',
  'feature disabled',
  'n/a',
  'no habilitado',
  'no habilitada',
  'no configurado',
  'no data',
  'no disponible',
  'not enabled',
  'not available',
  'not yet available',
  'not configured',
  'connection required',
  'requires connection',
  'not connected',
  'configuration required',
  'requires configuration',
  'conexion requerida',
  'requiere conexion',
  'no conectado',
  'no conectada',
  'configuracion requerida',
  'requiere configuracion',
  'not implemented',
  'not implemented yet',
  'placeholder',
  'proximamente',
  'setup required',
  'sin datos',
  'under construction',
  'work in progress',
]);
const BUILT_IN_ADMIN_CARD_BODY_COPY = [
  'revisa el estado del sistema ajusta permisos y valida cambios recientes desde un solo lugar',
  'review system status adjust permissions and validate recent changes from one place',
  'sigue este recorrido para ubicar cada bloque sin repetir revisiones vacias',
  'follow this walkthrough to find each block without repeating empty reviews',
  'valida el estado del servicio antes de cambiar permisos o repetir una accion',
  'check whether the api and database are ready before changing permissions',
  'la asignacion de roles se administra desde la pantalla de parties',
  'administra aqui la asignacion de roles y permisos del equipo',
  'administra accesos y permisos del equipo desde esta vista',
  'ajusta los accesos desde usuarios y roles para resolver el caso actual',
  'adjust team access from the users and roles workflow',
  'review team access before changing administrative permissions',
  'revisa accesos del equipo antes de cambiar permisos administrativos',
  'confirma el resultado en auditoria reciente antes de seguir con otro cambio',
  'confirm who changed what before repeating an admin action',
  'review administrative activity before repeating access changes',
  'revisa cambios administrativos recientes antes de repetir acciones',
  'review service health users roles and audit activity from one admin landing page',
  'review system status users roles and audit activity from one admin landing page',
  'review system health users roles and audit activity from one admin landing page',
  'confirm service health users roles and audit before changing access',
  'revisa estado del sistema usuarios roles y auditoria desde una sola consola administrativa',
  'confirma salud usuarios roles y auditoria antes de cambiar accesos',
  'generate sample users roles and audit events for review',
  'prepara usuarios y auditoria de demostracion para validar el panel',
  'load temporary admin records for sandbox review',
  'prepare a disposable workspace for onboarding checks',
  'prepara registros temporales para revisar la consola',
  'use quick shortcuts to refresh admin data and load sample records',
  'usa atajos para refrescar el panel y cargar datos de ejemplo',
] as const;
const GETTING_STARTED_ADMIN_SECTIONS = [
  { label: '1. Estado del servicio', targetId: 'admin-service-health' },
  { label: '2. Usuarios y roles', targetId: 'admin-users-and-roles' },
  { label: '3. Auditoría reciente', targetId: 'admin-recent-audit' },
] as const;
const MAX_SINGLE_ADDITIONAL_MODULE_ACTION_TITLE_LENGTH = 32;
const FIRST_RUN_USERS_EMPTY_STATE = 'Aún no hay usuarios administrables.';
const FIRST_RUN_AUDIT_EMPTY_STATE = 'La auditoría aparecerá cuando se registre el primer cambio.';
const ADMIN_USERS_VISIBLE_LIMIT = 5;
const AUDIT_VISIBLE_ENTRY_LIMIT = 5;
const AUDIT_DETAIL_PREVIEW_LIMIT = 72;
const ADMIN_DATE_UNAVAILABLE_LABEL = 'Fecha no disponible';
const HEALTHY_HEALTH_INDICATORS = new Set(['ok', 'healthy', 'up', 'ready']);
const WARNING_HEALTH_INDICATORS = new Set(['degraded', 'warning', 'warn', 'starting']);
const ERROR_HEALTH_INDICATORS = new Set(['down', 'offline', 'error', 'failed', 'fail', 'unhealthy']);
const ADMIN_USER_ROLES_COLUMN_HEADER = 'Roles';
const INLINE_ROLE_SUMMARY_LIMIT = 2;
const ROLE_SAVE_REFRESH_FOLLOWUP = 'Al guardar, usuarios y auditoría se actualizarán automáticamente.';
const AUDIT_ACTION_LABELS: Record<string, string> = {
  'package.created': 'Paquete creado',
  'package.synced': 'Paquete sincronizado',
  'roles.updated': 'Roles actualizados',
};
const AUDIT_ENTITY_LABELS: Record<string, string> = {
  booking: 'Reserva',
  bookings: 'Reservas',
  invoice: 'Factura',
  invoices: 'Facturas',
  package: 'Paquete',
  packages: 'Paquetes',
  party: 'Contacto',
  parties: 'Contactos',
  room: 'Sala',
  rooms: 'Salas',
  session: 'Sesión',
  sessions: 'Sesiones',
  user: 'Usuario',
  users: 'Usuarios',
};

function invalidateAdminPanelQueries(queryClient: QueryClient) {
  ADMIN_REFRESH_QUERY_KEYS.forEach((queryKey) => {
    void queryClient.invalidateQueries({ queryKey: [...queryKey] });
  });
}

function stripAdminConsolePresentationMarkers(value: string) {
  return value
    .trim()
    .replace(/^#{1,6}\s+/, '')
    .replace(/^>\s+/, '')
    .replace(/<\/?(?:strong|b|em|i|code|span)\b[^>]*>/gi, '')
    .replace(/<a\b[^>]*>([^<]*)<\/a>/gi, '$1')
    .replace(/!?\[([^\]\n]+)\]\([^)]+\)/g, '$1')
    .replace(/\*\*([^*\n]+)\*\*/g, '$1')
    .replace(/__([^_\n]+)__/g, '$1')
    .replace(/(^|[\s([{])\*([^*\n]+)\*(?=$|[\s.,:;!?)}\]])/g, '$1$2')
    .replace(/(^|[\s([{])_([^_\n]+)_(?=$|[\s.,:;!?)}\]])/g, '$1$2')
    .replace(/`([^`\n]+)`/g, '$1')
    .replace(/^(?:fallback|preview|planned|draft|placeholder|stub)\s*(?::|-|\u2013|\u2014)\s*/i, '')
    .trim();
}

function normalizeAdminConsoleCardKey(value: string) {
  return stripAdminConsolePresentationMarkers(value)
    .replace(/\s+/g, ' ')
    .normalize('NFD')
    .replace(/[\u0300-\u036f]/g, '')
    .toLowerCase()
    .trim();
}

function normalizeAdminConsoleSectionKey(value: string) {
  return normalizeAdminConsoleCardKey(value)
    .replace(/[^a-z0-9]+/g, ' ')
    .replace(/\s+/g, ' ')
    .trim();
}

function normalizeBuiltInAdminConsoleSectionKey(value: string) {
  return normalizeAdminConsoleSectionKey(value)
    .replace(
      /\b(?:and|y|of|de|del|la|el|the|admin|administration|administrative|administracion|administrativo|administrativa|console|consola|draft|fallback|planned|placeholder|preview|stub|module|modules|modulo|modulos|section|sections|seccion|secciones|page|pages|pagina|paginas|screen|screens|pantalla|pantallas|view|views|vista|vistas|workflow|workflows|flow|flows|flujo|flujos|area|areas|zona|zonas|card|cards|tarjeta|tarjetas|panel|panels|dashboard|dashboards|tile|tiles|widget|widgets|list|lists|lista|listas|table|tables|tabla|tablas|grid|grids|grilla|grillas|tab|tabs|pestana|pestanas|bloque|bloques)\b/g,
      ' ',
    )
    .replace(/\s+/g, ' ')
    .trim();
}

function normalizeAdditionalAdminConsoleModuleKey(value: string) {
  return normalizeBuiltInAdminConsoleSectionKey(value) || normalizeAdminConsoleSectionKey(value);
}

function normalizeAdminConsoleParagraphKey(value: string) {
  return normalizeAdminConsoleSectionKey(value);
}

function getAdminConsoleCardBodyKeys(card: Pick<AdminConsoleCard, 'body'>) {
  return [...new Set(
    card.body
      .map((paragraph) => normalizeAdminConsoleParagraphKey(paragraph))
      .filter((paragraphKey) => paragraphKey.length > 0),
  )]
    .sort();
}

function getAdminConsoleCardBodyKey(card: Pick<AdminConsoleCard, 'body'>) {
  return getAdminConsoleCardBodyKeys(card).join('\n');
}

function findAdminConsoleCardTitleKeyByBodySubset(
  cardsByTitle: ReadonlyMap<string, AdminConsoleCard>,
  bodyKeys: readonly string[],
) {
  if (bodyKeys.length === 0) {
    return undefined;
  }

  const bodyKeySet = new Set(bodyKeys);

  for (const [existingTitleKey, existingCard] of cardsByTitle) {
    const existingBodyKeys = getAdminConsoleCardBodyKeys(existingCard);

    if (existingBodyKeys.length === 0) {
      continue;
    }

    const existingBodyKeySet = new Set(existingBodyKeys);
    const currentContainsExisting = existingBodyKeys.every((paragraphKey) => bodyKeySet.has(paragraphKey));
    const existingContainsCurrent = bodyKeys.every((paragraphKey) => existingBodyKeySet.has(paragraphKey));

    if (currentContainsExisting || existingContainsCurrent) {
      return existingTitleKey;
    }
  }

  return undefined;
}

function escapeRegExp(value: string) {
  return value.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
}

function stripAdminConsoleListMarker(paragraph: string) {
  const withoutListMarker = stripAdminConsolePresentationMarkers(paragraph)
    .replace(/^(?:[-*•]\s+|\d+[.)]\s+)/, '')
    .trim();

  return stripAdminConsolePresentationMarkers(withoutListMarker);
}

function stripAdminConsoleTitlePrefix(paragraph: string, title: string) {
  const trimmedParagraph = stripAdminConsoleListMarker(paragraph);
  const trimmedTitle = title.trim();

  if (trimmedParagraph === '' || trimmedTitle === '') {
    return trimmedParagraph;
  }

  const titlePrefixPattern = new RegExp(`^${escapeRegExp(trimmedTitle)}\\s*[:—–-]\\s*`, 'i');
  const strippedParagraph = trimmedParagraph.replace(titlePrefixPattern, '').trim();

  return strippedParagraph === '' ? trimmedParagraph : strippedParagraph;
}

const BUILT_IN_ADMIN_CARD_ID_KEYS = new Set(
  BUILT_IN_ADMIN_CARD_IDS.map((value) => normalizeBuiltInAdminConsoleSectionKey(value)),
);
const BUILT_IN_ADMIN_CARD_TITLE_KEYS = new Set(
  BUILT_IN_ADMIN_CARD_TITLES.map((value) => normalizeBuiltInAdminConsoleSectionKey(value)),
);
const BUILT_IN_ADMIN_CARD_BODY_KEYS = new Set(
  BUILT_IN_ADMIN_CARD_BODY_COPY.map((value) => normalizeAdminConsoleParagraphKey(value)),
);

function sanitizeAdminConsoleCards(cards: readonly AdminConsoleCard[]) {
  return cards.flatMap((card) => {
    if (card.implemented === false) {
      return [];
    }

    const title = stripAdminConsolePresentationMarkers(card.title);
    if (title === '' || isPlaceholderAdminConsoleTitle(title)) {
      return [];
    }

    const titleParagraphKey = normalizeAdminConsoleParagraphKey(title);
    const seenParagraphs = new Set<string>();
    const body = card.body
      .map((paragraph) => stripAdminConsoleTitlePrefix(paragraph, title))
      .filter((paragraph) => paragraph.length > 0)
      .filter((paragraph) => !isPlaceholderAdminConsoleParagraph(paragraph))
      .filter((paragraph) => !isBuiltInAdminConsoleParagraph(paragraph))
      .filter((paragraph) => {
        const paragraphKey = normalizeAdminConsoleParagraphKey(paragraph);

        if (paragraphKey === '' || seenParagraphs.has(paragraphKey)) {
          return false;
        }

        if (titleParagraphKey !== '' && paragraphKey === titleParagraphKey) {
          return false;
        }

        seenParagraphs.add(paragraphKey);
        return true;
      });

    if (body.length === 0) {
      return [];
    }

    return [{ ...card, title, body }];
  });
}

function isPlaceholderAdminConsoleTitle(title: string) {
  const normalizedTitle = normalizeAdminConsoleCardKey(title);
  const exactPlaceholderKey = normalizedTitle.replace(/[.!?:;]+$/g, '').trim();

  return ADMIN_CONSOLE_PLACEHOLDER_TITLE_EXACT_KEYS.has(exactPlaceholderKey);
}

function isPlaceholderAdminConsoleParagraph(paragraph: string) {
  const normalizedParagraph = normalizeAdminConsoleCardKey(paragraph);
  const exactPlaceholderKey = normalizedParagraph.replace(/[.!?:;]+$/g, '').trim();

  if (ADMIN_CONSOLE_PLACEHOLDER_BODY_EXACT_KEYS.has(exactPlaceholderKey)) {
    return true;
  }

  return ADMIN_CONSOLE_PLACEHOLDER_BODY_FRAGMENTS.some((fragment) => (
    normalizedParagraph.includes(fragment)
  ));
}

function isBuiltInAdminConsoleParagraph(paragraph: string) {
  return BUILT_IN_ADMIN_CARD_BODY_KEYS.has(normalizeAdminConsoleParagraphKey(paragraph));
}

function isDedicatedAdminSectionCard(card: AdminConsoleCard) {
  const normalizedId = normalizeBuiltInAdminConsoleSectionKey(card.cardId);
  const normalizedTitle = normalizeBuiltInAdminConsoleSectionKey(card.title);

  return BUILT_IN_ADMIN_CARD_ID_KEYS.has(normalizedId) || BUILT_IN_ADMIN_CARD_TITLE_KEYS.has(normalizedTitle);
}

function dedupeAdminConsoleCards(cards: readonly AdminConsoleCard[]) {
  const cardsByTitle = new Map<string, AdminConsoleCard>();
  const titleKeyByBody = new Map<string, string>();
  const titleKeyByCardId = new Map<string, string>();

  cards.forEach((card) => {
    const titleKey = normalizeAdditionalAdminConsoleModuleKey(card.title);
    const cardIdKey = normalizeAdditionalAdminConsoleModuleKey(card.cardId);
    const bodyKeys = getAdminConsoleCardBodyKeys(card);
    const bodyKey = bodyKeys.join('\n');
    const existingTitleKey = cardsByTitle.has(titleKey)
      ? titleKey
      : (
        titleKeyByBody.get(bodyKey)
        ?? titleKeyByCardId.get(cardIdKey)
        ?? findAdminConsoleCardTitleKeyByBodySubset(cardsByTitle, bodyKeys)
      );
    const existingCard = existingTitleKey ? cardsByTitle.get(existingTitleKey) : undefined;

    if (!existingTitleKey || !existingCard) {
      cardsByTitle.set(titleKey, { ...card, body: [...card.body] });
      if (bodyKey !== '') {
        titleKeyByBody.set(bodyKey, titleKey);
      }
      if (cardIdKey !== '') {
        titleKeyByCardId.set(cardIdKey, titleKey);
      }
      return;
    }

    const seenParagraphs = new Set(
      existingCard.body.map((paragraph) => normalizeAdminConsoleParagraphKey(paragraph)),
    );
    const mergedBody = [...existingCard.body];

    card.body.forEach((paragraph) => {
      const paragraphKey = normalizeAdminConsoleParagraphKey(paragraph);

      if (seenParagraphs.has(paragraphKey)) {
        return;
      }

      seenParagraphs.add(paragraphKey);
      mergedBody.push(paragraph);
    });

    cardsByTitle.set(existingTitleKey, { ...existingCard, body: mergedBody });
    const mergedBodyKey = getAdminConsoleCardBodyKey({ body: mergedBody });
    if (mergedBodyKey !== '') {
      titleKeyByBody.set(mergedBodyKey, existingTitleKey);
    }
    if (bodyKey !== '') {
      titleKeyByBody.set(bodyKey, existingTitleKey);
    }
    if (cardIdKey !== '') {
      titleKeyByCardId.set(cardIdKey, existingTitleKey);
    }
  });

  return [...cardsByTitle.values()];
}

function compareAdminConsoleCards(left: Pick<AdminConsoleCard, 'cardId' | 'title'>, right: Pick<AdminConsoleCard, 'cardId' | 'title'>) {
  const leftTitleKey = normalizeAdminConsoleSectionKey(left.title);
  const rightTitleKey = normalizeAdminConsoleSectionKey(right.title);

  if (leftTitleKey !== rightTitleKey) {
    return leftTitleKey < rightTitleKey ? -1 : 1;
  }

  const leftIdKey = normalizeAdminConsoleSectionKey(left.cardId);
  const rightIdKey = normalizeAdminConsoleSectionKey(right.cardId);

  if (leftIdKey !== rightIdKey) {
    return leftIdKey < rightIdKey ? -1 : 1;
  }

  return 0;
}

function sortAdminConsoleCards(cards: readonly AdminConsoleCard[]) {
  return [...cards].sort(compareAdminConsoleCards);
}

function preferNonEmptyAdminUserText(primary?: string | null, fallback?: string | null) {
  return primary?.trim() ? primary : fallback;
}

function mergeAdminUserRecords(primary: AdminUserDTO, duplicate: AdminUserDTO): AdminUserDTO {
  return {
    ...primary,
    username: preferNonEmptyAdminUserText(primary.username, duplicate.username) ?? primary.username,
    displayName: preferNonEmptyAdminUserText(primary.displayName, duplicate.displayName),
    partyId: primary.partyId ?? duplicate.partyId,
    roles: normalizeRoleList([...primary.roles, ...duplicate.roles]),
    status: primary.status ?? duplicate.status,
    lastLoginAt: primary.lastLoginAt ?? duplicate.lastLoginAt,
    lastSeenAt: primary.lastSeenAt ?? duplicate.lastSeenAt,
  };
}

function dedupeAdminUsers(users: readonly AdminUserDTO[]) {
  const usersById = new Map<number, AdminUserDTO>();

  users.forEach((user) => {
    const existingUser = usersById.get(user.userId);

    usersById.set(
      user.userId,
      existingUser ? mergeAdminUserRecords(existingUser, user) : user,
    );
  });

  return [...usersById.values()];
}

function compareAdminUsers(left: AdminUserDTO, right: AdminUserDTO) {
  const leftIdentity = summarizeAdminUserIdentity(left);
  const rightIdentity = summarizeAdminUserIdentity(right);
  const leftPrimary = leftIdentity.primary.trim().toLowerCase();
  const rightPrimary = rightIdentity.primary.trim().toLowerCase();

  if (leftPrimary !== rightPrimary) {
    return leftPrimary.localeCompare(rightPrimary);
  }

  const leftUsername = leftIdentity.username.trim().toLowerCase();
  const rightUsername = rightIdentity.username.trim().toLowerCase();

  if (leftUsername !== rightUsername) {
    return leftUsername.localeCompare(rightUsername);
  }

  return left.userId - right.userId;
}

function sortAdminUsers(users: readonly AdminUserDTO[]) {
  return [...users].sort(compareAdminUsers);
}

function isRenderableAdminUser(user: Pick<AdminUserDTO, 'displayName' | 'username'>) {
  return user.username.trim() !== '' || (user.displayName?.trim().length ?? 0) > 0;
}

function preferRicherAuditDiff(primary?: string | null, fallback?: string | null) {
  const normalizedPrimary = primary?.trim() ?? '';
  const normalizedFallback = fallback?.trim() ?? '';

  if (normalizedPrimary === '') {
    return fallback ?? primary;
  }

  if (normalizedFallback === '') {
    return primary;
  }

  return normalizedFallback.length > normalizedPrimary.length ? fallback : primary;
}

function mergeAuditEntries(primary: AuditLogEntry, duplicate: AuditLogEntry): AuditLogEntry {
  return {
    ...primary,
    auditId: preferNonEmptyAdminUserText(primary.auditId, duplicate.auditId) ?? primary.auditId,
    actorId: primary.actorId ?? duplicate.actorId,
    entity: preferNonEmptyAdminUserText(primary.entity, duplicate.entity) ?? primary.entity,
    entityId: preferNonEmptyAdminUserText(primary.entityId, duplicate.entityId) ?? primary.entityId,
    action: preferNonEmptyAdminUserText(primary.action, duplicate.action) ?? primary.action,
    diff: preferRicherAuditDiff(primary.diff, duplicate.diff),
    createdAt: preferNonEmptyAdminUserText(primary.createdAt, duplicate.createdAt) ?? primary.createdAt,
  };
}

function getAuditEntryFingerprint(entry: AuditLogEntry) {
  return [
    entry.entity.trim(),
    entry.entityId.trim(),
    entry.action.trim(),
    entry.actorId ?? '',
    entry.diff?.trim() ?? '',
    entry.createdAt.trim(),
  ].join('::');
}

function dedupeAuditEntries(entries: readonly AuditLogEntry[]) {
  const dedupedEntries: AuditLogEntry[] = [];
  const entryIndexByAuditId = new Map<string, number>();
  const entryIndexByFingerprint = new Map<string, number>();

  entries.forEach((entry) => {
    const auditIdKey = entry.auditId?.trim() ?? '';

    if (auditIdKey !== '') {
      const existingIndex = entryIndexByAuditId.get(auditIdKey);

      if (existingIndex != null) {
        const existingEntry = dedupedEntries[existingIndex];

        if (existingEntry) {
          const mergedEntry = mergeAuditEntries(existingEntry, entry);
          dedupedEntries[existingIndex] = mergedEntry;
          entryIndexByFingerprint.set(getAuditEntryFingerprint(mergedEntry), existingIndex);
        }

        return;
      }
    }

    const fingerprint = getAuditEntryFingerprint(entry);

    if (entryIndexByFingerprint.has(fingerprint)) {
      return;
    }

    const index = dedupedEntries.length;
    dedupedEntries.push(entry);
    entryIndexByFingerprint.set(fingerprint, index);

    if (auditIdKey !== '') {
      entryIndexByAuditId.set(auditIdKey, index);
    }
  });

  return dedupedEntries;
}

function isRenderableAuditEntry(entry: Pick<AuditLogEntry, 'entity' | 'action'>) {
  return entry.entity.trim() !== '' && entry.action.trim() !== '';
}

function parseAdminDateTimestamp(value: string) {
  const timestamp = Date.parse(value.trim());

  return Number.isNaN(timestamp) ? null : timestamp;
}

function getAuditEntryTimestamp(entry: Pick<AuditLogEntry, 'createdAt'>) {
  return parseAdminDateTimestamp(entry.createdAt) ?? Number.NEGATIVE_INFINITY;
}

function hasAuditTimestamp(entry: Pick<AuditLogEntry, 'createdAt'>) {
  return parseAdminDateTimestamp(entry.createdAt) != null;
}

function compareAuditEntries(left: AuditLogEntry, right: AuditLogEntry) {
  const timestampDifference = getAuditEntryTimestamp(right) - getAuditEntryTimestamp(left);

  if (timestampDifference !== 0) {
    return timestampDifference;
  }

  const leftEntity = `${left.entity}::${left.entityId}`.toLowerCase();
  const rightEntity = `${right.entity}::${right.entityId}`.toLowerCase();

  if (leftEntity !== rightEntity) {
    return leftEntity.localeCompare(rightEntity);
  }

  const leftAction = left.action.trim().toLowerCase();
  const rightAction = right.action.trim().toLowerCase();

  if (leftAction !== rightAction) {
    return leftAction.localeCompare(rightAction);
  }

  return (left.auditId ?? '').localeCompare(right.auditId ?? '');
}

function sortAuditEntries(entries: readonly AuditLogEntry[]) {
  return [...entries].sort(compareAuditEntries);
}

function formatDate(value: string) {
  const timestamp = parseAdminDateTimestamp(value);

  return timestamp == null ? ADMIN_DATE_UNAVAILABLE_LABEL : new Date(timestamp).toLocaleString();
}

function formatDateOrDash(value?: string | null) {
  const trimmedValue = value?.trim();

  if (!trimmedValue) {
    return '—';
  }

  return formatDate(trimmedValue);
}

function summarizeAdminUserIdentity(user: Pick<AdminUserDTO, 'displayName' | 'username'>) {
  const displayName = user.displayName?.trim() ?? '';
  const username = user.username.trim();
  const primary = displayName || username;
  const showUsername = displayName !== '' && displayName.toLowerCase() !== username.toLowerCase();

  return { primary, username, showUsername };
}

function buildAdminUserRoleButtonTitle(user: Pick<AdminUserDTO, 'displayName' | 'username' | 'roles'>) {
  const roleSummary = formatEditableRoleList(user.roles);

  return `${buildAdminUserRoleActionName(user)}. Roles actuales: ${roleSummary}`;
}

function formatEditableRoleList(roles?: readonly RoleKey[] | null) {
  const formattedRoles = formatRoleList(roles);

  return formattedRoles === '—' ? 'Sin roles' : formattedRoles;
}

function formatInlineEditableRoleList(roles?: readonly RoleKey[] | null) {
  const normalizedRoles = normalizeRoleSelection(roles);

  if (normalizedRoles.length === 0) {
    return 'Asignar roles';
  }

  if (normalizedRoles.length <= INLINE_ROLE_SUMMARY_LIMIT) {
    return formatEditableRoleList(normalizedRoles);
  }

  const visibleRoles = normalizedRoles.slice(0, INLINE_ROLE_SUMMARY_LIMIT).join(', ');
  const hiddenRoleCount = normalizedRoles.length - INLINE_ROLE_SUMMARY_LIMIT;

  return `${visibleRoles} +${hiddenRoleCount} ${hiddenRoleCount === 1 ? 'rol' : 'roles'}`;
}

function formatInlineAdminUserRoleSummary(roles?: readonly RoleKey[] | null) {
  const normalizedRoles = normalizeRoleSelection(roles);

  if (normalizedRoles.length === 0) {
    return 'Sin roles';
  }

  return formatInlineEditableRoleList(normalizedRoles);
}

function buildAdminUserRoleActionLabel(roles?: readonly RoleKey[] | null) {
  return normalizeRoleSelection(roles).length === 0 ? 'Asignar roles' : 'Editar roles';
}

function buildAdminUserRoleActionName(user: Pick<AdminUserDTO, 'displayName' | 'username' | 'roles'>) {
  return `${buildAdminUserRoleActionLabel(user.roles)} de ${summarizeAdminUserIdentity(user).primary}`;
}

function buildCompactAdminUserRoleActionLabel(
  roles?: readonly RoleKey[] | null,
  options?: { showFullLabel?: boolean },
) {
  if (options?.showFullLabel) {
    return buildAdminUserRoleActionLabel(roles);
  }

  return normalizeRoleSelection(roles).length === 0 ? 'Asignar' : 'Editar';
}

function getAdminUserVisibleIdentityKey(user: Pick<AdminUserDTO, 'displayName' | 'username'>) {
  const identity = summarizeAdminUserIdentity(user);

  return [
    identity.primary.trim().toLowerCase(),
    identity.showUsername ? identity.username.trim().toLowerCase() : '',
  ].join('::');
}

function getAdminUserIdsRequiringPartyId(
  users: readonly Pick<AdminUserDTO, 'userId' | 'displayName' | 'username' | 'partyId'>[],
) {
  const visibleIdentityCounts = new Map<string, number>();

  users.forEach((user) => {
    const identityKey = getAdminUserVisibleIdentityKey(user);
    visibleIdentityCounts.set(identityKey, (visibleIdentityCounts.get(identityKey) ?? 0) + 1);
  });

  return new Set(
    users
      .filter((user) => user.partyId != null)
      .filter((user) => (visibleIdentityCounts.get(getAdminUserVisibleIdentityKey(user)) ?? 0) > 1)
      .map((user) => user.userId),
  );
}

function formatAuditActor(
  actorId: number | null | undefined,
  usersById: ReadonlyMap<number, Pick<AdminUserDTO, 'displayName' | 'username'>>,
) {
  if (actorId == null) {
    return 'Sistema';
  }

  const actor = usersById.get(actorId);
  if (!actor) {
    return `Usuario #${actorId}`;
  }

  const identity = summarizeAdminUserIdentity(actor);
  return identity.showUsername ? `${identity.primary} (${identity.username})` : identity.primary;
}

function hasAuditActor(actorId?: number | null) {
  return actorId != null;
}

function hasAuditDetail(diff?: string | null) {
  return (diff?.trim().length ?? 0) > 0;
}

function formatAuditDetailPreview(diff?: string | null) {
  const detail = diff?.trim();

  if (!detail) {
    return '—';
  }

  const compactDetail = detail.replace(/\s+/g, ' ');

  if (compactDetail.length <= AUDIT_DETAIL_PREVIEW_LIMIT) {
    return compactDetail;
  }

  return `${compactDetail.slice(0, AUDIT_DETAIL_PREVIEW_LIMIT - 1).trimEnd()}…`;
}

function getAuditDetailPreviewTitle(diff?: string | null) {
  const detail = diff?.trim();

  if (!detail) {
    return undefined;
  }

  return formatAuditDetailPreview(diff) === detail ? undefined : detail;
}

function formatAuditAction(action: string) {
  const trimmedAction = action.trim();

  return AUDIT_ACTION_LABELS[trimmedAction] ?? formatFallbackAuditAction(trimmedAction);
}

function formatFallbackAuditAction(action: string) {
  const words = action
    .replace(/([a-z])([A-Z])/g, '$1 $2')
    .split(/[._\s-]+/)
    .filter((word) => word.length > 0);

  if (words.length === 0) {
    return action;
  }

  return words
    .map((word, index) => {
      const normalizedWord = word.toLowerCase();
      return index === 0
        ? `${normalizedWord.charAt(0).toUpperCase()}${normalizedWord.slice(1)}`
        : normalizedWord;
    })
    .join(' ');
}

function getAuditActionTitle(action: string) {
  const formattedAction = formatAuditAction(action);

  return formattedAction === action ? undefined : action;
}

function formatAuditEntity(entity: string) {
  const trimmedEntity = entity.trim();

  return AUDIT_ENTITY_LABELS[trimmedEntity.toLowerCase()] ?? trimmedEntity;
}

function formatAuditEntityReference(entry: Pick<AuditLogEntry, 'entity' | 'entityId'>) {
  const formattedEntity = formatAuditEntity(entry.entity);
  const entityId = entry.entityId.trim();

  return entityId === '' ? formattedEntity : `${formattedEntity} · ${entityId}`;
}

function getAuditEntityReferenceTitle(entry: Pick<AuditLogEntry, 'entity' | 'entityId'>) {
  const rawEntity = entry.entity.trim();
  const rawEntityId = entry.entityId.trim();
  const rawReference = rawEntityId === '' ? rawEntity : `${rawEntity} · ${rawEntityId}`;
  const formattedReference = formatAuditEntityReference(entry);

  return rawReference === formattedReference ? undefined : rawReference;
}

function getAdminUserLastAccess(user: Pick<AdminUserDTO, 'lastSeenAt' | 'lastLoginAt'>) {
  return user.lastSeenAt ?? user.lastLoginAt;
}

function hasAdminUserLastAccessTimestamp(user: Pick<AdminUserDTO, 'lastSeenAt' | 'lastLoginAt'>) {
  const lastAccess = getAdminUserLastAccess(user);

  return lastAccess != null && parseAdminDateTimestamp(lastAccess) != null;
}

function adminUserStatusNeedsAttention(status?: AdminUserStatus | null) {
  return status != null && status !== 'ACTIVE';
}

function adminUserNeedsAccessAttention(user: Pick<AdminUserDTO, 'roles' | 'status'>) {
  return adminUserStatusNeedsAttention(user.status) || normalizeRoleSelection(user.roles).length === 0;
}

function normalizeHealthIndicator(value?: string | null) {
  return value?.trim().toLowerCase() ?? '';
}

function isHealthyHealthIndicator(value?: string | null) {
  return HEALTHY_HEALTH_INDICATORS.has(normalizeHealthIndicator(value));
}

function getHealthStatusChipColor(value?: string | null): 'default' | 'success' | 'warning' | 'error' {
  const normalizedValue = normalizeHealthIndicator(value);

  if (normalizedValue === '') {
    return 'default';
  }

  if (HEALTHY_HEALTH_INDICATORS.has(normalizedValue)) {
    return 'success';
  }

  if (ERROR_HEALTH_INDICATORS.has(normalizedValue)) {
    return 'error';
  }

  if (WARNING_HEALTH_INDICATORS.has(normalizedValue)) {
    return 'warning';
  }

  return 'warning';
}

function shouldShowServiceHealthChip(value?: string | null) {
  return !isHealthyHealthIndicator(value);
}

function formatHealthStatusChipValue(value?: string | null) {
  const normalizedValue = normalizeHealthIndicator(value);

  if (normalizedValue === '') {
    return 'sin dato';
  }

  if (ERROR_HEALTH_INDICATORS.has(normalizedValue)) {
    return 'sin respuesta';
  }

  if (normalizedValue === 'starting') {
    return 'iniciando';
  }

  if (WARNING_HEALTH_INDICATORS.has(normalizedValue)) {
    return 'requiere revisión';
  }

  if (HEALTHY_HEALTH_INDICATORS.has(normalizedValue)) {
    return 'listo';
  }

  return 'requiere revisión';
}

function getHealthStatusChipTitle(label: string, value?: string | null) {
  const trimmedValue = value?.trim();

  return trimmedValue ? `${label}: ${trimmedValue}` : undefined;
}

function normalizeRoleSelection(roles?: readonly RoleKey[] | null) {
  return normalizeRoleList(roles);
}

function sortRoleOptionsForEditor(pinnedRoles: readonly RoleKey[]) {
  const pinnedRoleSet = new Set(normalizeRoleSelection(pinnedRoles));

  return [...ROLE_OPTIONS].sort((left, right) => {
    const leftPinned = pinnedRoleSet.has(left.value);
    const rightPinned = pinnedRoleSet.has(right.value);

    if (leftPinned === rightPinned) {
      return 0;
    }

    return leftPinned ? -1 : 1;
  });
}

function hasRoleSelectionChanged(currentRoles?: readonly RoleKey[] | null, nextRoles?: readonly RoleKey[] | null) {
  const normalizedCurrentRoles = normalizeRoleSelection(currentRoles);
  const normalizedNextRoles = normalizeRoleSelection(nextRoles);

  if (normalizedCurrentRoles.length !== normalizedNextRoles.length) {
    return true;
  }

  return normalizedCurrentRoles.some((role, index) => role !== normalizedNextRoles[index]);
}

function formatRoleGroupLabel(roles: readonly string[]) {
  if (roles.length <= 1) {
    return roles[0] ?? '';
  }

  if (roles.length === 2) {
    return `${roles[0]} y ${roles[1]}`;
  }

  return `${roles.slice(0, -1).join(', ')} y ${roles[roles.length - 1]}`;
}

function buildServiceHealthWarningMessage({
  apiStatus,
  dbStatus,
}: {
  apiStatus?: string | null;
  dbStatus?: string | null;
}) {
  const dependenciesNeedingAttention = [
    !isHealthyHealthIndicator(apiStatus) ? 'API' : null,
    !isHealthyHealthIndicator(dbStatus) ? 'base de datos' : null,
  ].filter((dependency): dependency is string => dependency != null);

  if (dependenciesNeedingAttention.length === 0) {
    return null;
  }

  const dependenciesLabel = formatRoleGroupLabel(dependenciesNeedingAttention);
  const verb = dependenciesNeedingAttention.length === 1 ? 'requiere' : 'requieren';

  return `Atención: ${dependenciesLabel} ${verb} revisión antes de cambiar permisos o seguir con otras acciones administrativas.`;
}

function buildCompactHiddenColumnsDescription(hiddenColumnLabels: readonly string[]) {
  if (hiddenColumnLabels.length === 0) {
    return null;
  }

  const labels = formatRoleGroupLabel(hiddenColumnLabels);
  const verb = hiddenColumnLabels.length === 1 ? 'aparecerá' : 'aparecerán';
  const contextVerb = hiddenColumnLabels.length === 1 ? 'aporte' : 'aporten';
  const sentenceSubject = `${labels.charAt(0).toUpperCase()}${labels.slice(1)}`;

  return `${sentenceSubject} ${verb} cuando ${contextVerb} contexto.`;
}

function buildPendingRoleChangesSummary(
  currentRoles?: readonly RoleKey[] | null,
  nextRoles?: readonly RoleKey[] | null,
) {
  const normalizedCurrentRoles = normalizeRoleSelection(currentRoles);
  const normalizedNextRoles = normalizeRoleSelection(nextRoles);
  const rolesToAdd = normalizedNextRoles.filter((role) => !normalizedCurrentRoles.includes(role));
  const rolesToRemove = normalizedCurrentRoles.filter((role) => !normalizedNextRoles.includes(role));
  const actions: string[] = [];

  if (rolesToAdd.length > 0) {
    actions.push(`agregar ${formatRoleGroupLabel(rolesToAdd)}`);
  }

  if (rolesToRemove.length > 0) {
    actions.push(`quitar ${formatRoleGroupLabel(rolesToRemove)}`);
  }

  if (actions.length === 0) {
    return null;
  }

  return `${actions.length === 1 ? 'Cambio pendiente' : 'Cambios pendientes'}: ${actions.join(' · ')}.`;
}

function getNavigationEquivalentRoleGroups(roles?: readonly RoleKey[] | null) {
  const groupedByNavigationProfile = new Map<string, RoleKey[]>();

  normalizeRoleSelection(roles).forEach((role) => {
    const navigationProfile = normalizeRoles([role])[0] ?? role.toLowerCase();
    const group = groupedByNavigationProfile.get(navigationProfile) ?? [];
    group.push(role);
    groupedByNavigationProfile.set(navigationProfile, group);
  });

  return [...groupedByNavigationProfile.values()]
    .map((group) => normalizeRoleSelection(group))
    .filter((group): group is RoleKey[] => group.length > 1);
}

function buildAdminUsersSectionDescription({
  showLastAccessColumn,
  showStatusColumn,
  isSingleUserSummary,
  primaryRoleActionLabel = 'Editar roles',
  roleEditingDisabled = false,
}: {
  showLastAccessColumn: boolean;
  showStatusColumn: boolean;
  isSingleUserSummary?: boolean;
  primaryRoleActionLabel?: string;
  roleEditingDisabled?: boolean;
}) {
  const editHint = roleEditingDisabled
    ? 'Resuelve el estado del servicio para habilitar la edición de roles desde esta vista.'
    : (
      isSingleUserSummary
        ? `Revisa los roles actuales y usa ${primaryRoleActionLabel} para ajustar permisos desde esta misma vista.`
        : 'Revisa los roles actuales y usa el botón de cada fila para ajustar permisos desde esta misma vista.'
    );
  const hiddenColumnLabels: string[] = [];

  if (!showLastAccessColumn) {
    hiddenColumnLabels.push('último acceso');
  }

  if (!showStatusColumn) {
    hiddenColumnLabels.push('estado');
  }

  const compactViewHint = buildCompactHiddenColumnsDescription(hiddenColumnLabels);

  if (isSingleUserSummary) {
    return compactViewHint ? `${editHint} ${compactViewHint}` : editHint;
  }

  return compactViewHint ? `${editHint} ${compactViewHint}` : editHint;
}

function buildAuditSectionDescription({
  showDateColumn,
  showActorColumn,
  showDetailColumn,
}: {
  showDateColumn: boolean;
  showActorColumn: boolean;
  showDetailColumn: boolean;
}) {
  const hiddenColumnLabels: string[] = [];

  if (!showDateColumn) {
    hiddenColumnLabels.push('fecha');
  }

  if (!showActorColumn) {
    hiddenColumnLabels.push('actor');
  }

  if (!showDetailColumn) {
    hiddenColumnLabels.push('detalle');
  }

  return buildCompactHiddenColumnsDescription(hiddenColumnLabels);
}

function formatAdditionalModuleCountLabel(count: number) {
  return count === 1 ? '1 módulo adicional' : `${count} módulos adicionales`;
}

function shouldCollapseSingleAdditionalModuleActionTitle(title: string) {
  return title.trim().length > MAX_SINGLE_ADDITIONAL_MODULE_ACTION_TITLE_LENGTH;
}

function formatCompactAdditionalModuleActionTitle(title: string) {
  const trimmedTitle = title.trim();

  if (trimmedTitle.length <= MAX_SINGLE_ADDITIONAL_MODULE_ACTION_TITLE_LENGTH) {
    return trimmedTitle;
  }

  const visibleTitleLength = Math.max(MAX_SINGLE_ADDITIONAL_MODULE_ACTION_TITLE_LENGTH - 3, 1);
  return `${trimmedTitle.slice(0, visibleTitleLength).trimEnd()}...`;
}

function buildAdditionalModulesActionCopy({
  cards,
  optionalPrefix,
  avoidSingleTitleRepeat = false,
}: {
  cards: readonly Pick<AdminConsoleCard, 'title'>[];
  optionalPrefix: boolean;
  avoidSingleTitleRepeat?: boolean;
}) {
  const count = cards.length;
  const prefix = optionalPrefix ? 'Opcional: ver' : 'Ver';

  if (count === 0) {
    return { label: `${prefix} módulos adicionales` };
  }

  if (count === 1) {
    const title = cards[0]?.title?.trim() ?? '';

    if (avoidSingleTitleRepeat && title) {
      return {
        label: 'Ver detalles',
        title: `Ver detalles de ${title}`,
        ariaLabel: `Ver detalles de ${title}`,
      };
    }

    const shouldCollapseTitle = shouldCollapseSingleAdditionalModuleActionTitle(title);
    const summary = title
      ? (
        shouldCollapseTitle
          ? formatCompactAdditionalModuleActionTitle(title)
          : title
      )
      : 'módulo adicional';

    return {
      label: `${prefix} ${summary}`,
      title: shouldCollapseTitle && title ? `${prefix} ${title}` : undefined,
      ariaLabel: shouldCollapseTitle && title ? `${prefix} ${title}` : undefined,
    };
  }

  return { label: `${prefix} ${formatAdditionalModuleCountLabel(count)}` };
}

function formatFirstRunAdditionalModulesActionCopy(cards: readonly Pick<AdminConsoleCard, 'title'>[]) {
  if (cards.length === 1) {
    const title = cards[0]?.title?.trim() ?? '';

    return {
      label: 'Opcional: ver 1 módulo adicional',
      title: title ? `Opcional: ver ${title}` : undefined,
      ariaLabel: undefined,
    } as const;
  }

  return buildAdditionalModulesActionCopy({ cards, optionalPrefix: true });
}

function formatStandaloneAdditionalModulesActionCopy(cards: readonly Pick<AdminConsoleCard, 'title'>[]) {
  return buildAdditionalModulesActionCopy({
    cards,
    optionalPrefix: false,
    avoidSingleTitleRepeat: true,
  });
}

function getSingleAdditionalModule(cards: readonly AdminConsoleCard[]) {
  return cards.length === 1 ? (cards[0] ?? null) : null;
}

const STATUS_META: Record<AdminUserStatus, { label: string; color: 'default' | 'success' | 'warning' | 'error' | 'info' }> = {
  ACTIVE: { label: 'Activo', color: 'success' },
  INVITED: { label: 'Invitado', color: 'info' },
  DISABLED: { label: 'Suspendido', color: 'default' },
};

function renderAdditionalModuleCardsGrid({
  cards,
  isFetching,
  isPending,
  id,
  sx,
  hideSingleCardHeader,
}: {
  cards: readonly AdminConsoleCard[];
  isFetching: boolean;
  isPending: boolean;
  id?: string;
  sx?: SxProps<Theme>;
  hideSingleCardHeader?: boolean;
}) {
  const singleCard = hideSingleCardHeader ? getSingleAdditionalModule(cards) : null;

  return (
    <Box id={id} sx={sx}>
      {isFetching && !isPending && (
        <Typography
          variant="caption"
          color="text.secondary"
          sx={{ display: 'block', mb: 1.5 }}
        >
          Actualizando módulos…
        </Typography>
      )}
      <Grid container spacing={2}>
        {cards.map((card) => (
          <Grid item xs={12} md={4} key={card.cardId}>
            <Card variant="outlined">
              {singleCard == null && <CardHeader title={card.title} />}
              <CardContent>
                {card.body.map((paragraph, idx) => (
                  <Typography
                    key={`${card.cardId}-line-${idx}`}
                    variant="body2"
                    color="text.secondary"
                    paragraph={idx < card.body.length - 1}
                  >
                    {paragraph}
                  </Typography>
                ))}
              </CardContent>
            </Card>
          </Grid>
        ))}
      </Grid>
    </Box>
  );
}

function renderSectionLoading(label: string) {
  return (
    <Box sx={{ px: 2, pb: 2 }}>
      <Stack direction="row" alignItems="center" justifyContent="center" spacing={1} sx={{ py: 2 }}>
        <CircularProgress size={18} />
        <Typography variant="body2" color="text.secondary">
          {label}
        </Typography>
      </Stack>
    </Box>
  );
}

function renderFirstRunSectionStatus(label: string, testId: string) {
  return (
    <Box sx={{ px: 2, pb: 2 }}>
      <Chip
        data-testid={testId}
        label={label}
        size="small"
        variant="outlined"
      />
    </Box>
  );
}

export default function AdminConsolePage() {
  const qc = useQueryClient();
  const [rotationWarning, setRotationWarning] = useState(false);
  const [editingUser, setEditingUser] = useState<AdminUserDTO | null>(null);
  const [selectedRoles, setSelectedRoles] = useState<RoleKey[]>([]);
  const [dialogError, setDialogError] = useState<string | null>(null);
  const [showFirstRunAdditionalModules, setShowFirstRunAdditionalModules] = useState(false);
  const [showStandaloneAdditionalModules, setShowStandaloneAdditionalModules] = useState(false);
  const [showAllAdminUsers, setShowAllAdminUsers] = useState(false);
  const [showAllAuditEntries, setShowAllAuditEntries] = useState(false);

  const healthQuery = useQuery({
    queryKey: ['admin', 'health'],
    queryFn: Health.fetch,
    staleTime: 30_000,
  });

  const auditQuery = useQuery({
    queryKey: ['admin', 'audit'],
    queryFn: AdminApi.auditLogs,
    staleTime: 10_000,
  });

  const consoleQuery = useQuery({
    queryKey: ['admin', 'console'],
    queryFn: AdminApi.consolePreview,
    staleTime: 60_000,
  });

  const usersQuery = useQuery({
    queryKey: ['admin', 'users'],
    queryFn: AdminApi.listUsers,
    staleTime: 60_000,
  });

  const seedMutation = useMutation({
    mutationFn: AdminApi.seed,
    onSuccess: () => {
      invalidateAdminPanelQueries(qc);
    },
  });

  const updateRolesMutation = useMutation({
    mutationFn: ({ userId, roles }: { userId: number; roles: RoleKey[] }) =>
      AdminApi.updateUserRoles(userId, roles),
    onSuccess: () => {
      qc.invalidateQueries({ queryKey: ['admin', 'users'] });
      qc.invalidateQueries({ queryKey: ['admin', 'audit'] });
      setEditingUser(null);
      setDialogError(null);
    },
    onError: (error: unknown) => {
      const message = error instanceof Error ? error.message : 'No se pudieron actualizar los roles.';
      setDialogError(message);
    },
  });

  useEffect(() => {
    const handler = () => setRotationWarning(true);
    window.addEventListener('tdf-session-rotation-due', handler as EventListener);
    return () => {
      window.removeEventListener('tdf-session-rotation-due', handler as EventListener);
    };
  }, []);

  useEffect(() => {
    if (editingUser) {
      setSelectedRoles(normalizeRoleList(editingUser.roles));
      setDialogError(null);
    } else {
      setSelectedRoles([]);
      setDialogError(null);
    }
  }, [editingUser]);

  const audits = sortAuditEntries(
    dedupeAuditEntries((auditQuery.data ?? []).filter(isRenderableAuditEntry)),
  );
  const previewCards = sortAdminConsoleCards(dedupeAdminConsoleCards(
    sanitizeAdminConsoleCards(
      consoleQuery.data?.cards?.filter((card) => !isDedicatedAdminSectionCard(card)) ?? [],
    ),
  ));
  const consoleCards: AdminConsoleCard[] = previewCards;
  const consoleError = consoleQuery.isError ? (consoleQuery.error as Error).message : null;
  const users = sortAdminUsers(
    dedupeAdminUsers((usersQuery.data ?? []).filter(isRenderableAdminUser)),
  );
  const usersById = useMemo(
    () => new Map(users.map((user) => [user.userId, user])),
    [users],
  );
  const userIdsRequiringPartyId = useMemo(
    () => getAdminUserIdsRequiringPartyId(users),
    [users],
  );
  const isUsersLoading = usersQuery.isLoading;
  const usersError = usersQuery.isError ? (usersQuery.error as Error).message : null;
  const singleAdminUser = !isUsersLoading && users.length === 1 ? (users[0] ?? null) : null;
  const singleAdminUserIdentity = singleAdminUser ? summarizeAdminUserIdentity(singleAdminUser) : null;
  const singleAdminUserLastAccess = singleAdminUser ? getAdminUserLastAccess(singleAdminUser) : null;
  const singleAdminUserHasLastAccessTimestamp = singleAdminUser
    ? hasAdminUserLastAccessTimestamp(singleAdminUser)
    : false;
  const shouldShowSingleAdminUserStatus = singleAdminUser?.status != null && singleAdminUser.status !== 'ACTIVE';
  const singleAdminUserStatusLabel = shouldShowSingleAdminUserStatus && singleAdminUser?.status
    ? (STATUS_META[singleAdminUser.status]?.label ?? singleAdminUser.status)
    : null;
  const showUsersTable = users.length > 1;
  const visibleAdminUsers = showAllAdminUsers ? users : users.slice(0, ADMIN_USERS_VISIBLE_LIMIT);
  const hiddenAdminUsers = showAllAdminUsers ? [] : users.slice(ADMIN_USERS_VISIBLE_LIMIT);
  const hiddenAdminUserCount = Math.max(users.length - visibleAdminUsers.length, 0);
  const hiddenAdminUserAttentionCount = hiddenAdminUsers.filter((user) =>
    adminUserNeedsAccessAttention(user),
  ).length;
  const hiddenAdminUserAttentionSuffix = hiddenAdminUserAttentionCount > 0
    ? ` (${hiddenAdminUserAttentionCount} ${hiddenAdminUserAttentionCount === 1 ? 'requiere' : 'requieren'} atención)`
    : '';
  const showAdminUsersOverflowAction = showUsersTable && users.length > ADMIN_USERS_VISIBLE_LIMIT;
  const adminUsersOverflowActionLabel = showAllAdminUsers
    ? `Mostrar solo ${ADMIN_USERS_VISIBLE_LIMIT} usuarios`
    : `Ver ${hiddenAdminUserCount} ${hiddenAdminUserCount === 1 ? 'usuario más' : 'usuarios más'}${hiddenAdminUserAttentionSuffix}`;
  const showUsersLastAccessColumn = visibleAdminUsers.some((user) => hasAdminUserLastAccessTimestamp(user));
  const showUsersStatusColumn = visibleAdminUsers.some((user) => adminUserStatusNeedsAttention(user.status));
  const singleAuditEntry = !auditQuery.isLoading && audits.length === 1 ? (audits[0] ?? null) : null;
  const singleAuditHasTimestamp = singleAuditEntry ? hasAuditTimestamp(singleAuditEntry) : false;
  const singleAuditHasActor = hasAuditActor(singleAuditEntry?.actorId);
  const singleAuditHasDetail = hasAuditDetail(singleAuditEntry?.diff);
  const showAuditTable = audits.length > 1;
  const visibleAuditEntries = showAllAuditEntries ? audits : audits.slice(0, AUDIT_VISIBLE_ENTRY_LIMIT);
  const hiddenAuditEntries = showAllAuditEntries ? [] : audits.slice(AUDIT_VISIBLE_ENTRY_LIMIT);
  const hiddenAuditEntryCount = Math.max(audits.length - visibleAuditEntries.length, 0);
  const hiddenAuditEntryContextCount = hiddenAuditEntries.filter((entry) =>
    hasAuditActor(entry.actorId) || hasAuditDetail(entry.diff),
  ).length;
  const hiddenAuditEntryContextSuffix = hiddenAuditEntryContextCount > 0
    ? ` (${hiddenAuditEntryContextCount} con actor o detalle)`
    : '';
  const showAuditOverflowAction = showAuditTable && audits.length > AUDIT_VISIBLE_ENTRY_LIMIT;
  const auditOverflowActionLabel = showAllAuditEntries
    ? 'Mostrar solo cambios recientes'
    : `Ver ${hiddenAuditEntryCount} ${hiddenAuditEntryCount === 1 ? 'cambio anterior' : 'cambios anteriores'}${hiddenAuditEntryContextSuffix}`;
  const showAuditDateColumn = visibleAuditEntries.some((entry) => hasAuditTimestamp(entry));
  const showAuditActorColumn = visibleAuditEntries.some((entry) => hasAuditActor(entry.actorId));
  const showAuditDetailColumn = visibleAuditEntries.some((entry) => hasAuditDetail(entry.diff));
  const isAdminPanelBaselining = consoleQuery.isPending || usersQuery.isLoading || auditQuery.isLoading;
  const hasAdminPanelError =
    healthQuery.isError
    || auditQuery.isError
    || consoleQuery.isError
    || usersQuery.isError;
  const hasRequiredFirstRunError =
    healthQuery.isError
    || auditQuery.isError
    || usersQuery.isError;
  const shouldShowHealthLoadingState = healthQuery.isPending && healthQuery.data == null;
  const showCompactHealthyServiceSummary =
    healthQuery.data != null
    && isHealthyHealthIndicator(healthQuery.data.status)
    && isHealthyHealthIndicator(healthQuery.data.db);
  const canEditAdminRoles = showCompactHealthyServiceSummary;
  const serviceHealthWarningMessage = buildServiceHealthWarningMessage({
    apiStatus: healthQuery.data?.status,
    dbStatus: healthQuery.data?.db,
  });
  const serviceHealthStatusChips = healthQuery.data
    ? [
      {
        key: 'api',
        label: `API: ${formatHealthStatusChipValue(healthQuery.data.status)}`,
        title: getHealthStatusChipTitle('API', healthQuery.data.status),
        value: healthQuery.data.status,
      },
      {
        key: 'db',
        label: `Base de datos: ${formatHealthStatusChipValue(healthQuery.data.db)}`,
        title: getHealthStatusChipTitle('Base de datos', healthQuery.data.db),
        value: healthQuery.data.db,
      },
    ].filter((chip) => shouldShowServiceHealthChip(chip.value))
    : [];
  const showGettingStartedGuidance =
    !usersQuery.isLoading
    && !auditQuery.isLoading
    && users.length === 0
    && audits.length === 0;
  const hasFirstRunDataError = usersQuery.isError || auditQuery.isError;
  const showFirstRunDemoAction =
    showGettingStartedGuidance
    && showCompactHealthyServiceSummary
    && !hasFirstRunDataError
    && !seedMutation.isSuccess;
  const showFirstRunServiceHealthGate =
    showGettingStartedGuidance && !showCompactHealthyServiceSummary;
  const gettingStartedSections = showFirstRunServiceHealthGate
    ? GETTING_STARTED_ADMIN_SECTIONS.slice(0, 1)
    : GETTING_STARTED_ADMIN_SECTIONS;
  const showGettingStartedSectionLinks = gettingStartedSections.length > 1;
  const firstRunServiceNeedsRefresh =
    showFirstRunServiceHealthGate && !shouldShowHealthLoadingState;
  const showConsoleError = consoleError && !showGettingStartedGuidance;
  const showHeaderRefreshAction =
    hasAdminPanelError || (!isAdminPanelBaselining && (!showGettingStartedGuidance || firstRunServiceNeedsRefresh));
  const showFirstRunRefreshAction =
    showGettingStartedGuidance && (hasRequiredFirstRunError || firstRunServiceNeedsRefresh);
  const showHeaderActions = showHeaderRefreshAction && !showGettingStartedGuidance;
  const usersSectionDescription = showGettingStartedGuidance || users.length === 0
    ? null
    : buildAdminUsersSectionDescription({
      showLastAccessColumn: showUsersLastAccessColumn,
      showStatusColumn: showUsersStatusColumn,
      isSingleUserSummary: singleAdminUser != null && !showUsersTable,
      primaryRoleActionLabel: singleAdminUser != null && !showUsersTable
        ? buildAdminUserRoleActionLabel(singleAdminUser.roles)
        : 'Editar roles',
      roleEditingDisabled: !canEditAdminRoles,
    });
  const auditSectionDescription = showGettingStartedGuidance
    ? null
    : (
      showAuditTable
        ? buildAuditSectionDescription({
          showDateColumn: showAuditDateColumn,
          showActorColumn: showAuditActorColumn,
          showDetailColumn: showAuditDetailColumn,
        })
        : null
    );
  const firstRunAdditionalModulesActionCopy = formatFirstRunAdditionalModulesActionCopy(consoleCards);
  const canShowFirstRunAdditionalModules =
    showGettingStartedGuidance && showCompactHealthyServiceSummary && !hasFirstRunDataError;
  const shouldShowAdditionalModuleCards =
    consoleCards.length > 0
    && (!showGettingStartedGuidance || (canShowFirstRunAdditionalModules && showFirstRunAdditionalModules));
  const showFirstRunAdditionalModulesShowAction =
    canShowFirstRunAdditionalModules
    && consoleCards.length > 0
    && !shouldShowAdditionalModuleCards;
  const showFirstRunAdditionalModulesHideAction =
    canShowFirstRunAdditionalModules
    && shouldShowAdditionalModuleCards;
  const showStandaloneAdditionalModulesSection = consoleCards.length > 0 && !showGettingStartedGuidance;
  const standaloneAdditionalModulesActionCopy = formatStandaloneAdditionalModulesActionCopy(consoleCards);
  const singleAdditionalModule = getSingleAdditionalModule(consoleCards);
  const firstRunAdditionalModulesTitle = singleAdditionalModule?.title ?? 'Módulos opcionales';
  const firstRunAdditionalModulesDescription = singleAdditionalModule
    ? 'Revísalo aquí solo si ya necesitas este flujo extra, sin salir del recorrido inicial.'
    : 'Revísalos aquí solo si ya necesitas ese flujo extra, sin salir del recorrido inicial.';
  const firstRunAdditionalModulesHideLabel = singleAdditionalModule
    ? 'Ocultar módulo opcional'
    : 'Ocultar módulos adicionales';
  const standaloneAdditionalModulesTitle = singleAdditionalModule?.title ?? 'Módulos adicionales';
  const standaloneAdditionalModulesDescription = singleAdditionalModule
    ? 'Tarjeta auxiliar del panel. Ábrela solo cuando ya confirmaste salud, usuarios y auditoría.'
    : 'Tarjetas auxiliares del panel. Ábrelas solo cuando ya confirmaste salud, usuarios y auditoría.';
  const standaloneAdditionalModulesHideLabel = singleAdditionalModule
    ? 'Ocultar módulo adicional'
    : 'Ocultar módulos adicionales';
  const showStandaloneAdditionalModulesOptionalChip = singleAdditionalModule != null;
  const additionalModuleSignature = JSON.stringify(
    consoleCards.map((card) => [card.cardId, card.title, card.body]),
  );
  const adminUserSignature = JSON.stringify(
    users.map((user) => [
      user.userId,
      user.username,
      user.displayName,
      user.partyId,
      user.roles,
      user.status,
      user.lastLoginAt,
      user.lastSeenAt,
    ]),
  );
  const auditEntrySignature = JSON.stringify(
    audits.map((entry) => [
      entry.auditId,
      entry.actorId,
      entry.entity,
      entry.entityId,
      entry.action,
      entry.diff,
      entry.createdAt,
    ]),
  );
  useEffect(() => {
    if (showGettingStartedGuidance) {
      setShowFirstRunAdditionalModules(false);
    } else {
      setShowStandaloneAdditionalModules(false);
    }
  }, [additionalModuleSignature, showGettingStartedGuidance]);
  useEffect(() => {
    if (consoleCards.length === 0) {
      setShowFirstRunAdditionalModules(false);
      setShowStandaloneAdditionalModules(false);
    }
  }, [consoleCards.length]);
  useEffect(() => {
    setShowAllAuditEntries(false);
  }, [auditEntrySignature]);
  useEffect(() => {
    setShowAllAdminUsers(false);
  }, [adminUserSignature]);
  const editingTitle = useMemo(() => {
    if (!editingUser) return '';
    return editingUser.displayName?.trim() || editingUser.username;
  }, [editingUser]);
  const editingActionLabel = editingUser ? buildAdminUserRoleActionLabel(editingUser.roles) : 'Editar roles';
  const currentRoleSummary = useMemo(() => (
    editingUser ? formatEditableRoleList(editingUser.roles) : 'Sin roles'
  ), [editingUser]);
  const hasPendingRoleChanges = useMemo(() => (
    editingUser ? hasRoleSelectionChanged(editingUser.roles, selectedRoles) : false
  ), [editingUser, selectedRoles]);
  const selectedRoleFullSummary = formatEditableRoleList(selectedRoles);
  const selectedRoleControlSummary = formatInlineEditableRoleList(selectedRoles);
  const selectedRoleControlTitle = selectedRoleControlSummary === selectedRoleFullSummary
    ? undefined
    : selectedRoleFullSummary;
  const pendingRoleChangesSummary = useMemo(() => (
    editingUser ? buildPendingRoleChangesSummary(editingUser.roles, selectedRoles) : null
  ), [editingUser, selectedRoles]);
  const equivalentRoleWarning = useMemo(() => {
    if (!hasPendingRoleChanges) {
      return null;
    }

    const groups = getNavigationEquivalentRoleGroups(selectedRoles);
    if (groups.length === 0) {
      return null;
    }

    return groups
      .map((group) => formatRoleGroupLabel(group))
      .join(' · ');
  }, [hasPendingRoleChanges, selectedRoles]);
  const roleSelectionHelperText = useMemo(() => {
    if (!hasPendingRoleChanges) {
      return 'Sin cambios pendientes. Modifica la selección para mostrar Guardar cambios.';
    }

    const pendingSummary = pendingRoleChangesSummary ?? 'Hay cambios pendientes. Revisa la selección antes de guardar.';

    if (!equivalentRoleWarning) {
      return `${pendingSummary} ${ROLE_SAVE_REFRESH_FOLLOWUP}`;
    }

    return `${pendingSummary} Nota: ${equivalentRoleWarning} muestran la misma navegación principal; revisa si necesitas todos antes de guardar. ${ROLE_SAVE_REFRESH_FOLLOWUP}`;
  }, [equivalentRoleWarning, hasPendingRoleChanges, pendingRoleChangesSummary]);
  const currentRolesForEditor = useMemo(
    () => (editingUser ? normalizeRoleSelection(editingUser.roles) : []),
    [editingUser],
  );
  const roleOptionsForEditor = useMemo(
    () => sortRoleOptionsForEditor(currentRolesForEditor),
    [currentRolesForEditor],
  );
  const isRefreshingPanel =
    healthQuery.isFetching
    || auditQuery.isFetching
    || consoleQuery.isFetching
    || usersQuery.isFetching;
  const firstRunDemoActionCopy = {
    description: 'Carga datos de ejemplo para revisar usuarios, roles y auditoría sin tocar producción.',
    buttonLabel: 'Cargar datos de ejemplo',
    pendingLabel: 'Cargando ejemplo…',
  } as const;
  const firstRunServiceGateCopy = shouldShowHealthLoadingState
    ? 'Espera la comprobación de API y base de datos para habilitar usuarios, auditoría y datos de ejemplo.'
    : 'Primero resuelve el estado del servicio; luego se habilitarán usuarios, auditoría y datos de ejemplo.';
  const gettingStartedDescription = showFirstRunServiceHealthGate
    ? firstRunServiceGateCopy
    : 'Sigue este recorrido para ubicar cada bloque sin repetir revisiones vacías.';
  const firstRunDataGateCopy = 'Actualiza el panel para confirmar usuarios y auditoría antes de cargar datos de ejemplo.';
  const firstRunDemoStatusCopy = seedMutation.isSuccess
    ? null
    : showFirstRunServiceHealthGate
      ? null
      : hasFirstRunDataError
        ? firstRunDataGateCopy
        : showFirstRunDemoAction
          ? null
          : firstRunServiceGateCopy;
  const firstRunRefreshActionCopy = showFirstRunServiceHealthGate || !hasFirstRunDataError
    ? {
      label: 'Revisar estado del servicio',
      ariaLabel: 'Volver a comprobar API y base de datos',
      title: 'Volver a comprobar API y base de datos',
    }
    : {
      label: 'Reintentar carga inicial',
      ariaLabel: 'Reintentar carga de usuarios y auditoría',
      title: 'Reintentar carga de usuarios y auditoría',
    };
  const demoSeedActionCopy = {
    successMessage: 'Datos de demostración preparados correctamente.',
  } as const;

  const handleCloseDialog = () => {
    if (updateRolesMutation.isPending) return;
    setEditingUser(null);
  };

  const handleSaveRoles = () => {
    if (!editingUser || !hasPendingRoleChanges) return;
    setDialogError(null);
    updateRolesMutation.mutate({ userId: editingUser.userId, roles: selectedRoles });
  };

  const handleRefreshPanel = () => {
    invalidateAdminPanelQueries(qc);
  };

  const renderStatus = (status?: AdminUserStatus | null, options?: { hideActive?: boolean }) => {
    if (!status) return null;
    if (options?.hideActive && status === 'ACTIVE') return null;
    const meta = STATUS_META[status];
    if (!meta) {
      return status;
    }
    return <Chip label={meta.label} color={meta.color} size="small" />;
  };

  return (
    <Stack spacing={3}>
      <Stack
        direction={{ xs: 'column', sm: 'row' }}
        spacing={2}
        justifyContent="space-between"
        alignItems={{ xs: 'flex-start', sm: 'center' }}
      >
        <Box>
          <Typography variant="h5">Consola de administración</Typography>
          <Typography variant="body2" color="text.secondary" sx={{ mt: 0.5 }}>
            Revisa el estado del sistema, ajusta permisos y valida cambios recientes desde un solo lugar.
          </Typography>
        </Box>
        {showHeaderActions && (
          <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1.5}>
            {showHeaderRefreshAction && (
              <Button
                variant="outlined"
                startIcon={<RefreshIcon />}
                onClick={handleRefreshPanel}
                disabled={isRefreshingPanel}
              >
                {isRefreshingPanel ? 'Actualizando panel…' : 'Actualizar panel'}
              </Button>
            )}
          </Stack>
        )}
      </Stack>

      {rotationWarning && (
        <Alert severity="warning" onClose={() => setRotationWarning(false)}>
          Tu sesión lleva 30 días activa. Genera un nuevo token o vuelve a autenticarte para mantener la seguridad.
        </Alert>
      )}

      {seedMutation.isSuccess && (
        <Alert severity="success">
          {demoSeedActionCopy.successMessage}
        </Alert>
      )}

      {showGettingStartedGuidance && (
        <Alert severity="info" variant="outlined">
          <Stack spacing={1.5}>
            <Box>
              <Typography variant="subtitle2">Primeros pasos</Typography>
              <Typography variant="body2" color="text.secondary" sx={{ mt: 0.5 }}>
                {gettingStartedDescription}
              </Typography>
            </Box>
            {showGettingStartedSectionLinks && (
              <Stack direction="row" spacing={1} useFlexGap flexWrap="wrap">
                {gettingStartedSections.map((section) => (
                  <Chip
                    key={section.targetId}
                    component="a"
                    clickable
                    color="info"
                    variant="outlined"
                    label={section.label}
                    href={`#${section.targetId}`}
                  />
                ))}
              </Stack>
            )}
            {(firstRunDemoStatusCopy || showFirstRunDemoAction || showFirstRunRefreshAction) && (
              <Stack spacing={1} alignItems="flex-start">
                {firstRunDemoStatusCopy && (
                  <Typography variant="body2" color="text.secondary">
                    {firstRunDemoStatusCopy}
                  </Typography>
                )}
                {showFirstRunRefreshAction && (
                  <Button
                    variant="outlined"
                    size="small"
                    startIcon={<RefreshIcon />}
                    onClick={handleRefreshPanel}
                    disabled={isRefreshingPanel}
                    aria-label={firstRunRefreshActionCopy.ariaLabel}
                    title={firstRunRefreshActionCopy.title}
                  >
                    {isRefreshingPanel ? 'Actualizando panel…' : firstRunRefreshActionCopy.label}
                  </Button>
                )}
                {showFirstRunDemoAction && (
                  <Button
                    variant="contained"
                    size="small"
                    startIcon={<AutoFixHighIcon />}
                    onClick={() => seedMutation.mutate()}
                    disabled={seedMutation.isPending}
                    title={firstRunDemoActionCopy.description}
                  >
                    {seedMutation.isPending ? firstRunDemoActionCopy.pendingLabel : firstRunDemoActionCopy.buttonLabel}
                  </Button>
                )}
              </Stack>
            )}
            {showFirstRunAdditionalModulesShowAction && (
              <Button
                size="small"
                variant="text"
                onClick={() => setShowFirstRunAdditionalModules(true)}
                aria-controls="admin-additional-modules-list"
                aria-expanded="false"
                aria-label={firstRunAdditionalModulesActionCopy.ariaLabel}
                title={firstRunAdditionalModulesActionCopy.title}
                sx={{ alignSelf: 'flex-start' }}
              >
                {firstRunAdditionalModulesActionCopy.label}
              </Button>
            )}
            {showFirstRunAdditionalModulesHideAction && (
              <Stack
                spacing={1.5}
                sx={{
                  pt: 1.5,
                  borderTop: '1px solid',
                  borderColor: 'divider',
                }}
              >
                <Stack
                  direction={{ xs: 'column', sm: 'row' }}
                  spacing={1}
                  justifyContent="space-between"
                  alignItems={{ xs: 'flex-start', sm: 'center' }}
                >
                  <Box>
                    <Typography variant="subtitle2">{firstRunAdditionalModulesTitle}</Typography>
                    <Typography variant="body2" color="text.secondary" sx={{ mt: 0.5 }}>
                      {firstRunAdditionalModulesDescription}
                    </Typography>
                  </Box>
                  <Button
                    size="small"
                    variant="text"
                    onClick={() => setShowFirstRunAdditionalModules(false)}
                    aria-controls="admin-additional-modules-list"
                    aria-expanded="true"
                  >
                    {firstRunAdditionalModulesHideLabel}
                  </Button>
                </Stack>
                {renderAdditionalModuleCardsGrid({
                  cards: consoleCards,
                  isFetching: consoleQuery.isFetching,
                  isPending: consoleQuery.isPending,
                  id: 'admin-additional-modules-list',
                  hideSingleCardHeader: true,
                })}
              </Stack>
            )}
          </Stack>
        </Alert>
      )}

      {showConsoleError && (
        <Alert severity="warning">
          No se pudo cargar el panel dinámico. Mostrando la consola base. Detalle: {consoleError}
        </Alert>
      )}

      <Box sx={{ maxWidth: 480 }}>
        <Card variant="outlined" id="admin-service-health">
          <CardHeader title="Estado del servicio" />
          <CardContent>
            {shouldShowHealthLoadingState ? (
              <Stack direction="row" alignItems="center" spacing={1}>
                <CircularProgress size={18} />
                <Typography variant="body2" color="text.secondary">
                  Comprobando API y base de datos…
                </Typography>
              </Stack>
            ) : showCompactHealthyServiceSummary ? (
              <Typography variant="body2">
                Todo listo: API y base de datos responden correctamente.
              </Typography>
            ) : healthQuery.data ? (
              <Stack spacing={1.25}>
                {serviceHealthWarningMessage && (
                  <Typography variant="body2">
                    {serviceHealthWarningMessage}
                  </Typography>
                )}
                <Stack direction="row" spacing={1} useFlexGap flexWrap="wrap">
                  {serviceHealthStatusChips.map((chip) => (
                    <Chip
                      key={chip.key}
                      data-testid="admin-service-health-chip"
                      label={chip.label}
                      title={chip.title}
                      color={getHealthStatusChipColor(chip.value)}
                      size="small"
                      variant="filled"
                    />
                  ))}
                </Stack>
              </Stack>
            ) : null}
            {healthQuery.isError && (
              <Alert severity="error" sx={{ mt: 2 }}>
                {(healthQuery.error as Error).message}
              </Alert>
            )}
          </CardContent>
        </Card>
      </Box>

      {!showFirstRunServiceHealthGate && (
        <>
          <Paper variant="outlined" id="admin-users-and-roles">
            <Box sx={{ px: 2, py: 1 }}>
              <Box>
                <Typography variant="h6">Usuarios y roles</Typography>
                {usersSectionDescription && (
                  <Typography
                    variant="body2"
                    color="text.secondary"
                  >
                    {usersSectionDescription}
                  </Typography>
                )}
              </Box>
            </Box>
            {usersError && (
              <Alert severity="error" sx={{ mx: 2 }}>
                {usersError}
              </Alert>
            )}
            {isUsersLoading ? (
              renderSectionLoading('Cargando usuarios…')
            ) : showUsersTable ? (
              <>
                <TableContainer id="admin-users-table">
                  <Table size="small">
                    <TableHead>
                      <TableRow>
                        <TableCell>Usuario</TableCell>
                        <TableCell>{ADMIN_USER_ROLES_COLUMN_HEADER}</TableCell>
                        {showUsersLastAccessColumn && <TableCell>Último acceso</TableCell>}
                        {showUsersStatusColumn && <TableCell>Estado</TableCell>}
                      </TableRow>
                    </TableHead>
                    <TableBody>
                      {visibleAdminUsers.map((user, index) => {
                        const identity = summarizeAdminUserIdentity(user);
                        const editRoleLabel = buildAdminUserRoleActionName(user);
                        const editRoleTitle = buildAdminUserRoleButtonTitle(user);
                        const shouldShowPartyId = user.partyId != null && userIdsRequiringPartyId.has(user.userId);
                        return (
                          <TableRow key={user.userId} hover>
                            <TableCell>
                              <Stack spacing={0.25}>
                                <Typography variant="body2" fontWeight={600}>
                                  {identity.primary}
                                </Typography>
                                {identity.showUsername && (
                                  <Typography variant="caption" color="text.secondary">
                                    Usuario: {identity.username}
                                  </Typography>
                                )}
                                {shouldShowPartyId ? (
                                  <Typography variant="caption" color="text.secondary">
                                    Party #{user.partyId}
                                  </Typography>
                                ) : null}
                              </Stack>
                            </TableCell>
                            <TableCell>
                              <Stack spacing={0.5} alignItems="flex-start">
                                <Typography
                                  variant="body2"
                                  color="text.secondary"
                                  title={formatEditableRoleList(user.roles)}
                                >
                                  {formatInlineAdminUserRoleSummary(user.roles)}
                                </Typography>
                                {canEditAdminRoles && (
                                  <Button
                                    size="small"
                                    onClick={() => setEditingUser(user)}
                                    aria-label={editRoleLabel}
                                    title={editRoleTitle}
                                    sx={{
                                      px: 0,
                                      minWidth: 0,
                                      justifyContent: 'flex-start',
                                      textTransform: 'none',
                                    }}
                                  >
                                    {buildCompactAdminUserRoleActionLabel(user.roles, {
                                      showFullLabel: index === 0,
                                    })}
                                  </Button>
                                )}
                              </Stack>
                            </TableCell>
                            {showUsersLastAccessColumn && <TableCell>{formatDateOrDash(getAdminUserLastAccess(user))}</TableCell>}
                            {showUsersStatusColumn && <TableCell>{renderStatus(user.status, { hideActive: true })}</TableCell>}
                          </TableRow>
                        );
                      })}
                    </TableBody>
                  </Table>
                </TableContainer>
                {showAdminUsersOverflowAction && (
                  <Box sx={{ px: 2, pb: 2, pt: 1 }}>
                    <Button
                      size="small"
                      variant="text"
                      onClick={() => setShowAllAdminUsers((current) => !current)}
                      aria-controls="admin-users-table"
                      aria-expanded={showAllAdminUsers}
                    >
                      {adminUsersOverflowActionLabel}
                    </Button>
                  </Box>
                )}
              </>
            ) : singleAdminUser && !usersError ? (
              <Box sx={{ px: 2, pb: 2 }}>
                <Stack
                  spacing={1}
                  direction={{ xs: 'column', md: 'row' }}
                  justifyContent="space-between"
                  alignItems={{ xs: 'flex-start', md: 'center' }}
                  sx={{
                    border: '1px solid',
                    borderColor: 'divider',
                    borderRadius: 2,
                    px: 1.5,
                    py: 1.25,
                  }}
                >
                  <Stack spacing={0.25}>
                    <Typography variant="body2" fontWeight={600}>
                      {singleAdminUserIdentity?.primary}
                    </Typography>
                    {singleAdminUserIdentity?.showUsername && (
                      <Typography variant="caption" color="text.secondary">
                        Usuario: {singleAdminUserIdentity.username}
                      </Typography>
                    )}
                    {singleAdminUser.partyId != null && userIdsRequiringPartyId.has(singleAdminUser.userId) ? (
                      <Typography variant="caption" color="text.secondary">
                        Party #{singleAdminUser.partyId}
                      </Typography>
                    ) : null}
                  </Stack>
                  <Stack spacing={0.5} alignItems={{ xs: 'flex-start', md: 'flex-end' }}>
                    <Typography
                      variant="body2"
                      color="text.secondary"
                      title={formatEditableRoleList(singleAdminUser.roles)}
                    >
                      Roles: {formatInlineAdminUserRoleSummary(singleAdminUser.roles)}
                    </Typography>
                    {canEditAdminRoles && (
                      <Button
                        size="small"
                        onClick={() => setEditingUser(singleAdminUser)}
                        aria-label={buildAdminUserRoleActionName(singleAdminUser)}
                        title={buildAdminUserRoleButtonTitle(singleAdminUser)}
                        sx={{
                          px: 0,
                          minWidth: 0,
                          textTransform: 'none',
                        }}
                      >
                        {buildAdminUserRoleActionLabel(singleAdminUser.roles)}
                      </Button>
                    )}
                    {singleAdminUserHasLastAccessTimestamp && singleAdminUserLastAccess && (
                      <Typography variant="body2" color="text.secondary">
                        Último acceso: {formatDateOrDash(singleAdminUserLastAccess)}
                      </Typography>
                    )}
                    {singleAdminUserStatusLabel && (
                      <Typography variant="body2" color="text.secondary">
                        Estado: {singleAdminUserStatusLabel}
                      </Typography>
                    )}
                  </Stack>
                </Stack>
              </Box>
            ) : !usersError ? (
              showGettingStartedGuidance
                ? renderFirstRunSectionStatus(FIRST_RUN_USERS_EMPTY_STATE, 'admin-first-run-users-status')
                : (
                  <Box sx={{ px: 2, pb: 2 }}>
                    <Typography variant="body2" color="text.secondary">
                      Todavía no hay usuarios administrables. Cuando exista el primero, aquí verás roles, último acceso y el atajo para editar roles.
                    </Typography>
                  </Box>
                )
            ) : null}
          </Paper>

          <Paper variant="outlined" id="admin-recent-audit">
            <Box sx={{ px: 2, py: 1 }}>
              <Typography variant="h6">Auditoría reciente</Typography>
              {auditSectionDescription && (
                <Typography variant="body2" color="text.secondary">
                  {auditSectionDescription}
                </Typography>
              )}
            </Box>
            {auditQuery.isError && (
              <Alert severity="error" sx={{ mx: 2 }}>
                {(auditQuery.error as Error).message}
              </Alert>
            )}
            {auditQuery.isLoading ? (
              renderSectionLoading('Cargando auditoría…')
            ) : showAuditTable ? (
              <>
                <TableContainer id="admin-recent-audit-table">
                  <Table size="small">
                    <TableHead>
                      <TableRow>
                        {showAuditDateColumn && <TableCell>Fecha</TableCell>}
                        <TableCell>Entidad</TableCell>
                        <TableCell>Acción</TableCell>
                        {showAuditActorColumn && <TableCell>Actor</TableCell>}
                        {showAuditDetailColumn && <TableCell>Detalle</TableCell>}
                      </TableRow>
                    </TableHead>
                    <TableBody>
                      {visibleAuditEntries.map((entry: AuditLogEntry, index: number) => (
                        <TableRow key={`${entry.entity}-${entry.entityId}-${index}`}>
                          {showAuditDateColumn && <TableCell>{formatDate(entry.createdAt)}</TableCell>}
                          <TableCell>
                            <Typography variant="body2" title={getAuditEntityReferenceTitle(entry)}>
                              {formatAuditEntityReference(entry)}
                            </Typography>
                          </TableCell>
                          <TableCell>
                            <Typography variant="body2" title={getAuditActionTitle(entry.action)}>
                              {formatAuditAction(entry.action)}
                            </Typography>
                          </TableCell>
                          {showAuditActorColumn && <TableCell>{formatAuditActor(entry.actorId, usersById)}</TableCell>}
                          {showAuditDetailColumn && (
                            <TableCell>
                              <Typography
                                variant="body2"
                                color="text.secondary"
                                title={getAuditDetailPreviewTitle(entry.diff)}
                              >
                                {formatAuditDetailPreview(entry.diff)}
                              </Typography>
                            </TableCell>
                          )}
                        </TableRow>
                      ))}
                    </TableBody>
                  </Table>
                </TableContainer>
                {showAuditOverflowAction && (
                  <Box sx={{ px: 2, pb: 2, pt: 1 }}>
                    <Button
                      size="small"
                      variant="text"
                      onClick={() => setShowAllAuditEntries((current) => !current)}
                      aria-controls="admin-recent-audit-table"
                      aria-expanded={showAllAuditEntries}
                    >
                      {auditOverflowActionLabel}
                    </Button>
                  </Box>
                )}
              </>
            ) : singleAuditEntry && !auditQuery.isError ? (
              <Box sx={{ px: 2, pb: 2 }}>
                <Stack spacing={1.25}>
                  <Typography variant="body2" color="text.secondary">
                    Primer evento de auditoría. Revísalo aquí; cuando exista el segundo, volverá la tabla cronológica.
                  </Typography>
                  <Stack
                    spacing={0.75}
                    sx={{
                      border: '1px solid',
                      borderColor: 'divider',
                      borderRadius: 2,
                      px: 1.5,
                      py: 1.25,
                    }}
                  >
                    <Typography variant="body2" title={getAuditActionTitle(singleAuditEntry.action)}>
                      <Box component="span" sx={{ fontWeight: 600 }}>Acción:</Box>{' '}
                      {formatAuditAction(singleAuditEntry.action)}
                    </Typography>
                    {singleAuditHasTimestamp && (
                      <Typography variant="body2" color="text.secondary">
                        <Box component="span" sx={{ fontWeight: 600 }}>Fecha:</Box> {formatDate(singleAuditEntry.createdAt)}
                      </Typography>
                    )}
                    <Typography
                      variant="body2"
                      color="text.secondary"
                      title={getAuditEntityReferenceTitle(singleAuditEntry)}
                    >
                      <Box component="span" sx={{ fontWeight: 600 }}>Entidad:</Box> {formatAuditEntityReference(singleAuditEntry)}
                    </Typography>
                    {singleAuditHasActor && (
                      <Typography variant="body2" color="text.secondary">
                        <Box component="span" sx={{ fontWeight: 600 }}>Actor:</Box> {formatAuditActor(singleAuditEntry.actorId, usersById)}
                      </Typography>
                    )}
                    {singleAuditHasDetail && (
                      <Typography
                        variant="body2"
                        color="text.secondary"
                        title={getAuditDetailPreviewTitle(singleAuditEntry.diff)}
                        sx={{ whiteSpace: 'pre-wrap' }}
                      >
                        <Box component="span" sx={{ fontWeight: 600 }}>Detalle:</Box>{' '}
                        {formatAuditDetailPreview(singleAuditEntry.diff)}
                      </Typography>
                    )}
                  </Stack>
                </Stack>
              </Box>
            ) : !auditQuery.isError ? (
              showGettingStartedGuidance
                ? renderFirstRunSectionStatus(FIRST_RUN_AUDIT_EMPTY_STATE, 'admin-first-run-audit-status')
                : (
                  <Box sx={{ px: 2, pb: 2 }}>
                    <Typography variant="body2" color="text.secondary">
                      Todavía no hay eventos de auditoría. Cuando alguien cambie permisos o datos del sistema, aquí verás quién hizo qué y cuándo.
                    </Typography>
                  </Box>
                )
            ) : null}
          </Paper>
        </>
      )}

      {showStandaloneAdditionalModulesSection && (
        <Paper variant="outlined" id="admin-additional-modules">
          <Box sx={{ px: 2, py: 1 }}>
            <Stack
              direction={{ xs: 'column', sm: 'row' }}
              spacing={1}
              justifyContent="space-between"
              alignItems={{ xs: 'flex-start', sm: 'center' }}
            >
              <Box>
                <Stack direction="row" spacing={1} alignItems="center" useFlexGap flexWrap="wrap">
                  <Typography variant="h6">{standaloneAdditionalModulesTitle}</Typography>
                  {showStandaloneAdditionalModulesOptionalChip && (
                    <Chip label="Opcional" size="small" variant="outlined" />
                  )}
                </Stack>
                {!showStandaloneAdditionalModulesOptionalChip && (
                  <Typography variant="body2" color="text.secondary">
                    {standaloneAdditionalModulesDescription}
                  </Typography>
                )}
              </Box>
              <Button
                size="small"
                variant="text"
                onClick={() => setShowStandaloneAdditionalModules((current) => !current)}
                aria-controls="admin-additional-modules-list"
                aria-expanded={showStandaloneAdditionalModules}
                aria-label={standaloneAdditionalModulesActionCopy.ariaLabel}
                title={showStandaloneAdditionalModules ? undefined : standaloneAdditionalModulesActionCopy.title}
              >
                {showStandaloneAdditionalModules
                  ? standaloneAdditionalModulesHideLabel
                  : standaloneAdditionalModulesActionCopy.label}
              </Button>
            </Stack>
          </Box>
          {showStandaloneAdditionalModules && renderAdditionalModuleCardsGrid({
            cards: consoleCards,
            isFetching: consoleQuery.isFetching,
            isPending: consoleQuery.isPending,
            id: 'admin-additional-modules-list',
            sx: { px: 2, pb: 2 },
            hideSingleCardHeader: true,
          })}
        </Paper>
      )}

      <Dialog open={!!editingUser} onClose={handleCloseDialog} fullWidth maxWidth="sm">
        <DialogTitle>{editingActionLabel} · {editingTitle}</DialogTitle>
        <DialogContent>
          <Typography variant="body2" color="text.secondary" paragraph>
            Roles actuales: {currentRoleSummary}. Ajusta la selección para abrir o retirar módulos en esta cuenta.
          </Typography>
          {dialogError && (
            <Alert severity="error" sx={{ mb: 2 }}>
              {dialogError}
            </Alert>
          )}
          <FormControl fullWidth>
            <InputLabel id="admin-user-roles-label" shrink>Roles asignados</InputLabel>
            <Select
              labelId="admin-user-roles-label"
              label="Roles asignados"
              multiple
              displayEmpty
              value={selectedRoles}
              onChange={(event) => setSelectedRoles(normalizeRoleList(event.target.value as RoleKey[]))}
              renderValue={() => selectedRoleControlSummary}
              SelectDisplayProps={{
                title: selectedRoleControlTitle,
              }}
            >
              {roleOptionsForEditor.map((option) => (
                <MenuItem key={option.value} value={option.value}>
                  <Checkbox checked={selectedRoles.includes(option.value)} />
                  <ListItemText primary={option.label} />
                </MenuItem>
              ))}
            </Select>
            <FormHelperText>
              {roleSelectionHelperText}
            </FormHelperText>
          </FormControl>
        </DialogContent>
        <DialogActions>
          <Button onClick={handleCloseDialog} disabled={updateRolesMutation.isPending}>
            {hasPendingRoleChanges ? 'Descartar cambios' : 'Cerrar'}
          </Button>
          {hasPendingRoleChanges && (
            <Button
              onClick={handleSaveRoles}
              variant="contained"
              disabled={updateRolesMutation.isPending}
            >
              {updateRolesMutation.isPending ? 'Guardando…' : 'Guardar cambios'}
            </Button>
          )}
        </DialogActions>
      </Dialog>
    </Stack>
  );
}
