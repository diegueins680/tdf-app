import { useMemo } from 'react';
import { Alert, Box, Card, CardContent, Chip, Link, Stack, Typography } from '@mui/material';
import LaunchIcon from '@mui/icons-material/Launch';
import { useSession } from '../session/SessionContext';

interface ManualItem {
  title: string;
  path: string;
  description: string;
}

const sectionsByModule: Record<string, ManualItem[]> = {
  crm: [
    { title: 'CRM / Contactos', path: '/crm/contactos', description: 'Agenda, filtra y edita contactos. Usa las vistas de lista para filtrar por rol, etiquetas o estado. Los detalles del contacto incluyen datos de contacto, canales sociales, notas y relación con empresas.' },
    { title: 'CRM / Empresas', path: '/crm/empresas', description: 'Gestiona empresas y cuentas vinculadas. Cada empresa agrupa contactos relacionados y permite notas o acuerdos asociados.' },
    { title: 'CRM / Leads', path: '/crm/leads', description: 'Flujo de leads y seguimiento. Registra origen, responsable y notas. Pasa leads a contactos cuando se convierten.' },
  ],
  scheduling: [
    { title: 'Estudio / Calendario', path: '/estudio/calendario', description: 'Agenda de salas y servicios. Crea, edita o mueve reservas; asigna recurso (sala) y cliente; controla estados (confirmada, tentativa, cancelada).' },
    { title: 'Estudio / Salas y recursos', path: '/estudio/salas', description: 'Disponibilidad y configuración de recursos. Define si son reservables, nombres y preferencias por materia.' },
    { title: 'Estudio / Órdenes', path: '/estudio/ordenes', description: 'Órdenes de servicio y estado. Sigue el avance de trabajos vinculados a reservas o proyectos.' },
    { title: 'Estudio / Servicios', path: '/estudio/servicios', description: 'Catálogo de servicios ofrecidos. Ajusta precios, duraciones y tipos.' },
    { title: 'Estudio / Pipelines', path: '/estudio/pipelines', description: 'Kanban de proyectos/servicios. Arrastra tarjetas, cambia etapas y agrega notas rápidas.' },
    { title: 'Estudio / Reportes', path: '/estudio/reportes', description: 'KPIs rápidos de reservas, clases y cobros: ingresos últimos 30 días, ingresos del día, reservas y clases próximas.' },
    { title: 'Estudio / Live Sessions', path: '/estudio/live-sessions', description: 'Intake y agenda de sesiones en vivo. Registra banda/artista, fecha y necesidades técnicas.' },
    { title: 'Escuela / Clases', path: '/escuela/clases', description: 'Clases regulares: horarios, salas, profesor y asistencia. Permite filtrar, crear/editar y marcar “realizada”.' },
    { title: 'Escuela / Trial lessons', path: '/escuela/trial-lessons', description: 'Clases de prueba: programación y edición. Carga alumno, profesor, sala y notas.' },
    { title: 'Escuela / Trial queue', path: '/escuela/trial-queue', description: 'Bandeja de solicitudes de prueba. Asigna profesor, agenda y cambia estado.' },
    { title: 'Operación / Calendario domo', path: '/operacion/calendario-domo', description: 'Agenda de domo/espacios especiales para logística interna.' },
  ],
  invoicing: [
    { title: 'Finanzas / Cobros', path: '/finanzas/cobros', description: 'Seguimiento de cobros y estados. Consulta pagos pendientes y conciliación.' },
    { title: 'Finanzas / Pagos', path: '/finanzas/pagos', description: 'Registro de pagos y comprobantes. Soporta concepto, monto, referencia y adjuntos.' },
    { title: 'Finanzas / Cotizaciones', path: '/finanzas/cotizaciones', description: 'Cotizaciones emitidas y su vigencia. Convierte cotizaciones a órdenes/facturas.' },
    { title: 'Finanzas / Facturas', path: '/finanzas/facturas', description: 'Facturación y estados. Consulta facturas asociadas a clientes o órdenes.' },
    { title: 'Finanzas / Recibos', path: '/finanzas/recibos', description: 'Recibos generados; descarga o reenvía al cliente.' },
    { title: 'Finanzas / Regalías', path: '/finanzas/regalias', description: 'Cálculo y control de regalías por artista/proyecto.' },
  ],
  admin: [
    { title: 'Configuración / Usuarios admin', path: '/configuracion/usuarios-admin', description: 'Altas, bajas y reseteo de accesos administrativos.' },
    { title: 'Configuración / Roles y permisos', path: '/configuracion/roles-permisos', description: 'Asignación granular de permisos y módulos (CRM, scheduling, invoicing, etc.).' },
    { title: 'Configuración / Logs', path: '/configuracion/logs', description: 'Registros de sistema para auditar cambios y errores.' },
    { title: 'Configuración / Estado', path: '/configuracion/estado', description: 'Salud de integraciones y servicios (API, correo, pagos).' },
    { title: 'Configuración / Integraciones', path: '/configuracion/integraciones', description: 'Claves y conexiones externas (API tokens, webhooks).' },
    { title: 'Configuración / CMS', path: '/configuracion/cms', description: 'Contenido público (records, fan hub, cursos). Crea, publica y versiona bloques.' },
  ],
  packages: [
    { title: 'Label / Artistas', path: '/label/artistas', description: 'Catálogo de artistas (label). Datos biográficos, lanzamientos y enlaces.' },
    { title: 'Operación / Paquetes', path: '/operacion/paquetes', description: 'Paquetes y combinaciones de servicios (incluye resumen y pricing).' },
  ],
};

export default function ManualPage() {
  const { session } = useSession();
  const modules = useMemo(() => (session?.modules ?? []).map((m) => m.toLowerCase()), [session?.modules]);

  const items = useMemo(() => {
    const merged: ManualItem[] = [];
    modules.forEach((m) => {
      const entries = sectionsByModule[m];
      if (entries) merged.push(...entries);
    });
    return merged;
  }, [modules]);

  return (
    <Stack spacing={3}>
      <Stack spacing={0.5}>
        <Typography variant="overline" color="text.secondary">Recursos</Typography>
        <Typography variant="h4" fontWeight={800}>Manual de usuario</Typography>
        <Typography color="text.secondary">
          Accesos y flujos según tus módulos habilitados.
        </Typography>
        <Chip label={`${items.length} secciones`} size="small" />
      </Stack>

      {items.length === 0 && (
        <Alert severity="info">Tu usuario no tiene módulos asignados o la sesión expiró.</Alert>
      )}

      <Stack spacing={2}>
        {items.map((item) => (
          <Card key={item.path} variant="outlined">
            <CardContent>
              <Stack direction={{ xs: 'column', sm: 'row' }} justifyContent="space-between" spacing={1}>
                <Box>
                  <Typography variant="h6" fontWeight={800}>{item.title}</Typography>
                  <Typography color="text.secondary">{item.description}</Typography>
                  <Typography color="text.secondary" variant="body2">Ruta: {item.path}</Typography>
                </Box>
                <ButtonLink href={item.path} />
              </Stack>
            </CardContent>
          </Card>
        ))}
      </Stack>
    </Stack>
  );
}

function ButtonLink({ href }: { href: string }) {
  return (
    <Link href={href} underline="hover" target="_self" sx={{ alignSelf: 'flex-start', display: 'inline-flex', alignItems: 'center', gap: 0.5 }}>
      Abrir <LaunchIcon fontSize="small" />
    </Link>
  );
}
