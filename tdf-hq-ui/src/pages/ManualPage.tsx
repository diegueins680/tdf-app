import { useMemo } from 'react';
import { Box, Card, CardContent, Chip, Link, Stack, Typography } from '@mui/material';
import LaunchIcon from '@mui/icons-material/Launch';
import { useSession } from '../session/SessionContext';

interface ManualItem {
  title: string;
  path: string;
  description: string;
}

const sectionsByModule: Record<string, ManualItem[]> = {
  crm: [
    { title: 'CRM / Contactos', path: '/crm/contactos', description: 'Agenda, filtra y edita contactos.' },
    { title: 'CRM / Empresas', path: '/crm/empresas', description: 'Gestiona empresas y cuentas vinculadas.' },
    { title: 'CRM / Leads', path: '/crm/leads', description: 'Flujo de leads y seguimiento.' },
  ],
  scheduling: [
    { title: 'Estudio / Calendario', path: '/estudio/calendario', description: 'Agenda de salas y servicios.' },
    { title: 'Estudio / Salas y recursos', path: '/estudio/salas', description: 'Disponibilidad y configuración de recursos.' },
    { title: 'Estudio / Órdenes', path: '/estudio/ordenes', description: 'Órdenes de servicio y estado.' },
    { title: 'Estudio / Servicios', path: '/estudio/servicios', description: 'Catálogo de servicios ofrecidos.' },
    { title: 'Estudio / Pipelines', path: '/estudio/pipelines', description: 'Kanban de proyectos/servicios.' },
    { title: 'Estudio / Reportes', path: '/estudio/reportes', description: 'KPIs rápidos de reservas, clases y cobros.' },
    { title: 'Estudio / Live Sessions', path: '/estudio/live-sessions', description: 'Intake y agenda de sesiones en vivo.' },
    { title: 'Escuela / Clases', path: '/escuela/clases', description: 'Clases regulares: horarios, salas y asistencia.' },
    { title: 'Escuela / Trial lessons', path: '/escuela/trial-lessons', description: 'Clases de prueba: programación y edición.' },
    { title: 'Escuela / Trial queue', path: '/escuela/trial-queue', description: 'Bandeja de solicitudes de prueba.' },
    { title: 'Operación / Calendario domo', path: '/operacion/calendario-domo', description: 'Agenda de domo/espacios especiales.' },
  ],
  invoicing: [
    { title: 'Finanzas / Cobros', path: '/finanzas/cobros', description: 'Seguimiento de cobros y estados.' },
    { title: 'Finanzas / Pagos', path: '/finanzas/pagos', description: 'Registro de pagos y comprobantes.' },
    { title: 'Finanzas / Cotizaciones', path: '/finanzas/cotizaciones', description: 'Cotizaciones emitidas.' },
    { title: 'Finanzas / Facturas', path: '/finanzas/facturas', description: 'Facturación y estados.' },
    { title: 'Finanzas / Recibos', path: '/finanzas/recibos', description: 'Recibos generados.' },
    { title: 'Finanzas / Regalías', path: '/finanzas/regalias', description: 'Cálculo y control de regalías.' },
  ],
  admin: [
    { title: 'Configuración / Usuarios admin', path: '/configuracion/usuarios-admin', description: 'Altas y bajas de administradores.' },
    { title: 'Configuración / Roles y permisos', path: '/configuracion/roles-permisos', description: 'Asignación de permisos y módulos.' },
    { title: 'Configuración / Logs', path: '/configuracion/logs', description: 'Registros de sistema.' },
    { title: 'Configuración / Estado', path: '/configuracion/estado', description: 'Salud de integraciones y servicios.' },
    { title: 'Configuración / Integraciones', path: '/configuracion/integraciones', description: 'Claves y conexiones externas.' },
    { title: 'Configuración / CMS', path: '/configuracion/cms', description: 'Contenido público (records, fan hub, cursos).' },
  ],
  packages: [
    { title: 'Label / Artistas', path: '/label/artistas', description: 'Catálogo de artistas (label).' },
    { title: 'Operación / Paquetes', path: '/operacion/paquetes', description: 'Paquetes y combinaciones de servicios.' },
    { title: 'Bar / Inventory', path: '/bar/inventory', description: 'Inventario de bar / punto de venta.' },
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
