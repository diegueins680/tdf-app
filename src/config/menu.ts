export type Role =
  | 'admin' | 'finanzas' | 'booker' | 'ingeniero' | 'productor'
  | 'artista' | 'profesor' | 'estudiante' | 'promotor';

export const topLevel = [
  "Inicio",
  "CRM",
  "Estudio",
  "Label",
  "Eventos",
  "Escuela",
  "Finanzas",
  "Bar",
  "Operación",
  "Configuración",
  "Insights"
] as const;

export const submenus: Record<string, string[]> = {
  "CRM": ["Contactos","Empresas","Leads"],
  "Estudio": ["Calendario","Salas y recursos","Órdenes","Pipelines","Reportes"],
  "Label": ["Artistas","Proyectos","Releases","Tracks","Assets","Metadata","Contratos","Regalías","Marketing"],
  "Eventos": ["Agenda","Fechas y tours","Venues","Staff","Presupuestos","Post-mortem"],
  "Escuela": ["Profesores","Clases","Trial Lessons","Trial Queue","Programas","Cursos","Cohortes","Estudiantes","Inscripciones","Pagos"],
  "Finanzas": ["Cotizaciones","Facturas","Cobros","Regalías"],
  "Bar": ["Punto de venta","Caja","Inventario","Staff"],
  "Operación": ["Calendario DOMO","Inventario","Reservas de equipo","Mantenimiento","Paquetes"],
  "Configuración": ["Roles y permisos","Impuestos y series","Unidades de negocio","Sedes","Marcas","Integraciones","Preferencias"],
  "Insights": []
};

export const visibilityByRole: Record<Role, (string | "*")[]> = {
  admin: ["*"],
  finanzas: ["Finanzas","Label.Regalías","Estudio.Órdenes","Estudio.Reportes","Eventos.Presupuestos","CRM","Insights","Bar"],
  booker: ["Estudio","CRM","Operación.Calendario DOMO","Operación.Reservas de equipo","Operación.Paquetes","Finanzas.Cotizaciones","Bar"],
  ingeniero: ["Estudio","Operación","CRM","Label"],
  productor: ["Estudio","Label","Eventos","CRM","Bar"],
  artista: ["Label.Contratos","Label.Releases","Label.Tracks","Label.Assets","Label.Regalías","Estudio.Calendario","Eventos.Fechas y tours"],
  profesor: ["Escuela","Estudio.Calendario","CRM"],
  estudiante: ["Escuela.Cursos","Escuela.Inscripciones","Escuela.Pagos"],
  promotor: ["Eventos","CRM","Label.Artistas","Operación.Calendario DOMO","Operación.Reservas de equipo","Bar"]
};

export const quickCreate: Record<Role, string[]> = {
  admin: ["Contacto","Booking","Evento","Curso","Factura","Cotización","Equipo"],
  finanzas: ["Cotización","Factura","Cobro","Liquidación de regalías","Serie de facturación"],
  booker: ["Booking","Orden de servicio","Bloqueo de sala","Paquete","Contacto"],
  ingeniero: ["Parte de mantenimiento","Checklist de sesión","Solicitud de equipo","Nota técnica"],
  productor: ["Proyecto","Release","Reserva de sala","Asignación de staff","Solicitud de assets"],
  artista: ["Solicitud de sesión","Cargar assets","Actualizar datos bancarios"],
  profesor: ["Clase de curso","Evaluación","Material didáctico"],
  estudiante: ["Inscripción","Pago","Ticket de soporte"],
  promotor: ["Evento","Fecha de tour","Venue","Staff"]
};

const ROLE_ALIASES: Record<string, Role> = {
  admin: 'admin',
  Admin: 'admin',
  manager: 'admin',
  Manager: 'admin',
  finanzas: 'finanzas',
  Finance: 'finanzas',
  Accounting: 'finanzas',
  accounting: 'finanzas',
  booker: 'booker',
  Booker: 'booker',
  reception: 'booker',
  Reception: 'booker',
  ingeniero: 'ingeniero',
  Engineer: 'ingeniero',
  engineer: 'ingeniero',
  productor: 'productor',
  Producer: 'productor',
  producer: 'productor',
  artista: 'artista',
  Artist: 'artista',
  artist: 'artista',
  profesor: 'profesor',
  Teacher: 'profesor',
  teacher: 'profesor',
  estudiante: 'estudiante',
  Student: 'estudiante',
  student: 'estudiante',
  promotor: 'promotor',
  Promoter: 'promotor',
  promoter: 'promotor',
  vendor: 'promotor',
  Vendor: 'promotor',
  readonly: 'booker',
  ReadOnly: 'booker',
  customer: 'artista',
  Customer: 'artista'
};

export function normalizeRoles(rawRoles: readonly string[] | undefined): Role[] {
  if (!rawRoles || rawRoles.length === 0) {
    return ['admin'];
  }
  const normalized = rawRoles
    .map(role => ROLE_ALIASES[role] ?? ROLE_ALIASES[role.toLowerCase()])
    .filter((value): value is Role => Boolean(value));

  return normalized.length > 0 ? normalized : ['admin'];
}
