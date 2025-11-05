#!/usr/bin/env bash
set -euo pipefail

echo "==> Instalando rutas y menú en TDF-ui (add-only)"
ROOT="$(pwd)"
mkdir -p src/routes src/config public docs

cat > src/routes/AppRoutes.tsx <<'TSX'
import React from 'react';
import { Routes, Route, Navigate, Outlet } from 'react-router-dom';

export type Role =
  | 'admin' | 'finanzas' | 'booker' | 'ingeniero' | 'productor'
  | 'artista' | 'profesor' | 'estudiante' | 'promotor';

export interface User { id: string; roles: Role[]; }

function useCurrentUser(): User | null {
  // TODO: Reemplazar por tu hook real (auth).
  return { id: 'demo', roles: ['admin'] };
}

function RequireRole(props: { allowed: '*' | Role[]; children: React.ReactNode }) {
  const { allowed, children } = props;
  const user = useCurrentUser();
  if (!user) return <Navigate to="/inicio" replace />;
  if (allowed === '*') return <>{children}</>;
  if (user.roles.some(r => (allowed as Role[]).includes(r))) return <>{children}</>;
  return <Navigate to="/inicio" replace />;
}

function Layout() {
  return (
    <div className="app-layout">
      <header className="app-header" style={{ display: 'flex', gap: 12, alignItems: 'center', padding: 12 }}>
        <strong>TDF</strong>
        <select aria-label="Unidad" style={{ marginLeft: 'auto' }}>
          <option>Unidad A</option>
          <option>Unidad B</option>
        </select>
      </header>
      <main className="app-main">
        <Outlet />
      </main>
    </div>
  );
}

function Page(props: { title: string }) {
  return <div style={{ padding: 24 }}><h1>{props.title}</h1><p>Placeholder</p></div>;
}

export default function AppRoutes() {
  return (
    <Routes>
      {/* Aliases */}
      <Route path="/parties" element={<Navigate to="/crm/contactos" replace />} />
      <Route path="/bookings" element={<Navigate to="/estudio/calendario" replace />} />
      <Route path="/pipelines" element={<Navigate to="/estudio/pipelines" replace />} />
      <Route path="/contactos" element={<Navigate to="/crm/contactos" replace />} />

      <Route element={<Layout />}>
        <Route path="/inicio" element={<Page title="Inicio" />} />

        <Route
          path="/crm"
          element={<RequireRole allowed={['admin','finanzas','booker','ingeniero','productor','profesor','promotor']}><Outlet /></RequireRole>}
        >
          <Route path="contactos" element={<Page title="CRM / Contactos" />} />
          <Route path="empresas" element={<Page title="CRM / Empresas" />} />
          <Route path="leads" element={<Page title="CRM / Leads" />} />
          <Route index element={<Navigate to="contactos" replace />} />
        </Route>

        <Route
          path="/estudio"
          element={<RequireRole allowed={['admin','booker','ingeniero','productor','finanzas']}><Outlet /></RequireRole>}
        >
          <Route path="calendario" element={<Page title="Estudio / Calendario" />} />
          <Route path="salas" element={<Page title="Estudio / Salas y recursos" />} />
          <Route path="ordenes" element={<Page title="Estudio / Órdenes (OS)" />} />
          <Route path="pipelines" element={<Page title="Estudio / Pipelines (Producción)" />} />
          <Route path="reportes" element={<Page title="Estudio / Reportes" />} />
          <Route index element={<Navigate to="calendario" replace />} />
        </Route>

        <Route
          path="/label"
          element={<RequireRole allowed={['admin','productor','finanzas','artista','ingeniero']}><Outlet /></RequireRole>}
        >
          <Route path="artistas" element={<Page title="Label / Artistas" />} />
          <Route path="proyectos" element={<Page title="Label / Proyectos" />} />
          <Route path="releases" element={<Page title="Label / Releases" />} />
          <Route path="tracks" element={<Page title="Label / Tracks" />} />
          <Route path="assets" element={<Page title="Label / Assets" />} />
          <Route path="contratos" element={<Page title="Label / Contratos" />} />
          <Route path="regalias" element={<Page title="Label / Regalías" />} />
          <Route path="marketing" element={<Page title="Label / Marketing" />} />
          <Route index element={<Navigate to="artistas" replace />} />
        </Route>

        <Route
          path="/eventos"
          element={<RequireRole allowed={['admin','promotor','productor','finanzas']}><Outlet /></RequireRole>}
        >
          <Route path="agenda" element={<Page title="Eventos / Agenda" />} />
          <Route path="fechas-y-tours" element={<Page title="Eventos / Fechas y tours" />} />
          <Route path="venues" element={<Page title="Eventos / Venues" />} />
          <Route path="staff" element={<Page title="Eventos / Staff" />} />
          <Route path="presupuestos" element={<Page title="Eventos / Presupuestos" />} />
          <Route path="post-mortem" element={<Page title="Eventos / Post-mortem" />} />
          <Route index element={<Navigate to="agenda" replace />} />
        </Route>

        <Route
          path="/escuela"
          element={<RequireRole allowed={['admin','profesor','estudiante','finanzas']}><Outlet /></RequireRole>}
        >
          <Route path="programas" element={<Page title="Escuela / Programas" />} />
          <Route path="cursos" element={<Page title="Escuela / Cursos" />} />
          <Route path="cohortes" element={<Page title="Escuela / Cohortes" />} />
          <Route path="estudiantes" element={<Page title="Escuela / Estudiantes" />} />
          <Route path="inscripciones" element={<Page title="Escuela / Inscripciones" />} />
          <Route path="pagos" element={<Page title="Escuela / Pagos" />} />
          <Route index element={<Navigate to="cursos" replace />} />
        </Route>

        <Route
          path="/finanzas"
          element={<RequireRole allowed={['admin','finanzas']}><Outlet /></RequireRole>}
        >
          <Route path="cotizaciones" element={<Page title="Finanzas / Cotizaciones" />} />
          <Route path="facturas" element={<Page title="Finanzas / Facturas" />} />
          <Route path="cobros" element={<Page title="Finanzas / Cobros" />} />
          <Route path="regalias" element={<Page title="Finanzas / Regalías (liquidaciones)" />} />
          <Route index element={<Navigate to="cotizaciones" replace />} />
        </Route>

        <Route
          path="/operacion"
          element={<RequireRole allowed={['admin','booker','ingeniero','productor','promotor']}><Outlet /></RequireRole>}
        >
          <Route path="inventario" element={<Page title="Operación / Inventario" />} />
          <Route path="reservas-equipo" element={<Page title="Operación / Reservas de equipo" />} />
          <Route path="mantenimiento" element={<Page title="Operación / Mantenimiento" />} />
          <Route path="paquetes" element={<Page title="Operación / Paquetes" />} />
          <Route index element={<Navigate to="inventario" replace />} />
        </Route>

        <Route
          path="/configuracion"
          element={<RequireRole allowed={['admin']}><Outlet /></RequireRole>}
        >
          <Route path="roles-permisos" element={<Page title="Configuración / Roles y permisos" />} />
          <Route path="impuestos-series" element={<Page title="Configuración / Impuestos y series" />} />
          <Route path="unidades-negocio" element={<Page title="Configuración / Unidades de negocio" />} />
          <Route path="sedes" element={<Page title="Configuración / Sedes" />} />
          <Route path="marcas" element={<Page title="Configuración / Marcas" />} />
          <Route path="integraciones" element={<Page title="Configuración / Integraciones" />} />
          <Route path="preferencias" element={<Page title="Configuración / Preferencias" />} />
          <Route index element={<Navigate to="roles-permisos" replace />} />
        </Route>

        <Route
          path="/insights"
          element={<RequireRole allowed={['admin','finanzas','booker','ingeniero','productor','profesor','promotor']}><Page title="Insights" /></RequireRole>}
        />
      </Route>

      <Route path="/" element={<Navigate to="/inicio" replace />} />
      <Route path="*" element={<Navigate to="/inicio" replace />} />
    </Routes>
  );
}

TSX

cat > src/config/menu.ts <<'TS'
export type Role =
  | 'admin' | 'finanzas' | 'booker' | 'ingeniero' | 'productor'
  | 'artista' | 'profesor' | 'estudiante' | 'promotor';

export const topLevel = ["Inicio","CRM","Estudio","Label","Eventos","Escuela","Finanzas","Operación"] as const;

export const submenus: Record<string, string[]> = {
  "CRM": ["Contactos","Empresas","Leads"],
  "Estudio": ["Calendario","Salas y recursos","Órdenes","Pipelines","Reportes"],
  "Label": ["Artistas","Proyectos","Releases","Tracks y assets","Contratos","Regalías","Marketing"],
  "Eventos": ["Agenda","Fechas y tours","Venues","Staff","Presupuestos","Post-mortem"],
  "Escuela": ["Programas","Cursos","Cohortes","Estudiantes","Inscripciones","Pagos"],
  "Finanzas": ["Cotizaciones","Facturas","Cobros","Regalías"],
  "Operación": ["Inventario","Reservas de equipo","Mantenimiento","Paquetes"]
};

export const visibilityByRole: Record<Role, (string | "*")[]> = {
  admin: ["*"],
  finanzas: ["Finanzas","Label.Regalías","Estudio.Órdenes","Estudio.Reportes","Eventos.Presupuestos","CRM","Insights"],
  booker: ["Estudio","CRM","Operación.Reservas de equipo","Operación.Paquetes","Finanzas.Cotizaciones"],
  ingeniero: ["Estudio","Operación","CRM","Label"],
  productor: ["Estudio","Label","Eventos","CRM"],
  artista: ["Label.Contratos","Label.Releases","Label.Tracks y assets","Label.Regalías","Estudio.Calendario","Eventos.Fechas y tours"],
  profesor: ["Escuela","Estudio.Calendario","CRM"],
  estudiante: ["Escuela.Cursos","Escuela.Inscripciones","Escuela.Pagos"],
  promotor: ["Eventos","CRM","Label.Artistas","Operación.Reservas de equipo"]
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

TS

cat > docs/navegacion.md <<'MD'
# Navegación & Rutas (TDF-ui)

Este módulo añade un **Router v6** con **máx. 2 niveles**, slugs en **español** y **aliases** desde rutas previas:
- `/parties` → `/crm/contactos`
- `/bookings` → `/estudio/calendario`
- `/pipelines` → `/estudio/pipelines`
- `/contactos` → `/crm/contactos`

## Instrucciones (no destructivas)
1. Copia `src/routes/AppRoutes.tsx` y `src/config/menu.ts` a tu repo.
2. En `src/main.tsx` o `src/App.tsx`, monta el router:
   ```tsx
   import { BrowserRouter } from 'react-router-dom';
   import AppRoutes from './routes/AppRoutes';

   // Dentro del render:
   <BrowserRouter>
     <AppRoutes />
   </BrowserRouter>
   ```
3. Reemplaza los contenidos de tus páginas reales dentro de cada ruta (ahora hay placeholders).
4. Opcional: renderiza el menú a partir de `src/config/menu.ts` y muestra el botón **Crear (+)** según `quickCreate`.

## Roles
Ver `src/config/menu.ts` para la lista de roles, visibilidad y acciones rápidas.

## Móvil
Usa un bottom‑nav con tabs para: Inicio, Estudio, Label, Eventos, Escuela; y un menú **Más** para el resto.

MD

cat > public/sitemap.mmd <<'MMD'
%%{init: {'flowchart': {'curve': 'linear'}} }%%
flowchart TB
  INICIO[Inicio]

  subgraph CRM
    CRM1[Contactos]
    CRM2[Empresas]
    CRM3[Leads]
  end

  subgraph ESTUDIO
    E1[Calendario]
    E2[Salas y recursos]
    E3[Órdenes (OS)]
    E4[Pipelines (Producción)]
    E5[Reportes]
  end

  subgraph LABEL
    L1[Artistas]
    L2[Proyectos]
    L3[Releases]
    L4[Tracks y assets]
    L5[Contratos]
    L6[Regalías]
    L7[Marketing]
  end

  subgraph EVENTOS
    EV1[Agenda]
    EV2[Fechas y tours]
    EV3[Venues]
    EV4[Staff]
    EV5[Presupuestos]
    EV6[Post-mortem]
  end

  subgraph ESCUELA
    ES1[Programas]
    ES2[Cursos]
    ES3[Cohortes]
    ES4[Estudiantes]
    ES5[Inscripciones]
    ES6[Pagos]
  end

  subgraph FINANZAS
    F1[Cotizaciones]
    F2[Facturas]
    F3[Cobros]
    F4[Regalías (liquidaciones)]
  end

  subgraph OPERACION[Operación]
    OP1[Inventario]
    OP2[Reservas de equipo]
    OP3[Mantenimiento]
    OP4[Paquetes]
  end

  INICIO --> CRM
  INICIO --> ESTUDIO
  INICIO --> LABEL
  INICIO --> EVENTOS
  INICIO --> ESCUELA
  INICIO --> FINANZAS
  INICIO --> OPERACION

MMD

echo "OK. Recuerda montar <AppRoutes /> en tu App.tsx o main.tsx."
