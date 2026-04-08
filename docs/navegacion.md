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
