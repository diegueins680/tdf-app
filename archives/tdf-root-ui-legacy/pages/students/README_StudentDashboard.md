# 🎓 Student Dashboard - Nueva Funcionalidad

## Descripción

Implementación de un **Panel de Control para Estudiantes** que proporciona una vista consolidada de:
- Paquetes de clases adquiridos
- Clases disponibles y restantes
- Historial de clases completadas y programadas
- Estado de paquetes activos

## Ubicación

**Archivo**: `/tdf-hq-ui/src/pages/students/StudentDashboard.tsx`  
**Ruta**: `/escuela/estudiante/:studentId/dashboard`

## Características Implementadas

### 📊 Dashboard con Estadísticas

Cuatro tarjetas de métricas principales:

1. **Clases Disponibles** - Total de clases restantes en todos los paquetes activos
2. **Paquetes Activos** - Cantidad de paquetes con estado "active"
3. **Clases Completadas** - Historial total de clases finalizadas
4. **Clases Programadas** - Sesiones futuras agendadas

### 📦 Gestión de Paquetes

Tabla detallada mostrando:
- ID del paquete
- Fecha de compra
- Clases restantes (destacado en grande)
- Barra de progreso de uso
- Estado (active/completed/cancelled) con chips de colores

### 📚 Historial de Clases

Tabla con las últimas 10 clases ordenadas por fecha:
- Fecha y hora de la clase
- Tiempo relativo (ej: "Hace 2 días", "Mañana")
- ID del profesor
- Ubicación
- Estado con íconos visuales:
  - ✅ Completado (verde)
  - ⏰ Programado (azul)
  - ❌ Cancelado (rojo)
- Notas de la clase

## Tecnologías Utilizadas

- **React** con TypeScript
- **Material-UI (MUI)** para componentes visuales
- **React Query** para gestión de estado del servidor
- **React Router** para navegación
- API hooks auto-generados desde OpenAPI

## Uso

### Navegación

```typescript
// Desde código
navigate(`/escuela/estudiante/${studentId}/dashboard`);

// URL directa
http://localhost:5173/escuela/estudiante/[UUID]/dashboard
```

### Ejemplo

```
http://localhost:5173/escuela/estudiante/22222222-2222-2222-2222-222222222222/dashboard
```

## API Dependencies

El dashboard consume los siguientes endpoints:

```typescript
// Obtener información del estudiante
GET /api/students/{id}

// Obtener paquetes/enrollments del estudiante
GET /api/enrollments
// Filtrado en cliente por student_id

// Obtener clases del estudiante
GET /api/lessons?student_id={id}

// Obtener detalles de paquetes (para cálculo de progreso)
GET /api/packages

// Obtener información de profesores (para nombres en historial)
GET /api/teachers
```

## Estados y Validación

### Estados Manejados

- ✅ **Loading**: Muestra CircularProgress centrado
- ❌ **Error**: Muestra Alert con mensaje de error
- ⚠️ **Sin datos**: Alerts informativos cuando no hay paquetes o clases
- ✔️ **Éxito**: Visualización completa del dashboard

### Indicadores de Estado

- **Paquete Activo**: Chip verde "active"
- **Paquete Completado**: Chip gris "completed"
- **Paquete Cancelado**: Chip rojo "cancelled"

- **Clase Completada**: Chip verde con ícono CheckCircle
- **Clase Programada**: Chip azul con ícono Schedule
- **Clase Cancelada**: Chip rojo con ícono Cancel

## Mejoras Futuras

### Funcionalidades Pendientes

1. **Detalles mejorados del paquete**:
   - Mostrar fecha de expiración calculada
   - Incluir precio pagado con historial de pagos
   - Indicador visual de paquetes próximos a vencer

2. **Filtros y búsqueda**:
   - Filtrar clases por estado
   - Búsqueda por fecha
   - Filtrar por profesor

3. **Acciones interactivas**:
   - Botón para programar nueva clase
   - Cancelar clase programada
   - Ver materiales de la clase
   - Descargar recibos de pago

4. **Gráficos visuales**:
   - Gráfico de progreso circular para cada paquete
   - Timeline de clases
   - Estadísticas mensuales

5. **Notificaciones**:
   - Alertas de clases próximas a vencer
   - Recordatorios de clases programadas
   - Avisos de paquetes por expirar

6. **Vista móvil mejorada**:
   - Optimización responsive para tablets/móviles
   - Swipe gestures en tablas
   - Cards colapsables

## Integración con Backend

### Datos Requeridos

Para funcionalidad completa, el backend debe retornar:

```typescript
// Enrollment con relación al package
interface EnrollmentWithPackage {
  id: string;
  student_id: string;
  package_id: string;
  package: {
    id: string;
    name: string;
    total_lessons: number;
    price_cents: number;
    currency: string;
  };
  lessons_remaining: number;
  lessons_total: number;  // Calculado del paquete
  status: 'active' | 'completed' | 'cancelled';
  purchase_date: string;
  expiration_date?: string;  // Calculado
}

// Lesson con relación al teacher
interface LessonWithTeacher {
  id: string;
  teacher_id: string;
  teacher: {
    id: string;
    name: string;
  };
  student_id: string;
  start_at: string;
  end_at: string;
  location: string;
  status: 'scheduled' | 'completed' | 'cancelled';
  notes?: string;
}
```

## Testing

### Casos de Prueba Sugeridos

1. **Estudiante con paquetes activos**: Verificar cálculos correctos
2. **Estudiante sin paquetes**: Mostrar mensaje informativo
3. **Estudiante con clases programadas**: Timeline correcto
4. **Estudiante sin clases**: Mensaje apropiado
5. **Error de API**: Manejo de errores graceful
6. **Loading state**: Spinner visible durante carga

### Datos de Prueba

```sql
-- Usar los datos de seed en:
-- tdf-hq/sql/2025-10-21_packages_lessons_receipts.sql

-- Student ID de ejemplo: 22222222-2222-2222-2222-222222222222
-- Enrollment ID: 55555555-5555-5555-5555-555555555555
```

## Contribución

Para extender esta funcionalidad:

1. Modificar `/tdf-hq-ui/src/pages/students/StudentDashboard.tsx`
2. Actualizar tipos en `/tdf-hq-ui/src/api/hq/hooks.ts` si cambian los schemas
3. Agregar tests en `/tdf-hq-ui/src/pages/students/__tests__/StudentDashboard.test.tsx`
4. Documentar cambios aquí

## Changelog

### v1.1.0 - 2025-11-04

- ✅ **FIXED**: Cálculo de progreso ahora funcional usando datos del paquete
- ✅ **FIXED**: Nombres de paquetes se muestran correctamente
- ✅ **FIXED**: Nombres de profesores en lugar de IDs
- ✅ **IMPROVED**: Validación de studentId con mensaje de error
- ✅ **IMPROVED**: Lookup maps para mejor rendimiento

### v1.0.0 - 2025-11-04

- ✅ Implementación inicial del Student Dashboard
- ✅ Estadísticas de paquetes y clases
- ✅ Tabla de paquetes con estado
- ✅ Historial de clases con filtrado
- ✅ Manejo de estados (loading, error, sin datos)
- ✅ Diseño responsive con Material-UI
- ✅ Integración con React Query
- ✅ Formato de fechas en español

---

**Autor**: AI Assistant  
**Fecha**: 4 de Noviembre, 2025  
**Versión**: 1.0.0
