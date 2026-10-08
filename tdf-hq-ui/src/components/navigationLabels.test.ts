import { jest } from '@jest/globals';

jest.unstable_mockModule('../session/SessionContext', () => ({
  getStoredSessionToken: () => null,
  useSession: () => ({ session: null }),
}));

jest.unstable_mockModule('../hooks/useChatUnreadCount', () => ({
  useChatUnreadCount: () => ({ unreadCount: 0 }),
}));

const { NAV_GROUPS } = await import('./SidebarNav');
const { COURSE_REGISTRATIONS_NAV_LABEL, formatFriendlyPath } = await import('../utils/navigationLabels');

describe('navigation labels', () => {
  it('exposes the access-request review queue in the admin group', () => {
    const adminGroup = NAV_GROUPS.find((group) => group.title === 'ADMIN');

    expect(adminGroup?.items).toContainEqual({
      label: 'Solicitudes de acceso',
      path: '/solicitudes-acceso/revision',
    });
    expect(formatFriendlyPath('/solicitudes-acceso/revision')).toBe(
      'Solicitudes de acceso / Revisión',
    );
  });

  it('uses the same clear course-registration label in navigation and breadcrumbs', () => {
    const courseRegistrationItem = NAV_GROUPS
      .flatMap((group) => group.items)
      .find((item) => item.path === '/configuracion/inscripciones-curso');

    expect(courseRegistrationItem?.label).toBe(COURSE_REGISTRATIONS_NAV_LABEL);
    expect(formatFriendlyPath('/configuracion/inscripciones-curso')).toBe(
      `Configuración / ${COURSE_REGISTRATIONS_NAV_LABEL}`,
    );
    expect(courseRegistrationItem?.label).not.toBe('Inscripciones curso');
  });

  it('keeps the sidebar free of duplicate destinations', () => {
    const seenPaths = new Set<string>();
    const duplicatedPaths = NAV_GROUPS
      .flatMap((group) => group.items)
      .map((item) => item.path)
      .filter((path) => {
        if (seenPaths.has(path)) return true;
        seenPaths.add(path);
        return false;
      });

    expect(duplicatedPaths).toEqual([]);
  });

  it('keeps sidebar destination labels unique enough for first-time admins', () => {
    const seenLabels = new Set<string>();
    const duplicatedLabels = NAV_GROUPS
      .flatMap((group) => group.items)
      .map((item) => item.label)
      .filter((label) => {
        if (seenLabels.has(label)) return true;
        seenLabels.add(label);
        return false;
      });

    expect(duplicatedLabels).toEqual([]);
    expect(formatFriendlyPath('/records')).toBe('Lanzamientos públicos');
    expect(formatFriendlyPath('/label/releases')).toBe('Sello / Lanzamientos');
  });
});
