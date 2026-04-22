import { NAV_GROUPS } from './SidebarNav';
import { COURSE_REGISTRATIONS_NAV_LABEL, formatFriendlyPath } from '../utils/navigationLabels';

describe('course registration navigation labels', () => {
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
});
