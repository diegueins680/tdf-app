import { defaultMenuGroups, listedInMenuFor, menuFamilyMembers, menuFamilyOf, menuTabLabel } from './sidebarMenuRules';

const groups = [
  { title: 'CREAR', items: [
    { label: 'Inicio', path: '/inicio' },
    { label: 'Calendario', path: '/estudio/calendario' },
    { label: 'Salas y recursos', path: '/estudio/salas' },
    { label: 'Órdenes estudio', path: '/estudio/ordenes' },
    { label: 'Pipelines', path: '/estudio/pipelines' },
  ] },
  { title: 'GENTE', items: [
    { label: 'Directorio musical', path: '/buscar' },
    { label: 'Contactos', path: '/crm/contactos' },
    { label: 'Empresas', path: '/crm/empresas' },
    { label: 'Leads', path: '/crm/leads' },
    { label: 'Reservar DJ booth', path: '/dj-booth', searchOnly: true },
  ] },
  { title: 'AYUDA', items: [
    { label: 'Manual', path: '/manual' },
    { label: 'Sugerencias', path: '/feedback' },
  ] },
];

const labels = (menu: { items: { label: string }[] }[]) => menu.flatMap((group) => group.items.map((item) => item.label));

describe('sidebar menu rules', () => {
  it('lists internal studio pages for the studio team only', () => {
    expect(listedInMenuFor('/estudio/pipelines', ['Admin'])).toBe(true);
    expect(listedInMenuFor('/estudio/pipelines', ['Studio Manager'])).toBe(true);
    expect(listedInMenuFor('/estudio/pipelines', ['Reception'])).toBe(true);
    for (const roles of [['Artist'], ['Student'], ['Teacher'], ['Engineer'], ['Fan'], []]) {
      expect(listedInMenuFor('/estudio/pipelines', roles)).toBe(false);
      expect(listedInMenuFor('/estudio/calendario', roles)).toBe(true);
    }
  });

  it('keeps the session pages for engineers', () => {
    expect(listedInMenuFor('/estudio/ordenes', ['Engineer'])).toBe(true);
    expect(listedInMenuFor('/estudio/salas', ['Engineer'])).toBe(true);
    expect(listedInMenuFor('/estudio/reportes', ['Engineer'])).toBe(false);
    expect(listedInMenuFor('/estudio/ordenes', ['Artist'])).toBe(false);
  });

  it('leaves the duplicate directory entry and the avatar-menu pages out for everyone', () => {
    for (const path of ['/buscar', '/manual', '/docs', '/acerca', '/seguridad']) {
      expect(listedInMenuFor(path, ['Admin'])).toBe(false);
    }
    expect(listedInMenuFor('/feedback', ['Fan'])).toBe(true);
  });

  it('shows one entry per page family, named after the family', () => {
    const menu = defaultMenuGroups(groups, ['Admin'], '/inicio');
    expect(labels(menu)).toEqual(['Inicio', 'Calendario', 'Salas y recursos', 'Pipelines', 'Contactos', 'Sugerencias']);
    const contacts = menu[1]?.items[0];
    expect(contacts?.path).toBe('/crm/contactos');
    expect(contacts?.familyPaths).toEqual(['/crm/contactos', '/crm/empresas', '/crm/leads']);
  });

  it('gives an artist the calendar alone, with no family entry', () => {
    const menu = defaultMenuGroups(groups, ['Artist'], '/inicio');
    expect(labels(menu)).toEqual(['Inicio', 'Calendario', 'Contactos', 'Sugerencias']);
    expect(menu[0]?.items[1]?.familyPaths).toBeUndefined();
    expect(menuFamilyMembers(groups, 'calendar', ['Artist']).map((item) => item.path)).toEqual(['/estudio/calendario']);
  });

  it('lists an unlisted destination while it is the open page', () => {
    expect(labels(defaultMenuGroups(groups, ['Artist'], '/estudio/pipelines'))).toContain('Pipelines');
    expect(labels(defaultMenuGroups(groups, ['Admin'], '/manual'))).toContain('Manual');
    expect(labels(defaultMenuGroups(groups, ['Admin'], '/dj-booth'))).toContain('Reservar DJ booth');
    expect(labels(defaultMenuGroups(groups, ['Admin'], '/crm/leads/7'))).not.toContain('Leads');
  });

  it('keeps a family member with its own name when it is the only one listed', () => {
    const single = [{ title: 'GENTE', items: [{ label: 'Leads', path: '/crm/leads' }] }];
    expect(labels(defaultMenuGroups(single, ['Manager'], '/inicio'))).toEqual(['Leads']);
  });

  it('names tabs by what they show inside the family', () => {
    expect(menuFamilyOf('/estudio/ordenes')).toBe('calendar');
    expect(menuFamilyOf('/inicio')).toBeNull();
    expect(menuTabLabel({ label: 'Órdenes estudio', path: '/estudio/ordenes' })).toBe('Lista de sesiones');
    expect(menuTabLabel({ label: 'Leads', path: '/crm/leads' })).toBe('Leads');
  });
});
