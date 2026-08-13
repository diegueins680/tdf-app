import { catalogEditorPath } from './CatalogsPage';

describe('catalog administration routing', () => {
  it('uses persisted entity kinds only as typed editor dispatch boundaries', () => {
    expect(catalogEditorPath('appearance_mode_option')).toBe('/configuracion/catalogos/apariencia');
    expect(catalogEditorPath('radio_auto_stop_option')).toBe('/configuracion/catalogos/radio-auto-stop');
    expect(catalogEditorPath('feedback_category')).toBe('/configuracion/catalogos/feedback');
    expect(catalogEditorPath('feedback_severity')).toBe('/configuracion/catalogos/feedback');
    expect(catalogEditorPath('reaction_type')).toBe('/configuracion/catalogos/reacciones');
    expect(catalogEditorPath('content_reaction_type')).toBe('/configuracion/catalogos/reacciones-contenido');
    expect(catalogEditorPath('unknown_business_list')).toBeUndefined();
  });
});
