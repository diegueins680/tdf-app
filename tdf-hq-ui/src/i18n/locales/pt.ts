const pt = {
  common: { save: 'Salvar', saving: 'Salvando…', saved: 'Preferências salvas.', error: 'Ocorreu um erro.' },
  sessionMenu: { noRoles: 'Nenhuma função atribuída', logout: 'Sair', open: 'Abrir menu da sessão' },
  preferences: {
    title: 'Idioma e região', subtitle: 'Escolha como idioma, dinheiro, datas e horários são exibidos.',
    language: 'Idioma', currency: 'Moeda', timezone: 'Fuso horário', country: 'Código do país',
    countryHint: 'Código ISO opcional de duas letras, por exemplo BR ou PT.',
  },
  system: { title: 'Status do sistema', subtitle: 'Versão, integridade e metadados do backend.', application: 'Aplicação', version: 'Versão', status: 'Status', built: 'Compilado', codebase: 'Código-base' },
} as const;

export default pt;
