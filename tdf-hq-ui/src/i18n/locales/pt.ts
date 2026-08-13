const pt = {
  common: { save: 'Salvar', saving: 'Salvando…', saved: 'Preferências salvas.', error: 'Ocorreu um erro.' },
  internships: { taskDetail: { instructions: 'Instruções' } },
  pagination: {
    items: 'itens', rowsPerPage: 'Por página', loading: 'Carregando resultados…', updating: 'Atualizando resultados…',
    displayed: '{{from}}–{{to}} de {{count}} {{itemLabel}}', displayedMoreThan: '{{from}}–{{to}} de mais de {{count}} {{itemLabel}}',
    firstPage: 'Ir para a primeira página', lastPage: 'Ir para a última página',
    nextPage: 'Ir para a próxima página', previousPage: 'Ir para a página anterior',
  },
  promoCode: {
    label: 'Código promocional (opcional)', placeholder: 'DIGITE-O-CÓDIGO', clear: 'Limpar código promocional',
    inactive: 'O código promocional não está ativo', invalid: 'Código promocional inválido', checking: 'Verificando código promocional…',
    validUntil: 'Válido até', percentOff: '{{value}}% de desconto', fixedOff: '{{value}} de desconto',
    usesRemaining_one: '{{count}} uso restante', usesRemaining_other: '{{count}} usos restantes',
  },
  refunds: {
    loading: 'Carregando solicitações de reembolso', loadError: 'Não foi possível carregar as solicitações de reembolso.',
    pendingSummary_one: '{{count}} solicitação de reembolso aguardando aprovação',
    pendingSummary_other: '{{count}} solicitações de reembolso aguardando aprovação', itemLabel: 'reembolsos',
    orderId: 'ID do pedido', amount: 'Valor', reason: 'Motivo', status: 'Status', requested: 'Solicitado', actions: 'Ações',
    empty: 'Nenhuma solicitação de reembolso', approve: 'Aprovar', reject: 'Rejeitar', rejected: 'Rejeitado: {{reason}}',
    confirmApproval: 'Aprovar reembolso de {{amount}}?', rejectDialogTitle: 'Rejeitar solicitação de reembolso',
    rejectionReason: 'Motivo da rejeição', rejectionPlaceholder: 'Explique por que esta solicitação de reembolso está sendo rejeitada…',
    cancel: 'Cancelar', rejectRefund: 'Rejeitar reembolso',
    statuses: { pending: 'Pendente', approved: 'Aprovado', rejected: 'Rejeitado', processed: 'Processado' },
  },
  artistFans: {
    loading: 'Carregando fãs…', loadError: 'Não foi possível carregar os fãs.',
    empty: 'Este artista ainda não tem fãs.', followedSince: 'Fã desde {{date}}',
  },
  partyRelated: {
    unavailable: 'Contato indisponível', contact: 'Contato', subtitle: 'Navegação rápida pelo histórico relacionado',
    organization: 'Organização', user: 'Usuário', profile: 'Perfil', customerBookings: 'Reservas (cliente)',
    engineerBookings: 'Reservas (engenheiro)', studentClasses: 'Aulas (aluno)', teacherClasses: 'Aulas (professor)',
    tracks: 'Faixas', bookings: 'Reservas', customer: 'Cliente', engineer: 'Engenheiro', classes: 'Aulas',
    student: 'Aluno', teacher: 'Professor', bookingFallback: 'Reserva nº {{id}}', subjectFallback: 'Matéria nº {{id}}',
    studentFallback: 'Aluno nº {{id}}', updated: 'Atualizado: {{date}}', loading: 'Carregando histórico relacionado',
    loadError: 'Não foi possível carregar o histórico relacionado.',
    empty: 'Ainda não há histórico relacionado. Use Perfil para revisar ou completar este contato.',
    bookingItems: 'reservas', classItems: 'aulas', trackItems: 'faixas',
    statuses: { confirmed: 'Confirmada', confirmada: 'Confirmada', scheduled: 'Agendada', programada: 'Agendada', mix: 'Mixagem' },
  },
  sessionMenu: { noRoles: 'Nenhuma função atribuída', logout: 'Sair', open: 'Abrir menu da sessão' },
  preferences: {
    title: 'Idioma e região', subtitle: 'Escolha como idioma, dinheiro, datas e horários são exibidos.',
    language: 'Idioma', currency: 'Moeda', timezone: 'Fuso horário', country: 'Código do país',
    countryHint: 'Selecione um país do catálogo oficial ou deixe o campo vazio.',
  },
  system: { title: 'Status do sistema', subtitle: 'Versão, integridade e metadados do backend.', application: 'Aplicação', version: 'Versão', status: 'Status', built: 'Compilado', codebase: 'Código-base' },
} as const;

export default pt;
