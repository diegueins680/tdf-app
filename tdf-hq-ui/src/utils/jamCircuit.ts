export const jamRegistrationPath = '/social/eventos/nuevo?circuito=jams';
export const jamCircuitName = 'Circuito de Jams';

export function mentionsJamCircuit(text: string) {
  return /\bcircuito de jams\b/iu.test(text);
}
