import { afterEach, beforeEach, describe, expect, it, jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import type { LessonSlotSuggestionQuery, LessonSlotSuggestions as Answer, LessonSlotSuggestion } from '../../api/trials';

const suggestLessonSlots = jest.fn<(query: LessonSlotSuggestionQuery) => Promise<Answer>>();
jest.unstable_mockModule('../../api/trials', () => ({ Trials: { suggestLessonSlots } }));
const { LessonSlotSuggestions } = await import('./LessonSlotSuggestions');

const slot = (startAt: string, roomId: number, teacherGapMinutes: number | null): LessonSlotSuggestion => ({
  startAt,
  endAt: startAt.replace(':00:00Z', ':59:00Z'),
  roomId,
  roomName: `Sala ${roomId}`,
  insidePreferred: false,
  teacherGapMinutes,
});

let client: QueryClient;
const onSelect = jest.fn<(suggestion: LessonSlotSuggestion) => void>();
beforeEach(() => {
  suggestLessonSlots.mockReset();
  onSelect.mockReset();
  client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
});
afterEach(() => { cleanup(); client.clear(); });

type Props = Parameters<typeof LessonSlotSuggestions>[0];
const element = (props: Partial<Props>) => (
  <QueryClientProvider client={client}>
    <LessonSlotSuggestions
      enabled
      teacherId={7}
      studentId={8}
      subjectId={9}
      roomId={null}
      durationMinutes={60}
      onSelect={onSelect}
      {...props}
    />
  </QueryClientProvider>
);

describe('LessonSlotSuggestions', () => {
  it('renders nothing and requests nothing while the flag is off', () => {
    const { container } = render(element({ enabled: false }));
    expect(container.textContent).toBe('');
    expect(suggestLessonSlots).not.toHaveBeenCalled();
  });

  it('does not ask until the user does, and needs teacher, student and subject', () => {
    render(element({ studentId: null }));
    expect(suggestLessonSlots).not.toHaveBeenCalled();
    expect((screen.getByRole('button', { name: 'Sugerir horarios' })).disabled).toBe(true);
  });

  it('lists the suggestions in server order and hands the chosen one back', async () => {
    const first = slot('2030-12-02T14:00:00Z', 1, 0);
    const second = slot('2030-12-02T18:00:00Z', 2, null);
    suggestLessonSlots.mockResolvedValue({ computedAt: '2030-12-01T20:00:00Z', suggestions: [first, second] });
    render(element({ roomId: 2, durationMinutes: 90 }));

    fireEvent.click(screen.getByRole('button', { name: 'Sugerir horarios' }));
    await screen.findByText('Junto a otra clase del profesor');
    expect(suggestLessonSlots).toHaveBeenCalledWith({
      teacherId: 7, studentId: 8, subjectId: 9, roomId: 2, durationMinutes: 90,
    });
    const panel = screen.getByTestId('lesson-slot-suggestions');
    expect(panel.textContent).toContain('Sugerencias, no reservas');
    expect(panel.textContent?.indexOf('Sala 1')).toBeLessThan(panel.textContent?.indexOf('Sala 2') ?? -1);

    fireEvent.click(screen.getAllByRole('button', { name: 'Usar' })[1]!);
    expect(onSelect).toHaveBeenCalledWith(second);
  });

  it('says so when no slot is feasible', async () => {
    suggestLessonSlots.mockResolvedValue({ computedAt: '2030-12-01T20:00:00Z', suggestions: [] });
    render(element({}));
    fireEvent.click(screen.getByRole('button', { name: 'Sugerir horarios' }));
    await screen.findByText(/No hay horarios disponibles/);
  });

  it('falls back to manual entry when the request fails', async () => {
    suggestLessonSlots.mockRejectedValue(new Error('404'));
    render(element({}));
    fireEvent.click(screen.getByRole('button', { name: 'Sugerir horarios' }));
    await waitFor(() => expect(screen.getByText(/No se pudieron calcular sugerencias/)).toBeTruthy());
    expect(screen.queryByRole('button', { name: 'Usar' })).toBeNull();
  });
});
