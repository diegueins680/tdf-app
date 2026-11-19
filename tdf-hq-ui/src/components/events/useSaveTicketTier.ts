import { useMutation, useQueryClient } from '@tanstack/react-query';
import { SocialEventsAPI, type SocialTicketTierDTO } from '../../api/socialEvents';
import { TICKET_SALE_REQUESTS_QUERY_KEY, ticketSaleQueryKey } from './ticketSaleQueryKeys';

interface SaveTicketTierTarget {
  eventId: string;
  tierId?: string;
}

export function useSaveTicketTier(
  buildTier: (target: SaveTicketTierTarget) => SocialTicketTierDTO,
  callbacks: {
    onSuccess: (tier: SocialTicketTierDTO, target: SaveTicketTierTarget) => void;
    onError?: (error: Error) => void;
  },
) {
  const queryClient = useQueryClient();
  return useMutation({
    mutationFn: (target: SaveTicketTierTarget) => {
      const payload = buildTier(target);
      return target.tierId === undefined
        ? SocialEventsAPI.createTicketTier(target.eventId, payload)
        : SocialEventsAPI.updateTicketTier(target.eventId, target.tierId, payload);
    },
    onSuccess: (tier, target) => {
      const { eventId } = target;
      void queryClient.invalidateQueries({ queryKey: ['social-event-ticket-tiers', eventId] });
      void queryClient.invalidateQueries({ queryKey: ['social-ticket-tiers', eventId] });
      void queryClient.invalidateQueries({ queryKey: ticketSaleQueryKey(eventId) });
      void queryClient.invalidateQueries({ queryKey: ['public-event-ticket-storefront', eventId] });
      void queryClient.invalidateQueries({ queryKey: TICKET_SALE_REQUESTS_QUERY_KEY });
      callbacks.onSuccess(tier, target);
    },
    onError: callbacks.onError,
  });
}
