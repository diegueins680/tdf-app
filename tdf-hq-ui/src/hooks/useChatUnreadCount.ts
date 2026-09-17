import { useEffect, useMemo, useState } from 'react';
import { useQuery } from '@tanstack/react-query';

import { ChatAPI } from '../api/chat';
import { useSession } from '../session/SessionContext';
import { countUnreadThreads, loadChatReadMap, subscribeToChatReadState } from '../utils/chatReadState';

export function useChatUnreadCount(opts: { enabled?: boolean } = {}) {
  const { session } = useSession();
  const enabled = (opts.enabled ?? true) && Boolean(session?.partyId);
  const [readVersion, setReadVersion] = useState(0);

  useEffect(() => subscribeToChatReadState(() => setReadVersion((v) => v + 1), session?.partyId ?? 0), [session?.partyId]);

  const threadsQuery = useQuery({
    queryKey: ['chat-threads', session?.partyId ?? null],
    queryFn: ChatAPI.listThreads,
    enabled,
    staleTime: 10_000,
    refetchInterval: 10_000,
  });

  const unreadCount = useMemo(() => {
    void readVersion;
    const threads = enabled && !threadsQuery.isError ? threadsQuery.data ?? [] : [];
    const map = loadChatReadMap(session?.partyId ?? 0);
    return countUnreadThreads(threads, map);
  }, [enabled, session?.partyId, readVersion, threadsQuery.data, threadsQuery.isError]);

  return { unreadCount, threadsQuery };
}
