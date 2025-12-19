import { useEffect, useMemo, useState } from 'react';
import { useQuery } from '@tanstack/react-query';

import { ChatAPI } from '../api/chat';
import { useSession } from '../session/SessionContext';
import { countUnreadThreads, loadChatReadMap, subscribeToChatReadState } from '../utils/chatReadState';

export function useChatUnreadCount(opts: { enabled?: boolean } = {}) {
  const { session } = useSession();
  const enabled = (opts.enabled ?? true) && Boolean(session?.partyId);
  const [readVersion, setReadVersion] = useState(0);

  useEffect(() => subscribeToChatReadState(() => setReadVersion((v) => v + 1)), []);

  const threadsQuery = useQuery({
    queryKey: ['chat-threads'],
    queryFn: ChatAPI.listThreads,
    enabled,
    staleTime: 10_000,
    refetchInterval: 10_000,
  });

  const unreadCount = useMemo(() => {
    void readVersion;
    const threads = threadsQuery.data ?? [];
    const map = loadChatReadMap();
    return countUnreadThreads(threads, map);
  }, [readVersion, threadsQuery.data]);

  return { unreadCount, threadsQuery };
}
