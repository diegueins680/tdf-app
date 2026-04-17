export interface CmsVersionRowActions {
  showPublish: boolean;
  showLoadInEditor: boolean;
  loadedStateLabel: string | null;
}

export const getCmsVersionRowActions = (
  status: string,
  {
    isCurrentLive = false,
    isLoadedInEditor = false,
  }: { isCurrentLive?: boolean; isLoadedInEditor?: boolean } = {},
): CmsVersionRowActions => {
  const isPublished = status.trim().toLowerCase() === 'published';
  const loadedStateLabel = isLoadedInEditor ? 'En formulario' : isCurrentLive ? 'En vivo' : null;

  return {
    showPublish: !isPublished,
    showLoadInEditor: !loadedStateLabel,
    loadedStateLabel,
  };
};
