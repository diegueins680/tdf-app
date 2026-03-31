export interface CmsVersionRowActions {
  showPublish: boolean;
  showLoadInEditor: boolean;
  loadedStateLabel: string | null;
}

export const getCmsVersionRowActions = (
  status: string,
  { isLoadedInEditor = false }: { isLoadedInEditor?: boolean } = {},
): CmsVersionRowActions => {
  const isPublished = status.trim().toLowerCase() === 'published';

  return {
    showPublish: !isPublished,
    showLoadInEditor: !isLoadedInEditor,
    loadedStateLabel: isLoadedInEditor ? 'En formulario' : null,
  };
};
