export interface CmsVersionRowActions {
  showPublish: boolean;
  showOpenLivePage: boolean;
}

export const getCmsVersionRowActions = (status: string): CmsVersionRowActions => {
  const isPublished = status.trim().toLowerCase() === 'published';

  return {
    showPublish: !isPublished,
    showOpenLivePage: isPublished,
  };
};
