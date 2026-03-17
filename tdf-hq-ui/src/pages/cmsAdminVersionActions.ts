export interface CmsVersionRowActions {
  showPublish: boolean;
  showPublishAndView: boolean;
  showOpenLivePage: boolean;
}

export const getCmsVersionRowActions = (status: string): CmsVersionRowActions => {
  const isPublished = status.trim().toLowerCase() === 'published';

  return {
    showPublish: !isPublished,
    showPublishAndView: !isPublished,
    showOpenLivePage: isPublished,
  };
};
