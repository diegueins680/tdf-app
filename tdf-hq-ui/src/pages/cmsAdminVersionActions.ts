export interface CmsVersionRowActions {
  showPublish: boolean;
}

export const getCmsVersionRowActions = (status: string): CmsVersionRowActions => {
  const isPublished = status.trim().toLowerCase() === 'published';

  return {
    showPublish: !isPublished,
  };
};
