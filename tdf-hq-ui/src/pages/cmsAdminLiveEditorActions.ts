export interface CmsLiveEditorActionState {
  showUseLiveAction: boolean;
  showLiveInSyncChip: boolean;
  useLiveActionLabel: string;
}

export interface CmsLiveEditorActionInput {
  hasSlugSelection: boolean;
  hasLiveContent: boolean;
  hasPayloadError: boolean;
  isLoadingLiveOnDemand: boolean;
  payloadChangedFromLive: boolean;
  statusChangedFromLive: boolean;
  titleChangedFromLive: boolean;
}

export const getCmsLiveEditorActionState = ({
  hasSlugSelection,
  hasLiveContent,
  hasPayloadError,
  isLoadingLiveOnDemand,
  payloadChangedFromLive,
  statusChangedFromLive,
  titleChangedFromLive,
}: CmsLiveEditorActionInput): CmsLiveEditorActionState => {
  const hasRecoverableEditorDifference =
    hasPayloadError || payloadChangedFromLive || statusChangedFromLive || titleChangedFromLive;
  const canUseLiveContent = hasSlugSelection && hasLiveContent;

  return {
    showUseLiveAction: canUseLiveContent && hasRecoverableEditorDifference,
    showLiveInSyncChip: canUseLiveContent && !hasRecoverableEditorDifference,
    useLiveActionLabel: isLoadingLiveOnDemand ? 'Cargando versión en vivo...' : 'Usar versión en vivo',
  };
};
