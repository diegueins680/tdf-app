import { getCmsLiveEditorActionState } from './cmsAdminLiveEditorActions';

const baseInput = {
  hasSlugSelection: true,
  hasLiveContent: true,
  hasPayloadError: false,
  isLoadingLiveOnDemand: false,
  payloadChangedFromLive: false,
  statusChangedFromLive: false,
  titleChangedFromLive: false,
};

describe('getCmsLiveEditorActionState', () => {
  it('replaces the live reload button with an in-sync chip when using live would be a no-op', () => {
    expect(getCmsLiveEditorActionState(baseInput)).toEqual({
      showUseLiveAction: false,
      showLiveInSyncChip: true,
      useLiveActionLabel: 'Usar versión en vivo',
    });
  });

  it('keeps the live reload path when the editor has recoverable differences', () => {
    expect(getCmsLiveEditorActionState({
      ...baseInput,
      payloadChangedFromLive: true,
    })).toEqual({
      showUseLiveAction: true,
      showLiveInSyncChip: false,
      useLiveActionLabel: 'Usar versión en vivo',
    });

    expect(getCmsLiveEditorActionState({
      ...baseInput,
      hasPayloadError: true,
    }).showUseLiveAction).toBe(true);

    expect(getCmsLiveEditorActionState({
      ...baseInput,
      titleChangedFromLive: true,
    }).showUseLiveAction).toBe(true);

    expect(getCmsLiveEditorActionState({
      ...baseInput,
      statusChangedFromLive: true,
    }).showUseLiveAction).toBe(true);
  });

  it('does not show live actions until both a slug and live content are available', () => {
    expect(getCmsLiveEditorActionState({
      ...baseInput,
      hasLiveContent: false,
      payloadChangedFromLive: true,
    })).toMatchObject({
      showUseLiveAction: false,
      showLiveInSyncChip: false,
    });

    expect(getCmsLiveEditorActionState({
      ...baseInput,
      hasSlugSelection: false,
      payloadChangedFromLive: true,
    })).toMatchObject({
      showUseLiveAction: false,
      showLiveInSyncChip: false,
    });
  });
});
