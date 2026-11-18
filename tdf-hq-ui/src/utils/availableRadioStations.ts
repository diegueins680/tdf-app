/** Share runtime availability across catalogue, hidden and recent station controls. */
export function availableRadioStations<T extends { id: string; streamUrl: string }>(
  stations: readonly T[], unavailableTrackIds: readonly string[],
): T[] {
  const unavailable = new Set(unavailableTrackIds);
  return stations.filter((station) => station.streamUrl.trim().length > 0
    && !unavailable.has(`radio:${station.id}`));
}
