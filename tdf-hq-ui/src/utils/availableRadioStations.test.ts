import { availableRadioStations } from './availableRadioStations';

it('excludes exhausted streams and empty addresses while preserving eligible station order', () => {
  const stations = [
    { id: 'bad', streamUrl: 'https://media.invalid/missing.mp3' },
    { id: 'blank', streamUrl: '  ' },
    { id: 'good', streamUrl: 'https://media.invalid/good.mp3' },
    { id: 'torrent', streamUrl: 'magnet:?xt=urn:btih:example' },
  ];
  expect(availableRadioStations(stations, ['radio:bad'])).toEqual(stations.slice(2));
  expect(stations).toHaveLength(4);
});

it('uses exact radio identities without condemning artist tracks with the same id', () => {
  const stations = [{ id: 'one', streamUrl: 'https://media.invalid/one.mp3' }];
  expect(availableRadioStations(stations, ['one'])).toEqual(stations);
  expect(availableRadioStations(stations, ['radio:one'])).toEqual([]);
  expect(availableRadioStations(stations, [])).toEqual(stations);
});
