#!/usr/bin/env node
// /directory/search over a running backend whose database holds the rows of
// tdf-hq/test/integration/directory_event_search_http_seed.sql: events reach
// the Eventos tab and Todo under their venue's city, people-only filters do
// not exclude them, the lineup's genre finds them, every tab is counted
// whichever tab is selected, and the card carries image, venue and start time.
//
// Usage: node directory-event-search-http.mjs <api-base> <genre-id> <profession-id>
const [api, genreId, professionId] = process.argv.slice(2);
if (!api || !genreId || !professionId) {
  console.error('usage: directory-event-search-http.mjs <api-base> <genre-id> <profession-id>');
  process.exit(2);
}
const quito = '24000000-0000-4000-8000-000000000002';
const quitoEvent = 'event:HTTP search Quito synth night';
const otherEvent = 'event:HTTP search unresolved city night';
const unknownGenre = '11111111-1111-4111-8111-111111111111';

const search = async (query) => {
  const response = await fetch(`${api}/directory/search?${query}`);
  if (!response.ok) throw new Error(`GET /directory/search?${query} -> ${response.status}`);
  return response.json();
};
const keys = (value) => value.items.map((item) => `${item.type}:${item.title}`);
const failures = [];
const expect = (name, ok, detail) => {
  console.log(`${ok ? 'ok' : 'FAIL'} - ${name}${ok ? '' : ` (${detail})`}`);
  if (!ok) failures.push(name);
};

let page = await search(`entityType=event&cityId=${quito}`);
expect('Eventos + Quito lists the Quito event and nothing else', keys(page).join(',') === quitoEvent, JSON.stringify(keys(page)));
expect('total counts the whole match set', page.facets.total === 1, JSON.stringify(page.facets));
expect('every tab is counted while Eventos is selected', page.facets.entityTypes.event === 1 && page.facets.entityTypes.venue >= 1 && Number.isInteger(page.facets.entityTypes.all), JSON.stringify(page.facets.entityTypes));
const card = page.items[0] ?? {};
expect('event card carries image, venue and start time', card.imageUrl === 'https://cdn.example.test/http-search-quito.jpg' && card.subtitle === 'HTTP search Quito venue' && typeof card.effectiveAt === 'string' && card.location?.cityId === quito, JSON.stringify(card));

page = await search(`entityType=event&cityId=${quito}&professionId=${professionId}&remote=true&available=true`);
expect('profession, remote and availability filters do not exclude events', keys(page).join(',') === quitoEvent && page.facets.total === 1, JSON.stringify(keys(page)));

page = await search(`entityType=event&cityId=${quito}&genreId=${genreId}`);
expect('the lineup genre finds the event', keys(page).join(',') === quitoEvent, JSON.stringify(keys(page)));
page = await search(`entityType=event&cityId=${quito}&genreId=${unknownGenre}`);
expect('a genre nobody on the bill plays excludes the event', page.items.length === 0 && page.facets.total === 0, JSON.stringify(keys(page)));

page = await search('entityType=event&q=HTTP%20search%20headliner');
expect('the lineup artist name is searchable', keys(page).join(',') === quitoEvent, JSON.stringify(keys(page)));
page = await search('entityType=event&q=Eventos&limit=50');
expect('"Eventos" without a city returns both public events', keys(page).includes(quitoEvent) && keys(page).includes(otherEvent), JSON.stringify(keys(page)));
expect('an event whose venue city is unresolved has no city', page.items.find((item) => `${item.type}:${item.title}` === otherEvent)?.location?.cityId == null, JSON.stringify(page.items.map((item) => item.location)));

page = await search(`cityId=${quito}&limit=50`);
expect('Todo in Quito contains the event and its venue', keys(page).includes(quitoEvent) && keys(page).includes('venue:HTTP search Quito venue'), JSON.stringify(keys(page)));
expect('Todo total equals the all-tab count', page.facets.total === page.facets.entityTypes.all, JSON.stringify(page.facets));
expect('the other city event is not under Quito', !keys(page).includes(otherEvent), JSON.stringify(keys(page)));

page = await search(`entityType=profile&cityId=${quito}`);
expect('the Perfiles tab still counts events', page.facets.entityTypes.event === 1, JSON.stringify(page.facets.entityTypes));

const rejected = await fetch(`${api}/directory/search?entityType=events`);
expect('an unknown entityType is rejected with 400', rejected.status === 400, String(rejected.status));

if (failures.length > 0) {
  console.error(`directory event search HTTP checks failed: ${failures.join('; ')}`);
  process.exit(1);
}
console.log('directory event search HTTP checks passed');
