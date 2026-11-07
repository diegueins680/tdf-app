import { fireEvent, render, screen, cleanup } from '@testing-library/react';
import { afterEach, describe, expect, it } from '@jest/globals';
import RecordThumbnail from './RecordThumbnail';
import { isProviderPlaceholder, primaryRecordsResource, recordThumbnailCandidates } from './resolveRecordThumbnail';
import type { RecordsResourceDTO } from '../../api/records';

const youtube = { providerCode: 'youtube', externalCode: 'f2BabxM1Pjc' };
afterEach(cleanup);
describe('resource thumbnails', () => {
  it('retains valid secondary artwork when the primary resource cannot supply an image', () => {
    const primary: RecordsResourceDTO = {
      id: 'primary', providerCode: 'vimeo', externalCode: '123',
      kind: 'video', url: 'https://vimeo.com/123', primary: true,
      relationKind: 'watch', sortOrder: 0,
    };
    const secondary: RecordsResourceDTO = { ...primary, id: 'secondary', primary: false,
      thumbnailUrl: 'https://cdn.example.org/editorial.jpg' };
    expect(primaryRecordsResource([primary, secondary])).toBe(secondary);
    expect(primaryRecordsResource([{ ...primary, ...youtube, availability: 'unavailable' }, secondary])).toBe(secondary);
    const illustratedPrimary = { ...primary, thumbnailUrl: secondary.thumbnailUrl };
    expect(primaryRecordsResource([illustratedPrimary, secondary])).toBe(illustratedPrimary);
    expect(primaryRecordsResource([primary])).toBe(primary);
    expect(primaryRecordsResource([])).toBeUndefined();
  });
  it('preserves explicit editorial images, rejects mismatched videos and limits retries', () => {
    expect(recordThumbnailCandidates({ ...youtube, thumbnailUrl: ' https://cdn.example.org/editorial.jpg ' })[0])
      .toBe('https://cdn.example.org/editorial.jpg');
    expect(recordThumbnailCandidates({ ...youtube, thumbnailUrl: 'https://i.ytimg.com/vi/ooPsIHsikYU/hqdefault.jpg' }))
      .toEqual(['https://i.ytimg.com/vi/f2BabxM1Pjc/hqdefault.jpg', 'https://i.ytimg.com/vi/f2BabxM1Pjc/mqdefault.jpg']);
    expect(recordThumbnailCandidates({ ...youtube, thumbnailUrl: '' })).toHaveLength(2);
    expect(recordThumbnailCandidates({ ...youtube, thumbnailUrl: 'javascript:alert(1)' })).toHaveLength(2);
    expect(recordThumbnailCandidates({ providerCode: 'vimeo', externalCode: 'f2BabxM1Pjc' })).toEqual([]);
    expect(recordThumbnailCandidates({ ...youtube, externalCode: '../bad' })).toEqual([]);
  });
  it('detects provider placeholders without rejecting legitimate small editorial images', () => {
    expect(isProviderPlaceholder('https://i.ytimg.com/vi/f2BabxM1Pjc/hqdefault.jpg', 120, 90)).toBe(true);
    expect(isProviderPlaceholder('https://i.ytimg.com/vi/f2BabxM1Pjc/hqdefault.jpg', 480, 360)).toBe(false);
    expect(isProviderPlaceholder('https://cdn.example.org/editorial.jpg', 120, 90)).toBe(false);
  });
  it('observes the actual img failures and does not restart on rerender', () => {
    const { rerender } = render(<RecordThumbnail resource={youtube} title="Federico" />);
    fireEvent.error(screen.getByRole('img', { name: 'Federico' }));
    expect(screen.getByRole('img', { name: 'Federico' }).getAttribute('src')).toContain('mqdefault');
    fireEvent.error(screen.getByRole('img', { name: 'Federico' }));
    expect(screen.getByRole('img', { name: 'Miniatura no disponible' })).toBeTruthy();
    rerender(<RecordThumbnail resource={{ ...youtube }} title="Federico" />);
    expect(screen.getByRole('img', { name: 'Miniatura no disponible' })).toBeTruthy();
  });
  it('advances overview artwork after an unverified primary video fails at runtime', () => {
    const secondary = { providerCode: 'vimeo', externalCode: '123', thumbnailUrl: 'https://cdn.example.org/recording-art.jpg' };
    const { rerender } = render(<RecordThumbnail resource={youtube} fallbackResources={[youtube, secondary]} title="Recording overview" />);
    fireEvent.error(screen.getByRole('img', { name: 'Recording overview' }));
    fireEvent.error(screen.getByRole('img', { name: 'Recording overview' }));
    expect(screen.getByRole('img', { name: 'Recording overview' }).getAttribute('src')).toBe(secondary.thumbnailUrl);
    fireEvent.load(screen.getByRole('img', { name: 'Recording overview' }));
    rerender(<RecordThumbnail resource={{ ...youtube }} fallbackResources={[{ ...youtube }, { ...secondary }]} title="Recording overview" />);
    expect(screen.getByRole('img', { name: 'Recording overview' }).getAttribute('src')).toBe(secondary.thumbnailUrl);
    fireEvent.error(screen.getByRole('img', { name: 'Recording overview' }));
    expect(screen.getByRole('img', { name: 'Miniatura no disponible' })).toBeTruthy();
  });
  it('rejects a decoded 200 placeholder and keeps the working comparison image', () => {
    render(<RecordThumbnail resource={youtube} title="Federico" />);
    const image = screen.getByRole('img', { name: 'Federico' });
    Object.defineProperties(image, { naturalWidth: { value: 120 }, naturalHeight: { value: 90 } });
    fireEvent.load(image);
    const fallback = screen.getByRole('img', { name: 'Federico' });
    expect(fallback.getAttribute('src')).toContain('mqdefault');
    Object.defineProperties(fallback, { naturalWidth: { value: 320 }, naturalHeight: { value: 180 } });
    fireEvent.load(fallback);
    expect(screen.queryByLabelText('Cargando miniatura')).toBeNull();
  });
  it.each(['ooPsIHsikYU', 'Cb7VGZJ6apo'])('does not request an image for verified removed video %s', externalCode => {
    const { container } = render(<RecordThumbnail resource={{ ...youtube, externalCode, availability: 'unavailable' }} title="Llama Este Pez" />);
    expect(container.querySelector('img')).toBeNull();
    expect(screen.getByRole('img', { name: 'Video no disponible en la fuente' })).toBeTruthy();
  });
});

const first={providerCode:'youtube',externalCode:'ooPsIHsikYU'};
describe('bounded fallback scheduling and ownership controls',()=>{
 it('keeps initial image lazy and only starts its existing next candidate eagerly after failure',()=>{
  render(<RecordThumbnail resource={first} title="resource"/>);
  const original=screen.getByRole('img',{name:'resource'});
  expect(original.getAttribute('loading')).toBe('lazy');
  expect(original.getAttribute('src')).toBe('https://i.ytimg.com/vi/ooPsIHsikYU/hqdefault.jpg');
  fireEvent.error(original);
  const fallback=screen.getByRole('img',{name:'resource'});
  expect(fallback.getAttribute('loading')).toBe('eager');
  expect(fallback.getAttribute('src')).toBe('https://i.ytimg.com/vi/ooPsIHsikYU/mqdefault.jpg');
  expect(document.querySelectorAll('img')).toHaveLength(1);
 });
 it('does not restart an exhausted candidate queue after stale detached image events',()=>{
  const {rerender}=render(<RecordThumbnail resource={first} title="resource"/>);
  const original=screen.getByRole('img',{name:'resource'});fireEvent.error(original);
  const fallback=screen.getByRole('img',{name:'resource'});fireEvent.error(fallback);
  fireEvent.error(original);fireEvent.load(original);fireEvent.error(fallback);fireEvent.load(fallback);
  rerender(<RecordThumbnail resource={{...first}} title="resource"/>);
  expect(document.querySelectorAll('img')).toHaveLength(0);
  expect(screen.getByRole('img',{name:'Miniatura no disponible'})).toBeTruthy();
 });
 it('keeps changed resource identity lazy and ignores old resource events and foreign metadata',()=>{
  const {rerender}=render(<RecordThumbnail resource={first} title="resource"/>);
  const retired=screen.getByRole('img',{name:'resource'});
  const next={providerCode:'youtube',externalCode:'Cb7VGZJ6apo',thumbnailUrl:'https://i.ytimg.com/vi/ooPsIHsikYU/hqdefault.jpg',providerMetadata:{id:'ooPsIHsikYU',thumbnails:[{url:'https://i.ytimg.com/vi/ooPsIHsikYU/maxresdefault.jpg'}]}};
  rerender(<RecordThumbnail resource={next} title="resource"/>);
  fireEvent.error(retired);fireEvent.load(retired);
  const current=screen.getByRole('img',{name:'resource'});
  expect(current.getAttribute('src')).toBe('https://i.ytimg.com/vi/Cb7VGZJ6apo/hqdefault.jpg');
  expect(current.getAttribute('loading')).toBe('lazy');
  fireEvent.error(current);
  expect(screen.getByRole('img',{name:'resource'}).getAttribute('src')).toBe('https://i.ytimg.com/vi/Cb7VGZJ6apo/mqdefault.jpg');
 });
 it('retains the existing three-resource queue bound and never eagerly loads a fourth resource',()=>{
  const other=[1,2,3].map(n=>({providerCode:'vimeo',externalCode:String(n),thumbnailUrl:`https://cdn.example.org/art-${n}.jpg`}));
  render(<RecordThumbnail resource={first} fallbackResources={other} title="resource"/>);
  const seen=[];for(let count=0;count<8;count++){
   const image=screen.queryByRole('img',{name:'resource'});if(!image)break;
   seen.push(image.getAttribute('src'));expect(image.getAttribute('loading')).toBe(count===0?'lazy':'eager');fireEvent.error(image);
  }
  expect(seen).toEqual(['https://i.ytimg.com/vi/ooPsIHsikYU/hqdefault.jpg','https://i.ytimg.com/vi/ooPsIHsikYU/mqdefault.jpg','https://cdn.example.org/art-1.jpg','https://cdn.example.org/art-2.jpg']);
  expect(document.querySelectorAll('img')).toHaveLength(0);
 });
});
