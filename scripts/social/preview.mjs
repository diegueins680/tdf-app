// Local component preview with synthetic data; NOT authenticated API/E2E evidence.
import { createServer } from 'vite';
import react from '@vitejs/plugin-react';
import { fileURLToPath } from 'node:url';
import path from 'node:path';
const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '../..');
const mocks = {
  session: 'export const useSession = () => ({session:{partyId:1}});',
  api: `
const relation = { partyId:2, revision:0, following:false, requested:false, incoming:true,
 connected:false, blocked:false, muted:false, dismissed:false };
const profile = { discoverable:false, personalized:true, revision:0,
 relationships:[{...relation,displayName:'Lucía — guitarra y producción'}] };
export const SocialV2 = {
 me: async()=>structuredClone(profile),
 following:async()=>({items:[{postId:1,position:'1',publishedAt:'2026-09-14T12:00:00Z',
 createdAt:'2026-09-14T12:00:00Z',title:'Buscamos una voz para una colaboración',
 content:'Somos un proyecto independiente en Quito. Queremos grabar una canción juntos y compartir la producción.',
 authorId:2,authorName:'Lucía — guitarra y producción',artistId:2}],nextCursor:null}),
 discover:async()=>({personalized:profile.personalized,items:[{partyId:2,
 displayName:'Lucía — guitarra y producción',reason:profile.personalized?'shared_interests':'public_profile',relationship:{...relation}}]}),
 preferences:async(value)=>Object.assign(profile,value,{revision:profile.revision+1}),
 command:async(_,operation)=>{
   if(operation==='accept'){relation.connected=true;relation.requested=true;}
   if(operation==='block'){relation.blocked=true;relation.connected=false;relation.incoming=false;relation.requested=false;}
   relation.revision++;profile.relationships=[{...relation,displayName:'Lucía — guitarra y producción'}];return {...relation};
 }
};`,
};
const server = await createServer({
  root: path.join(root, 'tdf-hq-ui'), configFile: false,
  resolve: { dedupe: ['react', 'react-dom', '@tanstack/react-query', 'react-router-dom'] },
  plugins: [react(), {
    name: 'synthetic-social-preview',
    enforce: 'pre',
    resolveId(id) {
      if (id.replace(/\.(tsx?|jsx?)$/, '').endsWith('/session/SessionContext')) return '\0social-preview-session';
      if (id.replace(/\.(tsx?|jsx?)$/, '').endsWith('/api/socialV2')) return '\0social-preview-api';
    },
    load(id) {
      if (id === '\0social-preview-session') return mocks.session;
      if (id === '\0social-preview-api') return mocks.api;
    },
    configureServer(vite) {
      vite.middlewares.use('/social-preview', async (_req, res) => {
        res.setHeader('Content-Type', 'text/html');
        res.end(await vite.transformIndexHtml('/social-preview', `<!doctype html><html lang="es"><meta name="viewport" content="width=device-width,initial-scale=1"><title>Social — prueba local</title><body style="margin:24px"><div id="root"></div><script type="module" src="/@fs/${root}/scripts/social/preview-entry.tsx"></script></body></html>`));
      });
    },
  }],
  optimizeDeps: { entries: [], include: ['react', 'react-dom/client', '@tanstack/react-query', 'react-router-dom'] },
  server: { host: '127.0.0.1', port: 5199, strictPort: true, fs: { allow: [root] } },
});
await server.listen();
console.log('Synthetic local preview: http://127.0.0.1:5199/social-preview');
