import { jest } from '@jest/globals';
import { act, StrictMode } from 'react';
import { createRoot } from 'react-dom/client';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { fireEvent } from '@testing-library/react';
import { MemoryRouter, useLocation } from 'react-router-dom';

const initialAuthority = {username:'calendar-test',displayName:'Calendar test',roles:['admin'],partyId:1};
let authority = initialAuthority;
let activeAuthority: typeof initialAuthority | null = authority;
jest.unstable_mockModule('../session/SessionContext', () => ({ useSession: () => ({session:authority}), getActiveSession: () => activeAuthority }));
const getConfig = jest.fn<() => Promise<null | {configId: number; calendarId: string}>>(async () => null);
const exchangeCode = jest.fn<() => Promise<{configId: number; calendarId: string}>>(async () => ({configId: 1, calendarId: 'primary'}));
const sync = jest.fn<() => Promise<{created:number;updated:number;deleted:number}>>(async()=>({created:0,updated:0,deleted:0}));
const listEvents = jest.fn(async () => []);
jest.unstable_mockModule('../api/calendar', () => ({ CalendarApi: {
  getConfig, listEvents, getAuthUrl: jest.fn(), exchangeCode, sync,
} }));
jest.unstable_mockModule('../contexts/LocalePreferencesContext', () => ({
  useLocalePreferences: () => ({ locale: 'es', timezone: 'America/Guayaquil' }),
}));
const { default: CalendarSyncPage } = await import('./CalendarSyncPage');
(globalThis as typeof globalThis & { IS_REACT_ACT_ENVIRONMENT: boolean }).IS_REACT_ACT_ENVIRONMENT = true;

const pause = () => new Promise(resolve => setTimeout(resolve, 0));
beforeEach(() => { authority = initialAuthority; activeAuthority = authority; });

describe('CalendarSyncPage optional browser preferences', () => {
  afterEach(() => jest.restoreAllMocks());
  it.each(['getItem', 'setItem', 'removeItem', 'getter'] as const)(
    'keeps the real form usable when storage %s fails', async (failure) => {
      window.localStorage.clear();
      getConfig.mockClear(); listEvents.mockClear();
      const unavailable = () => { throw new DOMException('Storage unavailable', 'SecurityError'); };
      if (failure === 'getter') jest.spyOn(window, 'localStorage', 'get').mockImplementation(unavailable);
      else jest.spyOn(Storage.prototype, failure).mockImplementation(unavailable);
      const container = document.createElement('div'); document.body.append(container);
      const root = createRoot(container);
      const client = new QueryClient({ defaultOptions: { queries: { retry: false, gcTime: 0 } } });
      try {
        await act(async () => {
          root.render(<MemoryRouter><QueryClientProvider client={client}><CalendarSyncPage /></QueryClientProvider></MemoryRouter>);
          await pause();
        });
        await act(async () => { await pause(); });
        expect(container.textContent).toContain('Integración Google Calendar');
        expect(getConfig).toHaveBeenCalled();
        expect(listEvents).toHaveBeenCalled();
        const clear = [...container.querySelectorAll('button')].find(button => button.textContent?.includes('Limpiar preferencias locales'));
        expect(clear).toBeDefined();
        await act(async () => { clear?.click(); await pause(); });
        expect(container.textContent).toContain('Integración Google Calendar');
        expect(container.querySelector<HTMLInputElement>('input[role="combobox"]')?.value).toBe('primary');
      } finally {
        await act(async () => root.unmount()); client.clear(); container.remove();
      }
    },
  );
});


describe('CalendarSyncPage authoritative connection', () => {
  afterEach(() => jest.restoreAllMocks());
  it.each(['stale-marker', 'oauth-return'] as const)('rejects %s without replay or invented persistence', async scenario => {
    window.localStorage.clear(); getConfig.mockReset().mockResolvedValue(null); exchangeCode.mockReset().mockResolvedValueOnce({configId: 1, calendarId: 'primary'}).mockImplementation(() => new Promise(() => {}));
    if (scenario === 'stale-marker') window.localStorage.setItem('calendar-sync.connected', 'old-account');
    const container = document.createElement('div'); document.body.append(container);
    const root = createRoot(container); const client = new QueryClient({defaultOptions:{queries:{retry:false,gcTime:0}}});
    try {
      await act(async () => { root.render(<MemoryRouter initialEntries={[scenario === 'oauth-return' ? '/?code=synthetic-one-use-code' : '/']}><QueryClientProvider client={client}><CalendarSyncPage /></QueryClientProvider></MemoryRouter>); await pause(); });
      for (let i=0;i<8;i++) await act(async () => {await pause();});
      if(scenario === 'stale-marker') expect(container.textContent).not.toContain('Configuración guardada para old-account');
      else expect(exchangeCode).toHaveBeenCalledTimes(1);
    } finally { await act(async()=>root.unmount());client.clear();container.remove(); }
  });
});

function LocationProbe() { const location = useLocation(); return <output data-testid="location">{location.pathname}{location.search}{location.hash}</output>; }
const deferred = <T,>() => { let resolve!: (v:T)=>void; const promise = new Promise<T>(r=>{resolve=r;}); return {promise,resolve}; };

describe('CalendarSyncPage return and session authority', () => {
  afterEach(() => jest.restoreAllMocks());
  const setup = async (entry = '/configuracion/integraciones/calendario') => {
    window.localStorage.clear();
    const container=document.createElement('div');document.body.append(container); const root=createRoot(container);
    const client=new QueryClient({defaultOptions:{queries:{retry:false,gcTime:0}}});
    const render=async()=>{await act(async()=>{root.render(<StrictMode><MemoryRouter initialEntries={[entry]}><QueryClientProvider client={client}><CalendarSyncPage/><LocationProbe/></QueryClientProvider></MemoryRouter></StrictMode>);await pause();});await act(async()=>{await pause();});};
    await render();
    return {container,client,render,close:async()=>{await act(async()=>root.unmount());client.clear();container.remove();}};
  };
  const button=(container:HTMLElement,name:string)=> {const found=[...container.querySelectorAll('button')].find(b=>b.textContent?.trim()===name);expect(found).toBeDefined();return found!;};
  it('consumes a failed callback once, preserves destination/input, and allows only explicit retry',async()=>{
    getConfig.mockReset().mockResolvedValue(null);exchangeCode.mockReset().mockRejectedValue(new Error('Synthetic exchange failure'));
    const ui=await setup('/configuracion/integraciones/calendario?code=one-use&calendarId=primary&keep=yes#return');
    try {
      for(let i=0;i<5;i++)await act(async()=>{await pause();});
      expect(exchangeCode).toHaveBeenCalledTimes(1);
      expect(ui.container.querySelector('[data-testid="location"]')?.textContent).toBe('/configuracion/integraciones/calendario?calendarId=primary&keep=yes#return');
      expect(ui.container.querySelector<HTMLInputElement>('input[value="one-use"]')?.value).toBe('one-use');
      expect(ui.container.textContent).not.toContain('Conexión guardada en TDF.');
      expect(button(ui.container,'Guardar tokens').disabled).toBe(false);
      await act(async()=>{button(ui.container,'Guardar tokens').click();button(ui.container,'Guardar tokens').click();await pause();});
      expect(exchangeCode).toHaveBeenCalledTimes(2);
    }finally{await ui.close();}
  });
  it.each(['logout-before-render','A-B-A'] as const)('ignores a token-exchange response after %s',async(mode)=>{
    getConfig.mockReset().mockResolvedValue(null);const response=deferred<{configId:number;calendarId:string}>();exchangeCode.mockReset().mockImplementation(()=>response.promise);
    const ui=await setup('/configuracion/integraciones/calendario?code=one-use&calendarId=primary');
    try {
      expect(exchangeCode).toHaveBeenCalledTimes(1);
      if(mode==='A-B-A'){authority={...initialAuthority,partyId:2};activeAuthority=authority;await ui.render();authority={...initialAuthority};activeAuthority=authority;await ui.render();}
      else activeAuthority=null;
      await act(async()=>{response.resolve({configId:1,calendarId:'primary'});await pause();});
      await act(async()=>{await pause();});
      expect(ui.container.textContent).not.toContain('Configuración guardada para primary');
      expect(ui.container.textContent).not.toContain('Conexión guardada en TDF.');
      expect(ui.client.getQueryCache().getAll().some(q=>q.queryKey[0]==='calendar-config' && q.state.data!==null && q.state.data!==undefined)).toBe(false);
    }finally{await ui.close();}
  });
  it('queries the selected calendar and clears a saved-connection claim after a failed refresh',async()=>{
    getConfig.mockReset().mockResolvedValue({configId:1,calendarId:'primary'});
    const ui=await setup();
    try {
      expect(getConfig).toHaveBeenCalledWith('primary');
      expect(ui.container.textContent).toContain('Configuración guardada para primary');
      getConfig.mockRejectedValue(new Error('Synthetic offline'));
      await act(async()=>{button(ui.container,'Actualizar configuración').click();await pause();});
      await act(async()=>{await pause();});
      expect(ui.container.textContent).not.toContain('Configuración guardada para primary');
      expect(ui.container.textContent).not.toContain('Conexión verificada con el proveedor');
      expect(button(ui.container,'Sincronizar ahora').disabled).toBe(true);
    }finally{await ui.close();}
  });
  it('clears only local preferences without claiming a server disconnect',async()=>{
    getConfig.mockReset().mockResolvedValue({configId:1,calendarId:'primary'});
    const ui=await setup();
    try {
      await act(async()=>{button(ui.container,'Limpiar preferencias locales').click();await pause();});
      expect(ui.container.textContent).toContain('Configuración guardada para primary');
      expect(ui.container.textContent).toContain('La conexión guardada permanece en TDF.');
      expect(ui.container.textContent).not.toContain('Desconectar y limpiar');
    }finally{await ui.close();}
  });
  it('does not reuse another selected calendar configuration',async()=>{
    getConfig.mockReset().mockResolvedValueOnce({configId:1,calendarId:'primary'}).mockResolvedValue(null);
    const ui=await setup();
    try {
      const input=ui.container.querySelector<HTMLInputElement>('input[role="combobox"]')!;
      await act(async()=>{fireEvent.change(input,{target:{value:'other-calendar'}});await pause();});
      await act(async()=>{await pause();});
      expect(getConfig).toHaveBeenCalledWith('other-calendar');
      expect(ui.container.textContent).not.toContain('Configuración guardada para primary');
      expect(button(ui.container,'Sincronizar ahora').disabled).toBe(true);
    }finally{await ui.close();}
  });
  it('does not let an older config fetch erase a persisted exchange receipt',async()=>{
    const config=deferred<null>();getConfig.mockReset().mockImplementation(()=>config.promise);
    exchangeCode.mockReset().mockResolvedValue({configId:1,calendarId:'primary'});
    const ui=await setup('/configuracion/integraciones/calendario?code=one-use&calendarId=primary');
    try {
      for(let i=0;i<5;i++)await act(async()=>{await pause();});
      expect(ui.container.textContent).toContain('Configuración guardada para primary');
      await act(async()=>{config.resolve(null);await pause();});
      expect(ui.container.textContent).toContain('Configuración guardada para primary');
    }finally{await ui.close();}
  });
  it('sends one concurrent sync and waits for the server timestamp',async()=>{
    getConfig.mockReset().mockResolvedValue({configId:1,calendarId:'primary'});
    const response=deferred<{created:number;updated:number;deleted:number}>();sync.mockReset().mockImplementation(()=>response.promise);
    const ui=await setup();
    try {
      await act(async()=>{button(ui.container,'Sincronizar ahora').click();button(ui.container,'Sincronizar ahora').click();await pause();});
      expect(sync).toHaveBeenCalledTimes(1);
      await act(async()=>{response.resolve({created:1,updated:0,deleted:0});await pause();});
      await act(async()=>{await pause();});
      expect(ui.container.textContent).toContain('Última sync: Sin sincronizar');
      expect(window.localStorage.getItem('calendar-sync.lastSyncAt')).toBeNull();
    }finally{await ui.close();}
  });

});
