import { expect, test } from '@playwright/test';
import axe from 'axe-core';

// Production-bundle navigation, synthetic API only. No provider or production writes.
const thread={ctThreadId:10,ctOtherPartyId:2,ctOtherDisplayName:'Private peer',ctLastMessage:'Private preview',ctLastMessageAt:'2026-09-15T12:00:00Z',ctUpdatedAt:'2026-09-15T12:00:00Z'};
const other={...thread,ctThreadId:20,ctOtherPartyId:3,ctOtherDisplayName:'Other peer',ctLastMessage:'Other preview'};
const notification={nId:8,nType:'chat_message',nTitle:'Chat notification',nBody:'New message',nTargetType:'chat_thread',nTargetId:null,nTargetKey:'10',nIsRead:false,nCreatedAt:'2026-09-15T12:00:00Z'};

for (const { allowed, incoming, navigation } of [{ allowed: true }, { allowed: false }, { allowed: true, incoming: true }, { allowed: true, navigation: true }]) {
  test(navigation ? 'explicit chat navigation reveals the selected conversation below a full list @critical' : incoming ? 'incoming chat messages keep the conversation-list pointer target stable and retain drafts @critical' : `chat notification reauthorizes same-route ${allowed ? 'allowed' : 'revoked'} access and preserves drafts @critical`, async ({ page, baseURL }, testInfo) => {
    // This counterexample targets the fixed-height desktop message viewport.
    // The two original cases retain every project's configured device viewport.
    if (incoming) await page.setViewportSize({ width: 1280, height: 720 });
    const base=new URL(baseURL).origin;
    const context=page.context();let phase='initial',rows=[thread,other],pending=[],lateMessages=[],holdMessageRefresh=false;
    if (navigation) rows.push(...Array.from({ length: 8 }, (_, i) => ({ ...other, ctThreadId: 30 + i, ctOtherPartyId: 30 + i, ctOtherDisplayName: `Peer ${i}`, ctLastMessage: `Preview ${i}` })));
    const messages=[], errors=[];page.on('pageerror',e=>errors.push(e.message));
    await context.route('**/*',async route=>{
      const req=route.request(),url=new URL(req.url()),path=url.pathname;
      if(!['fetch','xhr'].includes(req.resourceType()))return url.origin===base?route.continue():route.abort();
      if(path==='/session')return route.fulfill({json:{username:'synthetic-reviewer',displayName:'Reviewer',partyId:5,roles:['Admin'],modules:['Catalog','CRM','Admin'],featureFlags:[]}});
      if(path==='/session/preferences')return route.fulfill({json:{localeId:'',locale:'es',currencyId:'',currency:'USD',timezone:'America/Guayaquil',countryId:null,countryCode:'EC'}});
      if(path==='/health')return route.fulfill({json:{status:'ok'}});
      if(path==='/session/onboarding')return route.fulfill({json:{eligible:false,completedAt:'2026-09-15T12:00:00Z'}});
      if(path==='/fans/me/notifications/count')return route.fulfill({json:{ncUnread:1}});
      if(path==='/fans/me/notifications')return route.fulfill({json:[notification]});
      if(path==='/fans/me/notifications/8')return route.fulfill({json:notification});
      if(path==='/fans/me/notifications/8/read')return route.fulfill({status:204});
      if(path==='/chat/threads'){
        if(phase==='pending')return new Promise(resolve=>pending.push(async list=>{await route.fulfill({json:list});resolve();}));
        return route.fulfill({json:rows});
      }
      const match=path.match(/^\/chat\/threads\/(10|20)\/messages$/);
      if(match){const id=Number(match[1]);messages.push(id);const body={cmId:id,cmThreadId:id,cmSenderPartyId:id===10?2:3,cmBody:id===10?'Private body':'Other body',cmCreatedAt:'2026-09-15T12:00:00Z'};
        if(id===10&&holdMessageRefresh){holdMessageRefresh=false;return new Promise(resolve=>lateMessages.push(async()=>{await route.fulfill({json:[body,{...body,cmId:11,cmBody:'Incoming while selecting: '+('A real message response. '.repeat(80))}]});resolve();}));}
        return route.fulfill({json:[body]});}
      if(path.startsWith('/social/profiles/'))return route.fulfill({json:{sppPartyId:Number(path.split('/').at(-1)),sppDisplayName:'Peer',sppBio:'',sppAvatarUrl:null}});
      if(path.startsWith('/radio/presence'))return route.fulfill({json:null});
      if(path.endsWith('/rsvp-feed'))return route.fulfill({json:{feedItems:[],feedNextCursor:null}});
      if(path.startsWith('/catalog'))return route.fulfill({json:{catalogs:[],items:[],defaults:[]}});
      return route.fulfill({json:[]});
    });
    const expectSelectedMessageInView=async(text)=>{
      const selected=page.getByText(text,{exact:true});
      await expect.poll(()=>selected.evaluate(node=>{
        const rect=node.getBoundingClientRect(),main=document.getElementById('main-content').getBoundingClientRect();
        return rect.top>=Math.max(0,main.top)&&rect.bottom<=Math.min(innerHeight,main.bottom);
      })).toBe(true);
    };
    try{
      await page.goto(base+'/chat');await expect(page.getByText('Private body',{exact:true})).toBeVisible();
      await page.getByText('Other preview',{exact:true}).click();await expect(page.getByText('Other body',{exact:true})).toBeVisible();
      if (navigation) await expectSelectedMessageInView('Other body');
      await page.getByRole('textbox',{name:'Escribe un mensaje'}).fill('Keep this draft');
      const before=await page.evaluate(()=>localStorage.getItem('tdf-chat-last-seen-v1:party:5'));
      const targetRequests=messages.filter(id=>id===10).length;
      await page.evaluate(()=>{window.syntheticDocumentIdentity='same-document';});
      phase='pending';
      await page.getByRole('button',{name:'Notificaciones',exact:true}).click();
      await page.getByRole('link',{name:/Chat notification/}).click();
      await expect.poll(()=>pending.length,{timeout:3000}).toBeGreaterThan(0);
      await expect(page.locator('.MuiPopover-root')).toHaveCount(0);
      await expect(page).toHaveURL(/\/chat\?threadId=10$/);
      await expect(page.getByText('Private body',{exact:true})).toHaveCount(0);
      await expect(page.getByText('Private preview',{exact:true})).toHaveCount(0);
      expect(messages.filter(id=>id===10).length).toBe(targetRequests);
      expect(await page.evaluate(()=>localStorage.getItem('tdf-chat-last-seen-v1:party:5'))).toBe(before);
      rows=allowed?[thread,other]:[other];phase='resolved';holdMessageRefresh=incoming;
      await Promise.all(pending.splice(0).map(release=>release(rows)));
      if(allowed){await expect(page.getByText('Private body',{exact:true})).toBeVisible();if(navigation)await expectSelectedMessageInView('Private body');}
      else{
        await expect(page.getByText('La conversación de esta notificación no está disponible.')).toBeVisible();
        await expect(page.getByText('Private body',{exact:true})).toHaveCount(0);
        await expect(page.getByText('Private preview',{exact:true})).toHaveCount(0);
        expect(messages.filter(id=>id===10).length).toBe(targetRequests);
      }
      expect(await page.evaluate(()=>window.syntheticDocumentIdentity)).toBe('same-document');
      if (incoming) {
        await expect.poll(()=>lateMessages.length,{timeout:5000}).toBeGreaterThan(0);
        await page.getByRole('textbox',{name:'Escribe un mensaje'}).focus();
        const otherPreview=page.getByText('Other preview',{exact:true});
        await otherPreview.scrollIntoViewIfNeeded();
        const mainBefore=await page.evaluate(()=>document.getElementById('main-content').scrollTop);
        const point=await otherPreview.boundingBox();
        expect(point).not.toBeNull();
        await page.mouse.move(point.x+point.width/2,point.y+point.height/2);
        await page.mouse.down();
        try {
          await lateMessages.shift()();
          // Observe the actual overflow container; never move MAIN or a target
          // in this fixture. The production message-arrival effect does the scrolling.
          const scrollState=()=>page.getByText('Private body',{exact:true}).evaluate(node=>{
            let viewport=node.parentElement;
            while(viewport&&viewport.id!=='main-content'&&getComputedStyle(viewport).overflowY!=='auto')viewport=viewport.parentElement;
            return {main:document.getElementById('main-content').scrollTop,top:viewport?.scrollTop??0,height:viewport?.clientHeight??0,total:viewport?.scrollHeight??0};
          });
          await expect.poll(async()=>{const state=await scrollState();return state.total>state.height&&state.top>0&&state.top+state.height>=state.total-2;}).toBe(true);
          const during=await scrollState();
          await testInfo.attach('chat-message-scroll-boundary.json',{body:JSON.stringify({mainBefore,during}),contentType:'application/json'});
          expect(during.main).toBe(mainBefore);
        } finally {await page.mouse.up();}
        await expect(page.getByText('Other body',{exact:true})).toBeVisible();
      } else await page.getByText('Other preview',{exact:true}).click();
      if (navigation) await expectSelectedMessageInView('Other body');
      await expect(page.getByRole('textbox',{name:'Escribe un mensaje'})).toHaveValue('Keep this draft');
      await page.addScriptTag({content:axe.source});
      const violations=await page.evaluate(async()=> (await window.axe.run(document.querySelector('main')??document.body)).violations.filter(v=>['serious','critical'].includes(v.impact)).map(v=>({id:v.id,nodes:v.nodes.map(n=>({target:n.target,html:n.html,summary:n.failureSummary}))})));
      expect(violations).toEqual([]);expect(errors).toEqual([]);
    }finally{phase='resolved';await Promise.all(pending.splice(0).map(release=>release([])));await Promise.all(lateMessages.splice(0).map(release=>release()));}
  });
}
