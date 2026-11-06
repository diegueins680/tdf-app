import base from './playwright.config.mjs';
import {defineConfig} from '/private/tmp/tdf-correctness-cycle-20261028/node_modules/@playwright/test/index.mjs';
export default defineConfig({...base,use:{...base.use,baseURL:'http://127.0.0.1:41212'},projects:base.projects.filter(p=>p.name==='chromium-phone'),reporter:[['line'],['json',{outputFile:'/private/tmp/tdf-booking-resize-observation-final-matrix-20261029/final-chromium-phone.json'}]],outputDir:'/private/tmp/tdf-booking-resize-observation-final-matrix-20261029/final-chromium-phone'});
