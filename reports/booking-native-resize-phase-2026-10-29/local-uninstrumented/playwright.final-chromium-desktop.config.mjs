import base from './playwright.config.mjs';
import {defineConfig} from '/private/tmp/tdf-correctness-cycle-20261028/node_modules/@playwright/test/index.mjs';
export default defineConfig({...base,use:{...base.use,baseURL:'http://127.0.0.1:41331'},projects:base.projects.filter(p=>p.name==='chromium-desktop'),reporter:[['line'],['json',{outputFile:'/private/tmp/tdf-booking-resize-phase-uninstrumented-matrix-20261029/final-chromium-desktop.json'}]],outputDir:'/private/tmp/tdf-booking-resize-phase-uninstrumented-matrix-20261029/final-chromium-desktop'});
