import base from './playwright.config.mjs';
import {defineConfig} from '/private/tmp/tdf-correctness-cycle-20261028/node_modules/@playwright/test/index.mjs';
export default defineConfig({...base,use:{...base.use,baseURL:'http://127.0.0.1:41093'},projects:base.projects.filter(p=>p.name==='firefox'),reporter:[['line'],['json',{outputFile:'/private/tmp/tdf-booking-tooltip-regression-final-c2-20261028/final-firefox.json'}]],outputDir:'/private/tmp/tdf-booking-tooltip-regression-final-c2-20261028/final-firefox'});
