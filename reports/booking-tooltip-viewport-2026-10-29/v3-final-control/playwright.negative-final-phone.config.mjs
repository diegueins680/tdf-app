import base from './playwright.config.mjs';
import {defineConfig} from '/private/tmp/tdf-correctness-cycle-20261028/node_modules/@playwright/test/index.mjs';
export default defineConfig({...base,projects:base.projects.filter(p=>p.name==='chromium-phone'),reporter:[['line'],['json',{outputFile:'/private/tmp/tdf-booking-tooltip-regression-qualified-v3-20261028/negative-final-phone.json'}]],outputDir:'/private/tmp/tdf-booking-tooltip-regression-qualified-v3-20261028/negative-final-phone'});
