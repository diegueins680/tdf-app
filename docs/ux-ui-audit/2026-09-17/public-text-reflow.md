# Public enlarged-text reflow — UX-260917-045

A real Firefox default-font change from16 to32px doubles the measured landing
heading35.2→70.4px and login14→28px, but the original320px page grows to531px.
The menu moves beyond the viewport and long heading words leave their background.
A separate computed-font stress sweep of42 public routes in ES/EN finds60 overflows
among84 cases. This stress technique is explicitly separate from native browser zoom.

PublicBranding now wraps header groups, long words and labels while keeping the
actual enlarged font. Chips grow vertically. The existing scrollable tab strip is
retained; no page overflow is hidden. Marketplace's sort Select is bounded by its
container and both selected value/options wrap. API calls, cart, permissions,
translations and branding assets are unchanged.

The completed production bundle (365727 gzip bytes/five preloads) passes84 final
actual-isolated-API Chromium cases: no axe violations, page errors or horizontal
page overflow. Actual Firefox font16/32 passes at320/768/1280px. Forty focused
ES/EN cases in three engines/five device profiles at320px verify visible navigation,
keyboard activation and restored menu focus;6existing component tests pass.
The first marketplace E2E assumed h1, whereas its existing title is h4; this harness
assumption was corrected without changing the product heading to make a test pass.
The initial preview also raced a rebuild; final runs serve an immutable completed
bundle. Failed/partial runs are not counted. See evidence/public-text-reflow.json.

Primary guidance checked18September: [WCAG text resizing](https://www.w3.org/WAI/WCAG22/Understanding/resize-text.html)
and [reflow](https://www.w3.org/WAI/WCAG22/Understanding/reflow.html). Results apply to
these mechanisms/routes, not complete WCAG conformance, human screen readers, all
private states or representative field performance. There is no changed security,
payment or asynchronous persistence transition requiring a new formal state model.
Review, applicable CI and production deployment/verification remain open.
