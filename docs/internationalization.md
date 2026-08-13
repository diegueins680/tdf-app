# Internationalization rollout

The platform stores timestamps as UTC and resolves presentation settings from a user's locale preferences. Authenticated web and mobile clients use `/session/preferences`; anonymous web users begin with browser language/timezone detection and local persistence.

## Runtime configuration

```env
DEFAULT_CURRENCY=USD
SUPPORTED_CURRENCIES=USD,EUR,GBP,CAD,AUD,JPY,BRL
DEFAULT_TIMEZONE=UTC
SUPPORTED_LOCALES=en,es,fr,de,pt
DEFAULT_LOCALE=en
ENABLE_GDPR_COMPLIANCE=true
```

The web client mirrors the allowlists with `VITE_DEFAULT_*` and `VITE_SUPPORTED_*`. `VITE_EXCHANGE_RATE_API_BASE` defaults to `https://api.frankfurter.dev/v1`; rates are cached for 24 hours. Venue-specific campaign pages may set their own timezone, for example `VITE_DOMO_TIMEZONE`.

## Database rollout

Apply [`tdf-hq/sql/2026-08-05_platform_internationalization.sql`](../tdf-hq/sql/2026-08-05_platform_internationalization.sql) before deploying application code when migrations are managed externally. With `RUN_MIGRATIONS=true`, Persistent creates the runtime preference, currency, payment-currency, and conversion-audit schema and the boot migration adds geographic columns.

Accounts created before 2026-08-05 are backfilled to `es`, `USD`, `America/Guayaquil`, and `EC` to preserve their prior presentation. New accounts use configured defaults until they save an override.

## Money and exchange rates

- Monetary records keep their original ISO 4217 currency; formatting uses the selected locale and the currency's CLDR fraction digits.
- Conversion is opt-in and never silently changes stored transaction amounts.
- Authenticated conversions are written to `currency_conversion_audit` with source/target minor units, rate source, user, and timestamps.
- Payment and catalog APIs reject codes outside both ISO 4217 and `SUPPORTED_CURRENCIES`.

## Content versus platform defaults

Geographic references that describe a real venue, artist, or campaign remain content. They must not be reused as platform defaults. Generic calendars, profiles, phone inputs, payments, logistics, and navigation resolve region settings dynamically.

## Translation rollout boundary

The shared shell, session controls, preference settings, system navigation, and the internationalized booking/payment/calendar surfaces use translation keys for `en`, `es`, `fr`, `de`, and `pt`. English and Spanish are the maintained source locales; French, German, and Portuguese currently provide the regional-settings and shared-shell vocabulary and fall back to English for keys that have not yet been translated.

Legacy product descriptions, editorial pages, campaign copy, and internal report narratives remain authored content. They are deliberately not machine-translated during this migration. New reusable interface text must be added to the locale resources rather than embedded in components; authored market-specific content should carry its own content locale when it is moved into the CMS.
