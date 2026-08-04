# DDEX Module for TDF HQ

## Overview
This module implements the DDEX Gateway and Canonical Catalog for TDF Records. It separates the exchange format (DDEX XML) from the internal data model (PostgreSQL), allowing for stable evolution of the catalog regardless of DDEX version changes.

## Architecture
- **Gateway**: Handles ingestion, validation (XSD/AVS), and parsing of DDEX messages (ERN, RIN, DSR).
- **Catalog**: Stores normalized data (Releases, Resources, Parties, Deals).
- **Security**: Sandboxed XML parsing, XXE protection, and private asset storage.

## Current Status
- **Phase 1 (Foundations)**: ✅ Complete
  - Database Schema
  - API Definitions
  - Security Types
  - Auth Integration

## Next Steps
- Implement ERN 4.3.2 Parser
- Build React Inbox UI
- Configure `xmllint` in Docker

## License
Internal TDF Records Use Only. DDEX Schemas are copyrighted by DDEX Consortium.
