# Form commit actions

Status: accepted by product owner on 2026-11-10.

## UX-FORM-ACTIONS-001

Editable web forms place Save, Update, or the equivalent commit action after the last field, including optional fields when expanded. Finishing the form must allow the user to continue to its commit action without scrolling back to the header.

An open course registration notes, receipt, or follow-up editor and the CMS editor keep the commit action visible. Existing validation, loading, unchanged-content, catalog, account-ownership, and in-flight restrictions determine whether it is enabled. Collapsed editors retain their existing open and close controls.

The artist editor places its update action within the editable field group after Highlights. The manual-payment form places Register payment after its optional receipt URL. Keyboard focus proceeds from each final field to its commit action.

The existing save handlers and API contracts govern persistence and recovery. Browser fixtures verify layout and submission at 390px and 1440px. Targeted component tests cover readiness restrictions, edits, optional composers, and recovery. These checks establish the exercised flows and viewport sizes.
