# Scope-decision matrix

The complete matrix is generated in two equivalent formats:

- `studio-feature-inventory.csv` for review and filtering.
- `test/internships/studio-audit/studio-feature-inventory.json` for validation and import.

## Decision rules

`direct_studio_management` covers a capability whose primary subject is studio operation. `required_dependency_of_studio_workflow` covers shared infrastructure without which a studio flow cannot complete. `shared_with_other_business_area` covers Domo, school, DDEX, CRM, commerce, or other shared behavior only to the extent that it affects studio people, rooms, resources, reservations, sessions, recordings, rights data, or money. `out_of_scope` excludes production mutation, real communications or charges, music distribution, destructive/load/security testing, and unrelated business functionality.

Every row independently records whether implementation is accessible, inaccessible for the intended role, partial, documented-only, undocumented implementation, and whether it is native mobile, responsive mobile fallback, web-only, or unavailable on mobile. A classification is evidence-based: the `evidence` list points to routes, handlers, models, registry data, tests, or documentation that support it.

## Coverage summary

| Measure | Count |
| --- | ---: |
| Inventory entries | 130 |
| Applicable entries | 125 |
| Generated test cases | 174 |
| Exploratory charters | 14 |
| Cases requiring strong evidence | 107 |
| Estimated scripted/exploratory execution | 23.4 hours |

The matrix must be regenerated after feature-registry, route, role, or mobile changes and reviewed before activation. A feature that Stewart cannot find is still an executable result, not an automatic exclusion.
