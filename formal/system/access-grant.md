# Access grant boundary (AUTH-ACCESS-PROVISION-001, AUTH-ARTIST-INVITE-001)

Status: bounded model and PostgreSQL regression added 2026-10-09 on base
`1e2d1307a940a8321a929a68879fcf86069a5355`, Mobile `810c91c5b5ec293c9e011fbec0f813bb62f8b17a`.

## Scope

The code-owned paths that grant the `Artist` security role:

1. **Reviewed feature access requests** (`TDF.Server.accessRequestsServer`).
   `registryFeatureAccessGrantRole` maps only `artist.onboarding:create` to
   `Artist`; every other approval is manual provisioning.
2. **Self-service activation**. Since 2026-09-16, `createRequest` for
   `artist.onboarding:create` calls `activateOwnArtistProfile`. That applies the
   `artist.self-service.artist` policy and records the request as `approved` by
   `system-policy`. No reviewer is involved.
3. **Personal invitation links** (`TDF.ServerAuth.redeemArtistInvitation`,
   `artistInvitationsServer`), which apply the `artist.invitation.artist` policy.

All automatic paths go through `applySecurityRoleAssignmentPolicy`, which refuses
to reactivate a revoked assignment.
Reviewed approval is different. `provisionReviewedSecurityRole` restores an
existing revoked assignment: it sets `active` and clears `revoked_at`. That is
the deliberate restoration route: a distinct administrator decides, and the
change is published as a security revision with an audit event. The model
separates the two paths. It checks that no *automatic* path reactivates a
revoked assignment, and it allows reviewed restoration (review finding on #529).

## Authority correction (AUTHORITY-054)

The previous statement said only an administrator distinct from the requester
may approve `artist.onboarding:create`. Since the self-service decision, new
requests of that kind never reach a reviewer. The reviewed path remains for
pending rows created before 2026-09-16. Self-service is the approved product
decision, so the requirement was corrected; the implementation was not changed
to match the old text. The distinct-administrator rule still governs every
reviewed decision, and the model checks it.

## Defect repaired

Request expiry is materialized lazily by `expireFeatureAccessRequests`. The
sweep ran only from the list endpoints. `decideRequest` and `cancelRequest`
matched `status = 'pending'` without checking `expires_at`. A reviewer could
therefore approve, or the requester cancel, a request whose detail view already
reported `expired`. Grant-bearing approval would provision a role from a lapsed
request.

`transitionPendingAccessRequest` now runs the sweep and the conditional UPDATE
in one transaction. A lapsed request becomes `expired`, with its history and
audit rows, and the decision or cancellation returns the existing 409.
The decision transaction reports "no longer pending" as a value, not an
exception. Throwing inside the transaction rolled back the sweep, leaving the
lapsed request `pending` (review finding on #529). The 409 is now raised after
the commit. The sweep inside the decision or cancellation is scoped to the target
request (`expirePendingAccessRequests` with an id filter). A provisioning failure
still throws and rolls back, so a global sweep there would also undo the expiry of
unrelated requests (second review finding). The list endpoints keep the global
sweep in their own transactions. Cancellation already raised its 409 after `runDB` returned.

`ArtistActivationSpec` runs against disposable PostgreSQL through
`scripts/test-artist-self-service.sh`. It covers the helper ("settles expiry
before deciding or cancelling …") and the real `decideRequest` handler
("commits expiry when a reviewer decides a lapsed request …"), asserting the
409, the persisted `expired` status, the history row and the absence of any grant.

Self-service activation follows the same rule (third review finding).
`activateOwnArtistProfile` completes only pending onboarding requests that have
not passed `expires_at`. `createArtistAccess` first settles that requester's
lapsed onboarding request, then records a fresh self-service approval instead of
resurrecting the expired row. Activation itself still succeeds.

## Bounded model

`formal/event-operations/AccessGrant.tla` covers one access request and one
invitation link over the parties requester, admin, manager and outsider. The
requester is itself an administrator, so the distinct-reviewer guard is not
masked by the role guard. 212 distinct states; no deadlock check (terminal
states are intended).

| Invariant | Implementation guard | Mutation config |
|---|---|---|
| `NoSelfApproval` | `decideRequest` rejects the requester (403) | `AccessGrantSelfApproval.cfg` |
| `AdminApprovesGrant` | `registryReviewerCanDecide` requires Admin for grant-bearing pairs | `AccessGrantNonAdmin.cfg` |
| `NoApprovalAfterExpiry` | `transitionPendingAccessRequest` | `AccessGrantExpired.cfg` |
| `SingleRedeemer` | claim `WHERE redeemed_by_party_id IS NULL OR = p` | `AccessGrantRebind.cfg` |
| `RedeemedNeverRevoked` | claim `revoked_at IS NULL`; revoke `redeemed_at IS NULL`; CHECK `artist_invitation_link_redeemed_or_revoked` | `AccessGrantRevokedLink.cfg` |
| `InvitationNeverAdministrative` | `artist.invitation.artist` binds the Artist role only | `AccessGrantAdminInvite.cfg` |
| `NoAutomaticReactivation` | `applySecurityRoleAssignmentPolicy` refuses reactivation (automatic paths only) | `AccessGrantReactivate.cfg` |

Each mutation disables exactly one guard, and `scripts/verify-event-operations-formal.sh`
requires its named invariant violation.

## Assumptions and exclusions

- Each action is atomic. This corresponds to one PostgreSQL transaction whose
  conditional UPDATE re-evaluates its predicate after a row lock under READ
  COMMITTED. The model does not check that correspondence.
- Session freshness is excluded: the reviewer's roles come from the
  authenticated session. Session revocation is covered by the identity
  lifecycle contract.
- Only one request and one link are modeled. Interaction between many
  requests, the self-service path and invitation attribution is not modeled.
- Re-redemption by the bound account within the link's validity re-runs the
  policy. That is idempotent while the assignment is active and refused once
  it is revoked (`NoAutomaticReactivation`).
- This is bounded model checking under these assumptions, not a proof of the
  Haskell implementation.
