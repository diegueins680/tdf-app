---- MODULE AccessGrant ----
\* Bounded model of the two code-owned paths that grant the Artist security role
\* (AUTH-ACCESS-PROVISION-001, AUTH-ARTIST-INVITE-001): reviewed feature access
\* requests (TDF.Server accessRequestsServer) and personal invitation links
\* (TDF.ServerAuth redeemArtistInvitation / artistInvitationsServer), together with
\* the revoked-assignment rule in TDF.Catalog.Security.applySecurityRoleAssignmentPolicy.
\* Each CHECK_* constant is one guard in the implementation; the mutation
\* configurations disable exactly one guard and must produce a counterexample.
EXTENDS Naturals, FiniteSets, TLC

CONSTANTS CheckDistinctReviewer, CheckAdminReviewer, CheckExpiry,
          CheckLinkBinding, CheckLinkRevoked, CheckNoReactivation, InvitationRole

Parties == {"requester", "admin", "manager", "outsider"}
\* The requester is itself an administrator: the distinct-reviewer guard must
\* hold even when the role guard alone would admit the requester.
Admins == {"admin", "requester"}
None == "none"

VARIABLES
  reqStatus,      \* pending | approved | rejected | cancelled | expired
  reqPastDeadline,\* wall clock passed expires_at (expiry is materialized lazily)
  reqApprover,    \* reviewer that approved, or None
  reqApprovedLate,\* approval admitted after the deadline
  linkBoundTo,    \* redeemed_by_party_id, or None
  linkRevoked,    \* revoked_at IS NOT NULL
  linkPastDeadline,
  inviteGrants,   \* parties granted a role through the link, with the role
  roleActive,     \* party -> Artist assignment active
  roleRevoked,    \* party -> Artist assignment was revoked by staff
  reactivated     \* a revoked assignment became active again

vars == <<reqStatus, reqPastDeadline, reqApprover, reqApprovedLate, linkBoundTo,
          linkRevoked, linkPastDeadline, inviteGrants, roleActive, roleRevoked, reactivated>>

Init ==
  /\ reqStatus = "pending" /\ reqPastDeadline = FALSE
  /\ reqApprover = None /\ reqApprovedLate = FALSE
  /\ linkBoundTo = None /\ linkRevoked = FALSE /\ linkPastDeadline = FALSE
  /\ inviteGrants = {}
  /\ roleActive = [p \in Parties |-> FALSE]
  /\ roleRevoked = [p \in Parties |-> FALSE]
  /\ reactivated = FALSE

\* Assigning Artist: an existing revoked assignment is never reactivated.
Assign(p) ==
  IF roleRevoked[p] /\ CheckNoReactivation
    THEN UNCHANGED <<roleActive, reactivated>>
    ELSE /\ roleActive' = [roleActive EXCEPT ![p] = TRUE]
         /\ reactivated' = (reactivated \/ roleRevoked[p])

AccessDeadlinePasses ==
  /\ ~reqPastDeadline /\ reqPastDeadline' = TRUE
  /\ UNCHANGED <<reqStatus, reqApprover, reqApprovedLate, linkBoundTo, linkRevoked,
                 linkPastDeadline, inviteGrants, roleActive, roleRevoked, reactivated>>

\* expireFeatureAccessRequests: lazy sweep from list/review/decide/cancel paths.
ExpireSweep ==
  /\ reqStatus = "pending" /\ reqPastDeadline
  /\ reqStatus' = "expired"
  /\ UNCHANGED <<reqPastDeadline, reqApprover, reqApprovedLate, linkBoundTo, linkRevoked,
                 linkPastDeadline, inviteGrants, roleActive, roleRevoked, reactivated>>

\* decideRequest: the sweep runs inside the decision transaction, then the
\* conditional UPDATE ... WHERE status = 'pending' admits at most one decision.
Approve(r) ==
  /\ reqStatus = "pending"
  /\ (CheckExpiry => ~reqPastDeadline)
  /\ (CheckDistinctReviewer => r # "requester")
  /\ (CheckAdminReviewer => r \in Admins)
  /\ r # "outsider"   \* not a reviewer at all: rejected before the guard under test
  /\ reqStatus' = "approved"
  /\ reqApprover' = r
  /\ reqApprovedLate' = reqPastDeadline
  /\ Assign("requester")
  /\ UNCHANGED <<reqPastDeadline, linkBoundTo, linkRevoked, linkPastDeadline,
                 inviteGrants, roleRevoked>>

Reject(r) ==
  /\ reqStatus = "pending" /\ ~reqPastDeadline
  /\ r \in Admins /\ r # "requester"
  /\ reqStatus' = "rejected"
  /\ UNCHANGED <<reqPastDeadline, reqApprover, reqApprovedLate, linkBoundTo, linkRevoked,
                 linkPastDeadline, inviteGrants, roleActive, roleRevoked, reactivated>>

Cancel ==
  /\ reqStatus = "pending" /\ ~reqPastDeadline
  /\ reqStatus' = "cancelled"
  /\ UNCHANGED <<reqPastDeadline, reqApprover, reqApprovedLate, linkBoundTo, linkRevoked,
                 linkPastDeadline, inviteGrants, roleActive, roleRevoked, reactivated>>

LinkDeadlinePasses ==
  /\ ~linkPastDeadline /\ linkPastDeadline' = TRUE
  /\ UNCHANGED <<reqStatus, reqPastDeadline, reqApprover, reqApprovedLate, linkBoundTo,
                 linkRevoked, inviteGrants, roleActive, roleRevoked, reactivated>>

\* UPDATE artist_invitation_link SET redeemed_by_party_id = p ... WHERE
\* revoked_at IS NULL AND expires_at > now AND (redeemed_by IS NULL OR redeemed_by = p)
Redeem(p) ==
  /\ CheckLinkRevoked => ~linkRevoked
  /\ ~linkPastDeadline
  /\ CheckLinkBinding => linkBoundTo \in {None, p}
  /\ linkBoundTo' = p
  /\ inviteGrants' = inviteGrants \cup {<<p, InvitationRole>>}
  /\ IF InvitationRole = "Artist" THEN Assign(p) ELSE UNCHANGED <<roleActive, reactivated>>
  /\ UNCHANGED <<reqStatus, reqPastDeadline, reqApprover, reqApprovedLate, linkRevoked,
                 linkPastDeadline, roleRevoked>>

\* revokeInvitation: UPDATE ... SET revoked_at WHERE redeemed_at IS NULL
RevokeLink ==
  /\ linkBoundTo = None /\ ~linkRevoked
  /\ linkRevoked' = TRUE
  /\ UNCHANGED <<reqStatus, reqPastDeadline, reqApprover, reqApprovedLate, linkBoundTo,
                 linkPastDeadline, inviteGrants, roleActive, roleRevoked, reactivated>>

StaffRevokesRole(p) ==
  /\ roleActive[p]
  /\ roleActive' = [roleActive EXCEPT ![p] = FALSE]
  /\ roleRevoked' = [roleRevoked EXCEPT ![p] = TRUE]
  /\ UNCHANGED <<reqStatus, reqPastDeadline, reqApprover, reqApprovedLate, linkBoundTo,
                 linkRevoked, linkPastDeadline, inviteGrants, reactivated>>

Next ==
  \/ AccessDeadlinePasses \/ ExpireSweep \/ Cancel \/ LinkDeadlinePasses \/ RevokeLink
  \/ \E r \in Parties : Approve(r) \/ Reject(r)
  \/ \E p \in Parties : Redeem(p) \/ StaffRevokesRole(p)

Spec == Init /\ [][Next]_vars

TypeOK ==
  /\ reqStatus \in {"pending", "approved", "rejected", "cancelled", "expired"}
  /\ reqApprover \in Parties \cup {None}
  /\ linkBoundTo \in Parties \cup {None}

NoSelfApproval == reqStatus = "approved" => reqApprover # "requester"
AdminApprovesGrant == reqStatus = "approved" => reqApprover \in Admins
NoApprovalAfterExpiry == ~reqApprovedLate
SingleRedeemer == Cardinality({g[1] : g \in inviteGrants}) <= 1
RedeemedNeverRevoked == ~(linkRevoked /\ linkBoundTo # None)
InvitationNeverAdministrative == \A g \in inviteGrants : g[2] = "Artist"
RevokedStaysRevoked == ~reactivated
====
