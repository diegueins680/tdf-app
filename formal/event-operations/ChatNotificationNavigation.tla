---- MODULE ChatNotificationNavigation ----
EXTENDS Naturals, FiniteSets, TLC
CONSTANTS BindNavigation, RejectOldResponse
VARIABLES navigation, pending, admitted, authority, member, badDisplay, badRead
vars == <<navigation, pending, admitted, authority, member, badDisplay, badRead>>

\* Two same-route navigations, one cached response and a prior pending poll.
\* authority records the response's real request scope independently of the
\* client admission marker. No backend policy refinement or liveness claim.
Init == /\ navigation = 0 /\ pending = {0}
        /\ admitted = 0 /\ authority = 0 /\ member = TRUE
        /\ badDisplay = FALSE /\ badRead = FALSE
Navigate == /\ navigation < 2 /\ navigation' = navigation + 1
            /\ pending' = pending \cup {navigation + 1}
            /\ UNCHANGED <<admitted, authority, member, badDisplay, badRead>>
Complete(request, allowed) ==
  /\ request \in pending
  /\ pending' = pending \ {request}
  /\ IF RejectOldResponse /\ request # navigation
        THEN UNCHANGED <<admitted, authority, member>>
        ELSE /\ admitted' = navigation /\ authority' = request /\ member' = allowed
  /\ UNCHANGED <<navigation, badDisplay, badRead>>
CanUse == member /\ (~BindNavigation \/ admitted = navigation)
Display == /\ navigation > 0 /\ CanUse
           /\ badDisplay' = (badDisplay \/ authority # navigation)
           /\ UNCHANGED <<navigation, pending, admitted, authority, member, badRead>>
MarkRead == /\ navigation > 0 /\ CanUse
            /\ badRead' = (badRead \/ authority # navigation)
            /\ UNCHANGED <<navigation, pending, admitted, authority, member, badDisplay>>
Next == Navigate \/ (\E request \in pending: Complete(request, TRUE) \/ Complete(request, FALSE))
        \/ Display \/ MarkRead
NoStaleNavigationDisplay == ~badDisplay
NoStaleNavigationRead == ~badRead
Spec == Init /\ [][Next]_vars
====
