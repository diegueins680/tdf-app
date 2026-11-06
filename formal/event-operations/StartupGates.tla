----------------------------- MODULE StartupGates -----------------------------
(* Startup capability-gate policy (DEPLOY-STARTUP-001): the revenue_feature_flag
   assertions of buildSchemaVerificationSql in scripts/lib/production-release.mjs,
   which tdf-hq/production-entrypoint.sh runs before the server accepts traffic
   whenever AUTO_APPLY_PRODUCTION_MIGRATIONS is true (the shipped default).

   Not a state machine: one state, and every invariant quantifies over every
   assignment of the asserted production flags to {missing, disabled,
   enabled}. The approved set of each flag is copied from the requirement and
   AUTHORITY-050; the verifier's rule is modelled per flag as the code writes
   it. Guards, each removed by one negative configuration:
     StagedRejected    an enabled staged provider (PlaceToPay, Payphone) fails
     MissingRejected   a missing gate fails
     EitherAccepted    PayPal and Datafast webhook/refund gates pass in either state
     RequiredChecked   the manual-settlement and marketplace gates must be enabled,
                       service bookings and course gates disabled *)
EXTENDS Naturals, FiniteSets

CONSTANTS StagedRejected, MissingRejected, EitherAccepted, RequiredChecked

States == {"missing", "disabled", "enabled"}

MustBeEnabled == {"checkout.bank_transfer", "checkout.cash", "checkout.pos",
                  "commerce.marketplace_sales", "commerce.marketplace_rentals"}
MustBeDisabled == {"commerce.service_bookings", "commerce.courses",
                   "commerce.course_recurring_billing"}
Either == {"checkout.paypal.webhooks", "checkout.paypal.refunds",
           "checkout.datafast.webhooks", "checkout.datafast.refunds"}
Staged == {"checkout.placetopay.webhooks", "checkout.payphone.notifications"}
Flags == MustBeEnabled \cup MustBeDisabled \cup Either \cup Staged

(* The approved states (the requirement). *)
Approved(f) ==
  CASE f \in MustBeEnabled -> {"enabled"}
    [] f \in MustBeDisabled -> {"disabled"}
    [] f \in Either -> {"enabled", "disabled"}
    [] f \in Staged -> {"disabled"}

(* The verifier's rule for one flag, as coded, guard by guard. *)
Accepts(f, s) ==
  /\ (MissingRejected => s # "missing")
  /\ (f \in Staged /\ StagedRejected => s # "enabled")
  /\ (f \in Either /\ ~EitherAccepted => s # "enabled")
  /\ (f \in MustBeEnabled /\ RequiredChecked => s = "enabled")
  /\ (f \in MustBeDisabled /\ RequiredChecked => s = "disabled")

Verifies(config) == \A f \in Flags : Accepts(f, config[f])

VARIABLE checked
Init == checked = TRUE
Next == UNCHANGED checked
Spec == Init /\ [][Next]_checked

Configs == [Flags -> States]

(* Every approved configuration starts; a restart cannot fail because an
   approved capability is enabled. *)
ApprovedStarts ==
  \A c \in Configs : (\A f \in Flags : c[f] \in Approved(f)) => Verifies(c)

(* A configuration that starts has every flag in an approved state: staged
   providers disabled, every gate present, required states held. *)
StartImpliesApproved ==
  \A c \in Configs : Verifies(c) => (\A f \in Flags : c[f] \in Approved(f))

=============================================================================
