------------------------- MODULE OperationsReadPolicy -------------------------
(* The Operations read capability policy (AUTH-OPS-READ-001):
   visibleEntityTypes and canViewEntityType in tdf-hq/src/TDF/Operations/Model.hs,
   and scopeFilterSql / scopeFilterParams in tdf-hq/src/TDF/Operations/Server.hs,
   which list, metrics and stream reads share while detail reads use the
   Haskell predicate.

   This is not a state machine: one state, and every invariant quantifies
   over every role set, assignment flag and entity type, so TLC evaluates
   the whole finite policy. The grant table is copied from Model.hs; Other
   stands for every role the table does not mention.

   Guards, each removed by one negative configuration:
     WildcardExcludesSecurity  "*" never includes security_incident (Haskell)
     SqlExcludesSecurity       the SQL wildcard branch excludes it too
     AssignmentRestricts       Teacher and Engineer see only assigned items
     ReadOnlyNoWrite           ReadOnly is dropped before write visibility
     UnknownDenies             an unlisted role grants nothing *)
EXTENDS Naturals, FiniteSets

CONSTANTS WildcardExcludesSecurity, SqlExcludesSecurity, AssignmentRestricts,
          ReadOnlyNoWrite, UnknownDenies

Roles == {"Admin", "Manager", "StudioManager", "ReadOnly", "Accounting",
          "Reception", "Teacher", "Engineer", "Maintenance", "Other"}

Security == "security_incident"
Wild == "*"

Types == {"course_registration", "booking", "party", "invoice", "payment",
          "manual", "uncorrelated_inbound", "proposal", "social_event",
          "marketplace_order", "intern_project", "maintenance_ticket",
          "stock_item", Security, "other_domain"}

Grants(r, assigned) ==
  CASE r = "Admin" -> {Wild, Security}
    [] r \in {"Manager", "StudioManager", "ReadOnly"} -> {Wild}
    [] r = "Accounting" -> {"invoice", "payment", "marketplace_order", "course_registration"}
    [] r = "Reception" -> {"course_registration", "booking", "party", "invoice", "payment",
                           "manual", "uncorrelated_inbound", "proposal", "social_event"}
    [] r = "Teacher" -> IF assigned \/ ~AssignmentRestricts
                          THEN {"course_registration", "booking", "social_event", "intern_project"}
                          ELSE {}
    [] r = "Engineer" -> IF assigned \/ ~AssignmentRestricts
                           THEN {"booking", "maintenance_ticket", "social_event", "intern_project"}
                           ELSE {}
    [] r = "Maintenance" -> {"maintenance_ticket", "stock_item", "booking", "manual"}
    [] r = "Other" -> IF UnknownDenies THEN {} ELSE {Wild}

Allowed(roles, assigned) == UNION {Grants(r, assigned) : r \in roles}

(* canViewEntityType *)
HsVisible(roles, assigned, t) ==
  \/ t \in Allowed(roles, assigned)
  \/ (Wild \in Allowed(roles, assigned) /\ (t # Security \/ ~WildcardExcludesSecurity))

(* scopeFilterSql with scopeFilterParams: the wildcard flag and the explicit
   list are computed with assigned = FALSE, the assignee branch with TRUE. *)
SqlVisible(roles, assigned, t) ==
  \/ (Wild \in Allowed(roles, FALSE) /\ (t # Security \/ ~SqlExcludesSecurity))
  \/ t \in Allowed(roles, FALSE)
  \/ (assigned /\ t \in Allowed(roles, TRUE))

(* Write visibility: the handlers call canViewEntityType on the roles
   without ReadOnly. *)
WriteVisible(roles, assigned, t) ==
  HsVisible(IF ReadOnlyNoWrite THEN roles \ {"ReadOnly"} ELSE roles, assigned, t)

VARIABLE checked
Init == checked = TRUE
Next == UNCHANGED checked
Spec == Init /\ [][Next]_checked

RoleSets == SUBSET Roles

(* Detail (Haskell) and list, metrics and stream (SQL) agree on every read. *)
ReadersAgree ==
  \A roles \in RoleSets, a \in BOOLEAN, t \in Types :
    HsVisible(roles, a, t) = SqlVisible(roles, a, t)

(* Only an Admin sees security incidents, by either evaluator. *)
NoSecurityDisclosure ==
  \A roles \in RoleSets, a \in BOOLEAN :
    "Admin" \notin roles => (~HsVisible(roles, a, Security) /\ ~SqlVisible(roles, a, Security))

(* Adding ReadOnly to any role set changes no write visibility. *)
ReadOnlyNoWriteGain ==
  \A roles \in RoleSets, a \in BOOLEAN, t \in Types :
    WriteVisible(roles \cup {"ReadOnly"}, a, t) = WriteVisible(roles, a, t)

(* Teachers and engineers (and unlisted roles) see nothing unassigned. *)
SpecialistsAssigned ==
  \A roles \in SUBSET {"Teacher", "Engineer", "Other"}, t \in Types :
    ~HsVisible(roles, FALSE, t)

(* An unlisted role alone sees nothing, assigned or not. *)
UnknownNothing == \A a \in BOOLEAN, t \in Types : ~HsVisible({"Other"}, a, t)

=============================================================================
