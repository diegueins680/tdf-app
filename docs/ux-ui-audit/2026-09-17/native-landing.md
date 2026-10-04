# Native return landing — UX-260917-042

ActualiOS18.3 source d70 deep-link authentication reaches the connected profile.
The next cold root displays onboarding because that entry bypassed the optional
welcome marker. The same persisted session opens Social directly, follows an artist
through the canonical endpoint and receives persisted onboarding completion from
actual isolated PostgreSQL. This distinguishes wrong arrival routing from token loss.
Two regression cases fail on oldsource: absent marker chooses welcome; indefinitely
pending marker prevents navigation. The corrected index derives destination from
current hydrated auth before the marker. Five cases preserve guest and hydration
behavior.507tests/85suites, releasecheck and hosted validate/Datadog PASS.

Mobile104 merged90cae070 from a785c998 with the same tree and currentmain0c ancestry.
Actual final production Hermes bundle SHA256
2395d8ee5891beaaf88461dedd8648ebca0a9eaea76270203a88491f6a231e35
was installed in the existing EAS simulator executable from3c72536ac. The ios/android
projects, app config and dependencies have no diff; only an EAS test profile differs.
Two cold returns show the connected profile and persisted followed artist. This is
local simulator verification, not a new cloud build, physical test or store signing.
The first Maestro attempt lacked a ready driver after ENOSPC; after freeing only
owned generated artifacts and obsolete owned simulators, the controlled retry ran.
The resulting d70 welcome counterexample was fixed and the a785 flow passes.

`NativeLanding.tla` enumerates auth(hydrating/guest/member) and optional marker
(pending/seen/unseen):25generated/9distinct states, depth3. HydrationBeforeNavigation,
CurrentSessionSkipsMarker and GuestChoicePreserved hold. HydrationSettles assumes weak
fairness only for auth hydration; marker completion is deliberately not fair and can
remain pending forever. The marker-only mutant violates CurrentSessionSkipsMarker
when hydration returns a member while storage is still pending. Full pinnedTLC/Alloy
runner passes, including the unchanged native follow models/negative controls.

Mapping: Destination mirrors the actual render expression in app/index.tsx;
IndexRoute.test.tsx supplies hydration, absent/pending marker and guest conformance.
This model does not prove AuthProvider, secure storage, OAuth, arbitrary routing or
server authorization. It grants no permission: the existing protected-route/API
checks remain authoritative. No production backend mutation was performed.

Final signed builds and physical checks remain required. EAS refused the new iOS
build after upload because monthly Free quota is exhausted (reset1October); no build
ID or artifact was created. User was asked to enable capacity; no plan purchase made.
Android1c92 sourcef982 atopd70 is valid downloadedQA but predates042 and remains
unexecuted because ADB reports no phone. Notification operator's0c Android14/iOS22
builds are separate and do not contain104. Personal phone installation is untouched.

Actual webD after native follow: real login, followed artist visible and unchanged
server completedAt after reload PASS, evidence/native-final-web-return.json. Initial
web harness used an exact label omitting the required-field asterisk and timed out
before login; corrected accessible-name matching is the qualified run. Prior60source
screenshots remain in versioned evidence; reused temporary filenames from thed70
flow were moved to explicitd70names, not used to relabel the prior proof.

Current release boundary, 18 September 11:40 UTC: mobile #104 is merged as
`90cae070446df6c740e9e8f25a034f3deb12c5c7` (tree identical to a785c998). The complete
507-test/85-suite run and release check pass. The parent main currently pins4122bb75;
this checkpoint advances to the published90ca while retaining4122/0c ancestry.

Final isolated Android build `4602aa55-1914-4a80-9842-30e645339293`, QA source
`6f8a998ed0fbc997e84770fea3d8e0c9edb3fe8e` atop90ca, FINISHED11:28:42UTC. APK package
`com.tdf.records.uxaudit`,1.0.1(8),175818010bytes, SHA256
`e9b8dbeaea34e4ad00a5a9a660b7bddea3ca76ddd7a83b7c6787e25e65420c14` verified.
The physical device is disconnected; this artifact has not been installed/executed.
It supersedes1c92, which predates the cold-return fix042. The local iOS final-Hermes
return proof is separate from the earlier d70 follow receipt; neither is a signed
production build. iOS cloud capacity and physical Google verification remain blocked.

The notification operator's0c Android14/iOS22 EAS builds are now directly observed
FINISHED. They exclude104; build22 is absent from App Store Connect on current query.
No new upload/review/publication is inferred. Existing iOS1.0.1 review still has
AFTER_APPROVAL automatic release, an outstanding gate/coordination issue recorded
in the current README. No competing store mutation was performed.
