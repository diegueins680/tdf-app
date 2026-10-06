# Concurrent branch deletions and retained recovery evidence

The audit issued no branch deletion. GitHub records deletions from another session at approximately 2026-10-03T23:38Z. The user explicitly instructed: “Keep the concurrent deletions; retain recovery evidence.” No restoration is pending.

All 73 last-observed heads were verified as ancestors of `f8925e339447cb593f7cce5d86403d1396aaf5a5`, which remains in main’s history. A standalone Git bundle was created and verified; it retains each recorded head under `refs/audit/concurrent-deletion-recovery-20261004/…`. This establishes recoverability, not that each concurrent deletion met this audit's operational safeguards. Three previously classified operational refs are absent: audit/provider-recovery-artifact-20260918, release/identity-compatible-recovery-20260918, and codex/portable-backend-release-20260928. The available repository event pages contain 32 individual matching DeleteEvents; absence of all 73 refs is independently verified against live paginated branches.

[Machine-readable evidence](../concurrent-deletions-recovery.json) · [verified bundle](../concurrent-deletions-recovery.bundle) · [verification log](../concurrent-deletions-recovery.log) · [user direction](../concurrent-deletions-user-direction.json)

To recover later, fetch the corresponding named ref from the bundle into a local repository, then use the recorded push command after checking current remote state. No recovery push was performed.

| Branch | Last observed head | Initial branch | Recovery command |
|---|---|---|---|
| `audit/event-discovery-integration-20260928` | `cda2c5f7790126f35bd7e09bed35b52b5a0012ef` | False | `git push origin cda2c5f7790126f35bd7e09bed35b52b5a0012ef:refs/heads/audit/event-discovery-integration-20260928` |
| `audit/formal-system-20260918` | `6fbae058e23c989599116c629a80fe216505c1e4` | True | `git push origin 6fbae058e23c989599116c629a80fe216505c1e4:refs/heads/audit/formal-system-20260918` |
| `audit/ip-address-security-20260930` | `d1f1affcd3fbd7e4fde78f2dce25ef0993c34b92` | False | `git push origin d1f1affcd3fbd7e4fde78f2dce25ef0993c34b92:refs/heads/audit/ip-address-security-20260930` |
| `audit/provider-recovery-artifact-20260918` | `f1ff05e6f591cb87ef71a9b66c5abeefa4c40095` | True | `git push origin f1ff05e6f591cb87ef71a9b66c5abeefa4c40095:refs/heads/audit/provider-recovery-artifact-20260918` |
| `audit/social-boundaries-integration-20260917` | `c55932de5c2c27dcd526e4f306fefda8c3671877` | True | `git push origin c55932de5c2c27dcd526e4f306fefda8c3671877:refs/heads/audit/social-boundaries-integration-20260917` |
| `audit/ux-calendar-authoritative-state-20260918` | `563dafa4240933ed7d615ad61cb3d38f000ada22` | True | `git push origin 563dafa4240933ed7d615ad61cb3d38f000ada22:refs/heads/audit/ux-calendar-authoritative-state-20260918` |
| `audit/ux-calendar-runtime-schema-20260918` | `9497be8f4da96159a492c7f290f6b6229095cff2` | True | `git push origin 9497be8f4da96159a492c7f290f6b6229095cff2:refs/heads/audit/ux-calendar-runtime-schema-20260918` |
| `audit/ux-directory-entry-20260918` | `c7124f906ad88e2237dad6cb89ba62a2616501d5` | True | `git push origin c7124f906ad88e2237dad6cb89ba62a2616501d5:refs/heads/audit/ux-directory-entry-20260918` |
| `audit/ux-directory-language-20260918` | `d073042dfef08a9a97c21d18dc5dafe9726e522d` | True | `git push origin d073042dfef08a9a97c21d18dc5dafe9726e522d:refs/heads/audit/ux-directory-language-20260918` |
| `audit/ux-experiment-contract-20260918` | `2ae5e779b1b92e746a45590bd0171b7099458776` | True | `git push origin 2ae5e779b1b92e746a45590bd0171b7099458776:refs/heads/audit/ux-experiment-contract-20260918` |
| `audit/ux-marketplace-storage-20260918` | `585003b6e7d011ea8c00ce0f56980ef7faa0ff8e` | True | `git push origin 585003b6e7d011ea8c00ce0f56980ef7faa0ff8e:refs/heads/audit/ux-marketplace-storage-20260918` |
| `audit/ux-native-release-20260918` | `33efe613f45a0a14208795591d5972a9e538e9a4` | True | `git push origin 33efe613f45a0a14208795591d5972a9e538e9a4:refs/heads/audit/ux-native-release-20260918` |
| `audit/ux-native-tab-release-20260918` | `de177c8589014dc394cd9af465e43be10ae1f744` | True | `git push origin de177c8589014dc394cd9af465e43be10ae1f744:refs/heads/audit/ux-native-tab-release-20260918` |
| `audit/ux-navigation-concurrency-20260918` | `9213b1334aca9b34f377790ed992b869185483c8` | True | `git push origin 9213b1334aca9b34f377790ed992b869185483c8:refs/heads/audit/ux-navigation-concurrency-20260918` |
| `audit/ux-operational-20260918` | `2fb19e8635e5a46e0d31093ed2e9b40dc9bf7b91` | True | `git push origin 2fb19e8635e5a46e0d31093ed2e9b40dc9bf7b91:refs/heads/audit/ux-operational-20260918` |
| `audit/ux-provider-rollback-guard-20260918` | `35172083af75f9c7530624173d82819252e25d1a` | True | `git push origin 35172083af75f9c7530624173d82819252e25d1a:refs/heads/audit/ux-provider-rollback-guard-20260918` |
| `audit/ux-public-data-latency-20260918` | `1d4143f7809f416317d858ad4161580c1dd125e1` | True | `git push origin 1d4143f7809f416317d858ad4161580c1dd125e1:refs/heads/audit/ux-public-data-latency-20260918` |
| `audit/ux-public-loading-labels-20260918` | `1aa708a00e870b17e0d32576b2247fb9a1156719` | True | `git push origin 1aa708a00e870b17e0d32576b2247fb9a1156719:refs/heads/audit/ux-public-loading-labels-20260918` |
| `audit/ux-public-text-reflow-20260918` | `f92b9947d12a3701aff9f38c9abcc77656e01413` | True | `git push origin f92b9947d12a3701aff9f38c9abcc77656e01413:refs/heads/audit/ux-public-text-reflow-20260918` |
| `audit/ux-release-integration-20260918` | `23d5388320583f0c756f6426516649c69b6b118e` | True | `git push origin 23d5388320583f0c756f6426516649c69b6b118e:refs/heads/audit/ux-release-integration-20260918` |
| `audit/ux-remaining-coverage-20260918` | `dabb773a25d56b567c95ee14a06216b40588fe04` | True | `git push origin dabb773a25d56b567c95ee14a06216b40588fe04:refs/heads/audit/ux-remaining-coverage-20260918` |
| `audit/ux-remaining-storage-boundaries-20260918` | `dd9633c1de19568b0f4096d85b9f452f06c1ef1b` | True | `git push origin dd9633c1de19568b0f4096d85b9f452f06c1ef1b:refs/heads/audit/ux-remaining-storage-boundaries-20260918` |
| `audit/ux-static-policy-accessibility-20260918` | `8f19726548c57df47ad2666def486a13d66fdf38` | True | `git push origin 8f19726548c57df47ad2666def486a13d66fdf38:refs/heads/audit/ux-static-policy-accessibility-20260918` |
| `audit/ux-ui-complete-20260917` | `19d8dc830b65b58ccbeb274a41710c6cc95a0d51` | True | `git push origin 19d8dc830b65b58ccbeb274a41710c6cc95a0d51:refs/heads/audit/ux-ui-complete-20260917` |
| `codex/portable-backend-release-20260928` | `3e241ecd8f1631c9e0ac3964ecc0b7600caebc93` | False | `git push origin 3e241ecd8f1631c9e0ac3964ecc0b7600caebc93:refs/heads/codex/portable-backend-release-20260928` |
| `codex/video-event-ingestion-20260925` | `b0713647b2232a77eb021e4428ed541a196de62b` | True | `git push origin b0713647b2232a77eb021e4428ed541a196de62b:refs/heads/codex/video-event-ingestion-20260925` |
| `docs/interaction-qualification-20260930` | `09893a13471d023e255059e74d5a734ed4a43bad` | False | `git push origin 09893a13471d023e255059e74d5a734ed4a43bad:refs/heads/docs/interaction-qualification-20260930` |
| `feat/artist-self-service-20260916` | `912f8326ba7a13611cfbad592f6680fe50bdbefc` | True | `git push origin 912f8326ba7a13611cfbad592f6680fe50bdbefc:refs/heads/feat/artist-self-service-20260916` |
| `feat/event-operations-formal-foundation` | `00f796e74da9e9ec9417ead93980b8a8e71d5fcc` | True | `git push origin 00f796e74da9e9ec9417ead93980b8a8e71d5fcc:refs/heads/feat/event-operations-formal-foundation` |
| `feat/identity-commerce-prevention-20260918` | `23311f29279b691cf2e80c445fb345f9cbfc57f5` | True | `git push origin 23311f29279b691cf2e80c445fb345f9cbfc57f5:refs/heads/feat/identity-commerce-prevention-20260918` |
| `feat/identity-consolidation-20260917` | `72d33c57d5723f1cda413bf8b7858cc361b5a2fb` | True | `git push origin 72d33c57d5723f1cda413bf8b7858cc361b5a2fb:refs/heads/feat/identity-consolidation-20260917` |
| `feat/identity-intake-prevention-20260918` | `02115f7d1b0786f3cdd4287a9466dd22682f603b` | True | `git push origin 02115f7d1b0786f3cdd4287a9466dd22682f603b:refs/heads/feat/identity-intake-prevention-20260918` |
| `feat/identity-registration-prevention-20260918` | `418c0da63a8a95639866f1e92619e6a00b7640c4` | True | `git push origin 418c0da63a8a95639866f1e92619e6a00b7640c4:refs/heads/feat/identity-registration-prevention-20260918` |
| `feat/identity-source-prevention-20260918` | `da0b02d90b57c4f78ce8f20df0bb9345edf824a1` | True | `git push origin da0b02d90b57c4f78ce8f20df0bb9345edf824a1:refs/heads/feat/identity-source-prevention-20260918` |
| `feat/identity-trials-prevention-20260918` | `d7ebacbff0f0e35dbd57238a8afa6f11e86cdb0e` | True | `git push origin d7ebacbff0f0e35dbd57238a8afa6f11e86cdb0e:refs/heads/feat/identity-trials-prevention-20260918` |
| `feat/records-youtube-provider-20260918` | `c79041b10e86872c0a4ea3c7d16e6eab53e7a877` | True | `git push origin c79041b10e86872c0a4ea3c7d16e6eab53e7a877:refs/heads/feat/records-youtube-provider-20260918` |
| `feat/social-api-client-20260915` | `0bac512baed505f7f035f44c022fc0b64405b9e7` | True | `git push origin 0bac512baed505f7f035f44c022fc0b64405b9e7:refs/heads/feat/social-api-client-20260915` |
| `feat/social-authority-20260914` | `c671a5acbe1403ba89d1d70db165b2b327b6cdec` | True | `git push origin c671a5acbe1403ba89d1d70db165b2b327b6cdec:refs/heads/feat/social-authority-20260914` |
| `feat/social-chat-cache-isolation-20260915` | `fd94ed7163fe8afe80f559a664a55576e781aa51` | True | `git push origin fd94ed7163fe8afe80f559a664a55576e781aa51:refs/heads/feat/social-chat-cache-isolation-20260915` |
| `feat/social-chat-verification-20260915` | `547490105d12e0581b6ca5fddc135f127abedf07` | True | `git push origin 547490105d12e0581b6ca5fddc135f127abedf07:refs/heads/feat/social-chat-verification-20260915` |
| `feat/social-dm-api-boundary-20260915` | `44a1c7bededf0ba4b54068d482667ac24299f8e4` | True | `git push origin 44a1c7bededf0ba4b54068d482667ac24299f8e4:refs/heads/feat/social-dm-api-boundary-20260915` |
| `feat/social-dm-write-boundary-20260915` | `e03ce193c2e9c6788fad5f1360aef1088006616f` | True | `git push origin e03ce193c2e9c6788fad5f1360aef1088006616f:refs/heads/feat/social-dm-write-boundary-20260915` |
| `feat/social-fanclub-effects-20260916` | `daf6c5a680b81928c2e2c66df98063328e6d5e9c` | True | `git push origin daf6c5a680b81928c2e2c66df98063328e6d5e9c:refs/heads/feat/social-fanclub-effects-20260916` |
| `feat/social-feed-filtering-20260915` | `6a6d9e02c22eee213cf56787d9de9e4252721fed` | True | `git push origin 6a6d9e02c22eee213cf56787d9de9e4252721fed:refs/heads/feat/social-feed-filtering-20260915` |
| `feat/social-formal-audit-20260914` | `0cb8d2e15ac024682933485ed122c34826f2ba6f` | True | `git push origin 0cb8d2e15ac024682933485ed122c34826f2ba6f:refs/heads/feat/social-formal-audit-20260914` |
| `feat/social-legacy-write-boundary-20260916` | `7e601607f60b2987e502c4a14db821bbf37df296` | True | `git push origin 7e601607f60b2987e502c4a14db821bbf37df296:refs/heads/feat/social-legacy-write-boundary-20260916` |
| `feat/social-profile-read-boundary-20260915` | `7c452efcbc7b45e797b7ebd590dfffccf0fc9751` | True | `git push origin 7c452efcbc7b45e797b7ebd590dfffccf0fc9751:refs/heads/feat/social-profile-read-boundary-20260915` |
| `feat/social-query-budget-20260914` | `f52b1f8b906d5bf61774dfaf6b6173233b0d2172` | True | `git push origin f52b1f8b906d5bf61774dfaf6b6173233b0d2172:refs/heads/feat/social-query-budget-20260914` |
| `feat/social-reaction-repair-20260915` | `24590a40787d81a1681cd14a46f2f0ab7cac5632` | True | `git push origin 24590a40787d81a1681cd14a46f2f0ab7cac5632:refs/heads/feat/social-reaction-repair-20260915` |
| `feat/social-relationship-read-boundary-20260915` | `8b2d1557a5e72e0346af1fd43dc7cf36033da067` | True | `git push origin 8b2d1557a5e72e0346af1fd43dc7cf36033da067:refs/heads/feat/social-relationship-read-boundary-20260915` |
| `feat/social-schema-compatibility-20260915` | `3606fb5c2215e00b1f3661abdf9e969231ac3b36` | True | `git push origin 3606fb5c2215e00b1f3661abdf9e969231ac3b36:refs/heads/feat/social-schema-compatibility-20260915` |
| `feat/social-token-boundary-20260915` | `2be39150f626dcd5181951532a8ebe25a3ce3fa4` | True | `git push origin 2be39150f626dcd5181951532a8ebe25a3ce3fa4:refs/heads/feat/social-token-boundary-20260915` |
| `feat/universal-interactions-20260928` | `fd87416e99af3176d107178832f238ef00003be5` | False | `git push origin fd87416e99af3176d107178832f238ef00003be5:refs/heads/feat/universal-interactions-20260928` |
| `fix/auth-locale-20260917` | `209b21b07f304d21ab397353653b531646ae72be` | True | `git push origin 209b21b07f304d21ab397353653b531646ae72be:refs/heads/fix/auth-locale-20260917` |
| `fix/ci-green-baseline-20260912` | `5bceb329f034cdfadf077f8f2889aa50bd402c7c` | True | `git push origin 5bceb329f034cdfadf077f8f2889aa50bd402c7c:refs/heads/fix/ci-green-baseline-20260912` |
| `fix/ci-messaging-readonly-20260915` | `75c096bea71dbf58f92852fca3ab140116e6697f` | True | `git push origin 75c096bea71dbf58f92852fca3ab140116e6697f:refs/heads/fix/ci-messaging-readonly-20260915` |
| `fix/directory-privacy-migration-order-20260917` | `c7643f0ba07f330995415c4848194938536d51ae` | True | `git push origin c7643f0ba07f330995415c4848194938536d51ae:refs/heads/fix/directory-privacy-migration-order-20260917` |
| `fix/disable-unverified-service-escrow-20260920` | `989d0663cc249bcee74331604561628d3435c961` | True | `git push origin 989d0663cc249bcee74331604561628d3435c961:refs/heads/fix/disable-unverified-service-escrow-20260920` |
| `fix/event-confirmed-end-20260920` | `648f2d6dc69080d4438ae2a25309497012ded2d1` | True | `git push origin 648f2d6dc69080d4438ae2a25309497012ded2d1:refs/heads/fix/event-confirmed-end-20260920` |
| `fix/event-discovery-schedule-20260918` | `f4223a98d82bebfd5b15147795379a6c8cf58fb6` | True | `git push origin f4223a98d82bebfd5b15147795379a6c8cf58fb6:refs/heads/fix/event-discovery-schedule-20260918` |
| `fix/fanhub-audit-20260917` | `36f254d14b62efc10268df5545a15bfe157cb0a3` | True | `git push origin 36f254d14b62efc10268df5545a15bfe157cb0a3:refs/heads/fix/fanhub-audit-20260917` |
| `fix/identity-request-recovery-20260918` | `261bfdfbbdab0520b35725a72b776f160b608dd3` | True | `git push origin 261bfdfbbdab0520b35725a72b776f160b608dd3:refs/heads/fix/identity-request-recovery-20260918` |
| `fix/interaction-deletion-focus-20260930` | `4731a35742b4939236bc08f43c5ea2780820caee` | False | `git push origin 4731a35742b4939236bc08f43c5ea2780820caee:refs/heads/fix/interaction-deletion-focus-20260930` |
| `fix/interaction-release-dompurify-20261001` | `6a830cf74b1c9ff81fdce9814d73435ce05d0b27` | False | `git push origin 6a830cf74b1c9ff81fdce9814d73435ce05d0b27:refs/heads/fix/interaction-release-dompurify-20261001` |
| `fix/mail-delivery-observability-20260918` | `ab9bbacc9da845b6bfe70ac3fda2ace44f17c918` | True | `git push origin ab9bbacc9da845b6bfe70ac3fda2ace44f17c918:refs/heads/fix/mail-delivery-observability-20260918` |
| `fix/mail-smtp-log-flush-20260918` | `592916f2a7fbcd740409acda42e69d1a6bfb128a` | True | `git push origin 592916f2a7fbcd740409acda42e69d1a6bfb128a:refs/heads/fix/mail-smtp-log-flush-20260918` |
| `fix/migration-introduction-ancestry-audit-20260916` | `e24922210d09226bec610a8fe0f1b6f05867e8d7` | True | `git push origin e24922210d09226bec610a8fe0f1b6f05867e8d7:refs/heads/fix/migration-introduction-ancestry-audit-20260916` |
| `fix/notification-navigation-20260917` | `a5a391954ec4152a19e1502c08347edd34624060` | True | `git push origin a5a391954ec4152a19e1502c08347edd34624060:refs/heads/fix/notification-navigation-20260917` |
| `fix/records-thumbnails-ingestion-20260918` | `697a5c748332a7179d3a3e7f7bbfd7d5323579b9` | True | `git push origin 697a5c748332a7179d3a3e7f7bbfd7d5323579b9:refs/heads/fix/records-thumbnails-ingestion-20260918` |
| `fix/recovery-dialog-navigation-20260918` | `65cc350a68bf293720bed21afb771d75ab8fbea7` | True | `git push origin 65cc350a68bf293720bed21afb771d75ab8fbea7:refs/heads/fix/recovery-dialog-navigation-20260918` |
| `fix/redact-stripe-webhook-example-20260916` | `2455951713967642de9b429c5a790ceec6cb1fa2` | True | `git push origin 2455951713967642de9b429c5a790ceec6cb1fa2:refs/heads/fix/redact-stripe-webhook-example-20260916` |
| `fix/social-stack-integration-20260916` | `5fcd17c926a70073c8ef3eda33904e1b207859ee` | True | `git push origin 5fcd17c926a70073c8ef3eda33904e1b207859ee:refs/heads/fix/social-stack-integration-20260916` |
| `release/identity-compatible-recovery-20260918` | `bdd9e24bddaaa96e2da72d75041b1e1b20236e04` | True | `git push origin bdd9e24bddaaa96e2da72d75041b1e1b20236e04:refs/heads/release/identity-compatible-recovery-20260918` |
