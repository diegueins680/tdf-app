module event_operations/FriendSuggestionGraph

// COMMUNITY-FRIEND-SUGGESTION-001, formal/system/friend-suggestions-pagerank.md.
// Structure of the graph one viewer walks. Scores are a function of the viewer
// and of this graph only (TDF.Social.SuggestionRank.walkScores), so two worlds
// with the same graph give the same order. Numbers are not modelled here.

sig Account {
  follows: set Account,   -- follow rows before the attack
  injected: set Account,  -- follow rows the attacker adds
  blocks: set Account
}
// Facts the nightly run reads about each account.
one sig Policy { live, aged, verified: set Account }

pred eligible[a: Account] {
  a in Policy.live
  a in Policy.aged
  a in Policy.verified
}
// An account created less than the minimum age ago, or without a verified email.
pred fake[a: Account] {
  a not in Policy.aged or a not in Policy.verified
}
pred node[v, a: Account] {
  eligible[a]
  a not in v.blocks
  v not in a.blocks
}
pred edge[v, a, b: Account, rows: Account -> Account] {
  node[v, a]
  node[v, b]
  a != b
  a -> b in rows
}
fun walk[v: Account, rows: Account -> Account]: Account -> Account {
  { a, b: Account | edge[v, a, b, rows] }
}
// Accounts that can hold score for v: reachable from v along the walk.
fun support[v: Account, rows: Account -> Account]: set Account {
  v.*(walk[v, rows])
}

// The attacker controls the fake accounts. It can make them follow anyone and,
// because the legacy friend writer creates both directions, make anyone follow
// them. It cannot add a follow between two accounts it does not control.
fact InjectedFollowsInvolveAFake {
  all a, b: Account | a -> b in injected implies fake[a] or fake[b]
}

assert FakeFollowsLeaveEveryWalkUnchanged {
  all v: Account | walk[v, follows + injected] = walk[v, follows]
}
assert OnlyLiveAgedVerifiedAccountsCarryScore {
  all v: Account | node[v, v] implies
    support[v, follows + injected] in Policy.live & Policy.aged & Policy.verified
}
assert AccountsTheViewerBlockedCarryNoScore {
  all v, a: Account | node[v, v] and a in v.blocks implies
    a not in support[v, follows + injected]
}
assert AccountsBlockingTheViewerCarryNoScore {
  all v, a: Account | node[v, v] and v in a.blocks implies
    a not in support[v, follows + injected]
}
pred secondDegreeScenario {
  some disj v, friend, candidate: Account | {
    node[v, v]
    friend in v.follows
    candidate in friend.follows
    candidate not in v.follows
    candidate in support[v, follows]
    some a: Account | fake[a] and some a.injected
  }
}
run secondDegreeScenario for 4
check FakeFollowsLeaveEveryWalkUnchanged for 5
check OnlyLiveAgedVerifiedAccountsCarryScore for 5
check AccountsTheViewerBlockedCarryNoScore for 5
check AccountsBlockingTheViewerCarryNoScore for 5
