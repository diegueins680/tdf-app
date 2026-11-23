#!/usr/bin/env python3
"""DIR-SEMANTIC-SEARCH-001: the ranking envelope around the semantic term.

Two pinned regions are read:

  * tdf-hq/src/TDF/Server/Directory.hs, searchSql: the title tiers of
    text_score, the weights and divisor of organic_score, its rounding and the
    image boost of rank_score. The numbers are parsed from the SQL text; an
    unknown shape fails;
  * tdf-hq/src/TDF/Directory/SemanticSearch.hs, similaritySql: the similarity
    is clamped to [0, 1], is 0 without an embedding row, and the semantic term
    is greatest(alias, similarity).

Scores are real numbers. The model of one document is

    organic = round6((wt*text + ws*semantic + wp*proximity + wq*quality
                      + wa*activity + wv*availability + wr*reputation) / div)
    rank    = organic + boost*image

with semantic = max(alias, similarity), alias in {0, 1}, similarity in
[0, 1], image in {0, 1}, every other component in [0, 1], and round6 any
multiple of 1e-6 within 5e-7 of its argument (rounding to six decimals; the
direction of a tie is left open).

Proofs (UNSAT of the negation), for all component values in range:
  - exact-title: a document whose title equals the query (text = 10) ranks
    strictly above every document whose title does not (text <= 7), whatever
    the two semantic terms are;
  - prefix-title, contains-title: the same for the next two tiers (7 against
    <= 4, and 4 against <= 1);
  - fallback: with similarity = 0 the weighted sum equals the sum computed
    with the alias term alone, term for term (today's formula);
  - range: 0 <= organic <= 1;
  - monotone: a larger similarity never lowers the weighted sum.

Witnesses: within one title tier a higher similarity can reverse the order
of two documents (the term has an effect), and an exact match with nothing
else can sit within .03 of a prefix match with everything else (the margin
is small but positive).

Necessity: the clamp of the similarity, the bound of the semantic weight and
the size of the image boost are each relaxed in turn; each must make
exact-title fail.

Not covered: that PostgreSQL evaluates the SQL as the real-number model says
(float8 arithmetic, the numeric cast); that the components the SQL reads are
in range, which rests on CHECK constraints of directory_search_document and
on source_updated_at not lying in the future; the alias and title predicates
themselves; and the cosine, which is whatever the provider's vectors give.

Run with the pinned solver of formal/system/requirements-verification.txt
(z3-solver 4.13.3.0).
"""
import hashlib
import re
import sys
from fractions import Fraction
from pathlib import Path

import z3

PINNED = '4.13.3'
ROOT = Path(__file__).resolve().parents[2]
SEARCH = 'tdf-hq/src/TDF/Server/Directory.hs'
SEMANTIC = 'tdf-hq/src/TDF/Directory/SemanticSearch.hs'
REGIONS = (
    (SEARCH, 'searchSql :: Bool -> Text', '-- `total` and `cities` describe the current query', 'PIN_SEARCH'),
    (SEMANTIC, 'similaritySql :: Text', '-- | Derive the semantic statement from today', 'PIN_SIMILARITY'),
)
PINS = {
    'PIN_SEARCH': '68cc5f6bb821bb6d896207ff0f0dcc3cfe842f7c37f0e94a6649bb827929cc18',
    'PIN_SIMILARITY': 'e9e3515e1905875a48baae99c2c602325e0a7ee441d8c2d6f59ebe4445e9fe27',
}
NUM = r'(\d*\.?\d+)'
TIERS = re.compile(
    r"CASE WHEN input\.q='' THEN " + NUM
    + r" WHEN directory_normalize_text\(document\.title\)=directory_normalize_text\(input\.q\) THEN " + NUM
    + r" WHEN directory_normalize_text\(document\.title\) LIKE directory_normalize_text\(input\.q\)\|\|'%' THEN " + NUM
    + r" WHEN position\(directory_normalize_text\(input\.q\) in directory_normalize_text\(document\.title\)\)>0 THEN " + NUM
    + r" ELSE least\(1,greatest\(")
FORMULA = re.compile(
    r"round\(\(\(" + NUM + r"\*text_score\+" + NUM + r"\*semantic_score\+" + NUM
    + r"\*CASE WHEN distance_km IS NULL THEN " + NUM + r" ELSE 1/\(1\+distance_km/25\) END\+" + NUM
    + r"\*profile_completeness\+" + NUM
    + r"\*least\(1,1/\(1\+extract\(epoch from \(now\(\)-source_updated_at\)\)/2592000\)\)\+" + NUM
    + r"\*availability_score\+" + NUM + r"\*reputation_score\)/" + NUM + r"\)::numeric,6\) organic_score")
BOOST = re.compile(r"organic_score\+" + NUM + r"\*image_score rank_score")
ALIAS = "THEN 1 ELSE 0 END semantic_score "
CLAMP = "round(least(1,greatest(0,coalesce(nullif(1-(emb.embedding<=>input.q_embedding),'NaN'::float8),0)))::numeric,4)"
GUARDS = ('similarity-clamp', 'semantic-weight', 'image-boost')
HALF_ULP = Fraction(5, 10 ** 7)


def regions():
    ok, texts = True, {}
    for source, start, end, pin in REGIONS:
        text = (ROOT / source).read_text()
        if text.count(start) != 1 or text.count(end) != 1:
            print(f'TDF_SMT_RESULT source {source} markers FAILED')
            ok = False
            continue
        region = text[text.index(start):text.index(end)]
        digest = hashlib.sha256(region.encode()).hexdigest()
        current = digest == PINS[pin]
        ok = ok and current
        texts[pin] = region
        print(f"TDF_SMT_RESULT source {source} region-sha256={digest} {'ok' if current else 'FAILED'}")
    return ok, texts


def constants(texts):
    """The numbers of the ranking formula, parsed from the pinned SQL text."""
    search = texts['PIN_SEARCH']
    tiers, formula, boost = TIERS.findall(search), FORMULA.findall(search), BOOST.findall(search)
    if len(tiers) != 1 or len(formula) != 1 or len(boost) != 1 or search.count(ALIAS) != 1:
        return None
    similarity = texts['PIN_SIMILARITY']
    if similarity.count(CLAMP) != 1 or similarity.count('),0)"') != 1:
        return None
    browse, exact, prefix, contains = (Fraction(v) for v in tiers[0])
    wt, ws, wp, unknown, wq, wa, wv, wr, div = (Fraction(v) for v in formula[0])
    return {
        'browse': browse, 'exact': exact, 'prefix': prefix, 'contains': contains, 'weak': Fraction(1),
        'wt': wt, 'ws': ws, 'wp': wp, 'unknown-distance': unknown, 'wq': wq, 'wa': wa, 'wv': wv, 'wr': wr,
        'div': div, 'boost': Fraction(boost[0]),
    }


def real(value):
    return z3.RealVal(str(value))


class Document:
    """One ranked document; `facts` constrain its components to their ranges."""

    def __init__(self, name, k, dropped=None):
        var = lambda field: z3.Real(f'{name}_{field}')
        self.text, self.alias, self.similarity = var('text'), var('alias'), var('similarity')
        self.proximity, self.quality, self.activity = var('proximity'), var('quality'), var('activity')
        self.availability, self.reputation, self.image = var('availability'), var('reputation'), var('image')
        self.organic = var('organic')
        self.micros = z3.Int(f'{name}_micros')
        ws = real(k['ws'] * 2) if dropped == 'semantic-weight' else real(k['ws'])
        boost = real(k['boost'] * 2) if dropped == 'image-boost' else real(k['boost'])
        ceiling = 2 if dropped == 'similarity-clamp' else 1
        self.semantic = z3.If(self.alias >= self.similarity, self.alias, self.similarity)
        self.weights = (real(k['wt']), ws, real(k['wp']), real(k['wq']), real(k['wa']), real(k['wv']), real(k['wr']))
        self.div = real(k['div'])
        self.raw = self.sum(self.semantic)
        self.rank = self.organic + boost * self.image
        unit = lambda x: z3.And(x >= 0, x <= 1)
        self.facts = z3.And(
            z3.Or(self.alias == 0, self.alias == 1),
            self.similarity >= 0, self.similarity <= ceiling,
            z3.Or(self.image == 0, self.image == 1),
            unit(self.proximity), unit(self.quality), unit(self.activity),
            unit(self.availability), unit(self.reputation),
            self.organic * 1000000 == z3.ToReal(self.micros),
            self.organic - self.raw <= real(HALF_ULP), self.raw - self.organic <= real(HALF_ULP))

    def sum(self, semantic):
        wt, ws, wp, wq, wa, wv, wr = self.weights
        return (wt * self.text + ws * semantic + wp * self.proximity + wq * self.quality
                + wa * self.activity + wv * self.availability + wr * self.reputation) / self.div


def proved(claim):
    solver = z3.Solver()
    solver.set('timeout', 30000)
    solver.add(z3.Not(claim))
    return solver.check() == z3.unsat


def satisfiable(*facts):
    solver = z3.Solver()
    solver.set('timeout', 30000)
    solver.add(*facts)
    return solver.check() == z3.sat


def properties(k, dropped=None):
    a, b = Document('a', k, dropped), Document('b', k, dropped)
    ranges = z3.And(a.facts, b.facts)
    tier = lambda upper, lower: z3.Implies(
        z3.And(ranges, a.text == real(k[upper]), b.text >= 0, b.text <= real(k[lower])), a.rank > b.rank)
    return {
        'exact-title': tier('exact', 'prefix'),
        'prefix-title': tier('prefix', 'contains'),
        'contains-title': tier('contains', 'weak'),
        'fallback': z3.Implies(z3.And(a.facts, a.similarity == 0), a.raw == a.sum(a.alias)),
        'range': z3.Implies(
            z3.And(a.facts, a.text >= 0, a.text <= real(k['exact'])), z3.And(a.organic >= 0, a.organic <= 1)),
        'monotone': z3.Implies(
            z3.And(ranges, a.text == b.text, a.alias == b.alias, a.proximity == b.proximity,
                   a.quality == b.quality, a.activity == b.activity, a.availability == b.availability,
                   a.reputation == b.reputation, a.similarity <= b.similarity),
            a.raw <= b.raw),
    }


def witnesses(k):
    a, b = Document('a', k), Document('b', k)
    same = z3.And(a.text == real(k['weak']), b.text == real(k['weak']), a.alias == 0, b.alias == 0,
                  a.image == b.image, a.availability == b.availability, a.reputation == b.reputation,
                  a.proximity == b.proximity, a.activity == b.activity)
    return {
        'similarity-reverses-an-order-within-a-tier': (
            a.facts, b.facts, same, a.quality < b.quality, a.similarity > b.similarity, a.rank > b.rank),
        'exact-margin-is-below-three-hundredths': (
            a.facts, b.facts, a.text == real(k['exact']), b.text == real(k['prefix']),
            a.rank - b.rank < real(Fraction(3, 100))),
    }


def main():
    ok = z3.get_version_string().startswith(PINNED)
    print(f"TDF_SMT_RESULT solver z3 {z3.get_version_string()} {'ok' if ok else 'FAILED'}", flush=True)
    current, texts = regions()
    ok = current and ok
    k = constants(texts) if len(texts) == len(REGIONS) else None
    if k is None:
        print('TDF_SMT_RESULT source ranking-formula shape FAILED')
        print('TDF_SMT_RESULT summary FAILED')
        return 1
    print('TDF_SMT_RESULT source ranking-formula '
          + ' '.join(f'{name}={float(value):g}' for name, value in k.items()) + ' ok', flush=True)
    for name, claim in properties(k).items():
        result = proved(claim)
        ok = ok and result
        print(f"TDF_SMT_RESULT proof {name} {'unsat' if result else 'FAILED'}", flush=True)
    for name, facts in witnesses(k).items():
        result = satisfiable(*facts)
        ok = ok and result
        print(f"TDF_SMT_RESULT witness {name} {'sat' if result else 'FAILED'}", flush=True)
    for guard in GUARDS:
        result = not proved(properties(k, guard)['exact-title'])
        ok = ok and result
        print(f"TDF_SMT_RESULT necessity relax-{guard} "
              f"{'breaks exact-title' if result else 'FAILED (guard is not load-bearing)'}", flush=True)
    print(f"TDF_SMT_RESULT summary {'ok' if ok else 'FAILED'}", flush=True)
    return 0 if ok else 1


if __name__ == '__main__':
    sys.exit(main())
