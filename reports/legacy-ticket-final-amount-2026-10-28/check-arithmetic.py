"""Scoped arithmetic proof; not a Haskell/SQL refinement or provider limit proof."""
import argparse,hashlib,json,re,subprocess
from pathlib import Path
import z3

parser=argparse.ArgumentParser(description=__doc__)
parser.add_argument('--root',type=Path,default=Path.cwd())
root=parser.parse_args().root.resolve()
def require(condition, message):
    if not condition:
        raise ValueError(message)

path='tdf-hq/src/TDF/Server/SocialEventsHandlers.hs'
b=(root/path).read_bytes();source=b.decode()
require(re.search(r'^ticketPlatformFeeBps = 400$',source,re.M), 'Fee policy assumption changed')
require(re.search(r'^    \| total > 99999999 =$',source,re.M), 'Envelope assumption changed')
start=source.index('ticketPlatformFeeBreakdown faceValue =')
end=source.index('\nencodeTicketCheckoutMetadata ::',start)
formula=source[start:end]
require('faceValue * ticketPlatformFeeBps `div` 10000' in formula, 'Fee formula syntax changed')
require('(totalFee + 1) `div` 2' in formula, 'Buyer rounding syntax changed')
require('ticketCheckoutTotalCents = faceValue + buyerFee' in formula, 'Total formula syntax changed')
face=z3.Int('discounted_face');fee=face*400/10000;buyer=(fee+1)/2;total=face+buyer
claims=[]
for name,fault in [('all_supported_faces_fit',z3.And(face<=98039215,total>99999999)),
                   ('all_larger_faces_exceed',z3.And(face>98039215,total<=99999999)),
                   ('fee_arithmetic_fits_current_int64',z3.Or(face*400>2**63-1,total>2**63-1))]:
    solver=z3.Solver();solver.add(face>=0,face<=99999999,fault)
    result=solver.check();require(result==z3.unsat, f'{name}: expected unsat, got {result}')
    claims.append({'id':name,'negation':str(result),'assurance':'PROVEN',
                   'scope':'Integer arithmetic only under the stated bounds/formula; no implementation refinement.'})
solver=z3.Solver();solver.add(face==98039216,total==100000000)
require(solver.check()==z3.sat, 'Boundary counterexample is not satisfiable')
print(json.dumps({'schemaVersion':1,'observedRevision':subprocess.check_output(['git','rev-parse','HEAD'],cwd=root,text=True).strip(),
 'source':path,'sourceSha256':hashlib.sha256(b).hexdigest(),'formulaSha256':hashlib.sha256(formula.encode()).hexdigest(),
 'z3Version':z3.get_version_string(),'claims':claims,'counterexample':{'face':98039216,'final':100000000},
 'assumptions':['Integer arithmetic, discounted face between0 and99,999,999 inclusive, fee bps400, floor division and buyer rounding from the checked source formula.',
                'Current64-bit Int; existing base validation plus valid promo keeps face within these bounds.',
                'The conservative99,999,999 internal admission envelope is the requirement; this is not a universal Stripe maximum.'],
 'limitations':['Manual abstraction with exact source/formula hashes and syntax checks, not a Haskell semantics/refinement proof.',
                'Source can change after this one-shot check; committed source seal and implementation regression tests supply separate evidence.',
                'No provider contact, PostgreSQL guarantee, account/currency/card validation or financial charge attestation.']},indent=2))
