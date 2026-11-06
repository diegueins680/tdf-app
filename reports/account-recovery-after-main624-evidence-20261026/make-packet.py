from pathlib import Path
import ast,hashlib,json,subprocess
source=Path('/private/tmp/tdf-account-recovery-ledger222-runtime-20261026')
out=Path('/private/tmp/tdf-account-recovery-main624-runtime-20261026')
receipt=json.loads(Path('/private/tmp/tdf-account-recovery-main624-native-20261026/receipt.json').read_text())
assert receipt['status']=='PASS' and receipt['exitCode']==0 and receipt['source']=='2e01bbe1263e8761fa2692bd951f828c01142449' and receipt['compiler']=='9.10.3'
binary=Path(receipt['binary']);assert hashlib.sha256(binary.read_bytes()).hexdigest()==receipt['binarySha256']
original=(source/'run.py').read_text()
replacements={"binary = Path('/private/tmp/tdf-alert-parity-repro-20261025/bin-post551/tdf-hq-exe')":"binary = Path("+repr(str(binary))+")", "expected_binary = 'cfe718ef2465e872e9db2ab3892b60f5df81642e5c959d6ad07a6ff20bf8e76a'":"expected_binary = "+repr(receipt['binarySha256']), "baseline = 'fe179de45d9b0e085bb7cd2f00a9704991fa1652'":"baseline = "+repr(receipt['source'])}
updated=original
for a,b in replacements.items():assert updated.count(a)==1;updated=updated.replace(a,b)
def body(s):
 tree=ast.parse(s)
 for node in tree.body:
  if isinstance(node,ast.Assign) and any(isinstance(t,ast.Name) and t.id in ['binary','expected_binary','baseline'] for t in node.targets):node.value=ast.Constant(value='qualified-backend-identity')
 return ast.dump(tree,include_attributes=False)
assert body(original)==body(updated)
(out/'run.py').write_text(updated)
for name in ['smtp.py','browser.mjs','loopback-bind.c']:assert (out/name).read_bytes()==(source/name).read_bytes()
r={'qualifiedBinarySource':receipt['source'],'qualifiedBinarySha256':receipt['binarySha256'],'changedOnlyThreePinnedBackendIdentityAssignments':True,'allOtherHarnessAstExact':True,'sameNetworkAndResourceGuards':True,'smtpBrowserAndCByteExact':True,'originalRunnerSha256':hashlib.sha256(original.encode()).hexdigest(),'runnerSha256':hashlib.sha256(updated.encode()).hexdigest(),'originalReceiptPreserved':True,'scope':'New actual protected-main binary replaces historicalfe179; no weakening of source, hash,222ledger,listener/egress or14journey assertions.'}
(out/'composition.json').write_text(json.dumps(r,indent=2)+'\n');print(json.dumps(r,indent=2))
