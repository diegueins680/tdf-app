import collect
for endpoint,dest in [('branches/main','mobile-main.json'),('branches/main/protection','mobile-protection.json'),('rulesets?per_page=100','mobile-rulesets.json'),('pulls?state=open&per_page=100','mobile-open-prs.json')]:
 collect.api('repos/diegueins680/TDF-mobile/'+endpoint,dest,'?' in endpoint)
