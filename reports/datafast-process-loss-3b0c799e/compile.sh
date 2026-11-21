set -eu
r="$PWD/replay-tools/root"
g="$r/usr/lib/ghc"
"$g/bin/ghc-pkg" --global-package-db="$r/var/lib/ghc/package.conf.d" recache
"$g/bin/ghc" -B"$g/lib" -threaded -O0 -ipg-replay-src/tdf-hq/src -outputdir durable-lab/build -L"$r/usr/lib/x86_64-linux-gnu" durable-lab/Main.hs -o durable-lab/probe
