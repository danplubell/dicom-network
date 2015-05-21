.PHONY: all bench build clean configure haddock hpc install repl run test

all: install configure build haddock test hpc bench

bench:
	cabal bench --jobs

build:
	cabal build --jobs

clean:
	cabal clean
	if test -d .cabal-sandbox; then cabal sandbox delete; fi
	if test -d .hpc; then rm -r .hpc; fi

configure:
	cabal configure --enable-tests

haddock:
	cabal haddock --hyperlink-source
	# dist/doc/html/dicom-network/index.html

hpc:
	hpc markup --destdir=tmp dist/hpc/tix/tests/tests.tix
	# tmp/hpc_index.html

install:
	cabal sandbox init --sandbox  /Users/cerdep/Documents/haskell-workspace/dicom/.cabal-sandbox
	cabal install --enable-tests --jobs --only-dependencies --reorder-goals

repl:
	cabal repl lib:dicom-network

run:
	cabal run --jobs dicom-network

test:
	if test -d tests.tix; then rm -r tests.tix; fi
	cabal test --jobs
	cabal check
