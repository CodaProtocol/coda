########################################
## Docker Wrapper
## Hint: export USEDOCKER=TRUE

GITHASH = $(shell git rev-parse --short=8 HEAD)
GITLONGHASH = $(shell git rev-parse HEAD)

MYUID = $(shell id -u)
DOCKERNAME = codabuilder-$(MYUID)

# Unique signature of kademlia code tree
KADEMLIA_SIG = $(shell cd src/app/kademlia-haskell ; find . -type f -print0  | xargs -0 sha1sum | sort | sha1sum | cut -f 1 -d ' ')
LIBP2P_HELPER_SIG = $(shell cd src/app/libp2p_helper ; find . -type f -print0  | xargs -0 sha1sum | sort | sha1sum | cut -f 1 -d ' ')

ifeq ($(DUNE_PROFILE),)
DUNE_PROFILE := dev
endif

TMPDIR ?= /tmp

ifeq ($(USEDOCKER),TRUE)
 $(info INFO Using Docker Named $(DOCKERNAME))
 WRAP = docker exec -it $(DOCKERNAME)
 WRAPAPP = docker exec --workdir /home/opam/app -t $(DOCKERNAME)
else
 $(info INFO Not using Docker)
 WRAP =
endif

########################################
## Coverage directory

COVERAGE_DIR=_coverage

########################################
## Git hooks

git_hooks: $(wildcard scripts/git_hooks/*)
	@case "$$(file .git | cut -d: -f2)" in \
	' ASCII text') \
	    echo 'refusing to install git hooks in worktree' \
	    break;; \
	' directory') \
	    for f in $^; do \
	      [ ! -f ".git/hooks/$$(basename $$f)" ] && ln -s ../../$$f .git/hooks/; \
	    done; \
	    break;; \
	*) \
	    echo 'unhandled case when installing git hooks' \
	    exit 1 \
	    break;; \
	esac

########################################
## Code

all: clean codabuilder containerstart build

clean:
	$(info Removing previous build artifacts)
	@rm -rf _build
	@rm -rf src/$(COVERAGE_DIR)

kademlia:
	@# FIXME: Bash wrap here is awkward but required to get nix-env
	bash -c "source ~/.profile && cd src/app/kademlia-haskell && nix-build release2.nix"

libp2p_helper:
	bash -c "source ~/.profile && cd src/app/libp2p_helper && nix-build default.nix"

# Alias
dht: kademlia libp2p_helper

GENESIS_DIR := $(TMPDIR)/coda_cache_dir

genesis_tar:
	@GENESIS_FILE=$(GENESIS_DIR)/$(shell cat _build/default/src/app/runtime_genesis_ledger/genesis_filename.txt).tar.gz && if [ ! -f $$GENESIS_FILE ] || [ _build/default/src/app/runtime_genesis_ledger/runtime_genesis_ledger.exe -nt $$GENESIS_FILE ]; \
	then ./_build/default/src/app/runtime_genesis_ledger/runtime_genesis_ledger.exe; \
	fi

genesis_ledger:
# compile the tool and write the filename to `genesis_filename.txt`
	$(info Building runtime_genesis_ledger)
	ulimit -s 65532 && (ulimit -n 10240 || true) && $(WRAPAPP) env CODA_COMMIT_SHA1=$(GITLONGHASH) dune build --profile=$(DUNE_PROFILE) src/app/runtime_genesis_ledger/runtime_genesis_ledger.exe src/app/runtime_genesis_ledger/genesis_filename.txt && make genesis_tar
	$(info Genesis ledger and genesis proof generated)

build: git_hooks reformat-diff
	$(info Starting Build)
	ulimit -s 65532 && (ulimit -n 10240 || true) && $(WRAPAPP) env CODA_COMMIT_SHA1=$(GITLONGHASH) dune build src/app/logproc/logproc.exe src/app/cli/src/coda.exe --profile=$(DUNE_PROFILE) && make genesis_ledger
	$(info Build complete)

build_archive: git_hooks reformat-diff
	$(info Starting Build)
	ulimit -s 65532 && (ulimit -n 10240 || true) && dune build src/app/archive/archive.exe --profile=$(DUNE_PROFILE)
	$(info Build complete)

dev: codabuilder containerstart build

# update OPAM, pinned packages in Docker
update-opam:
	$(WRAPAPP) ./scripts/update-opam-in-docker.sh

macos-portable:
	@rm -rf _build/coda-daemon-macos/
	@rm -rf _build/coda-daemon-macos.zip
	@./scripts/macos-portable.sh src/_build/default/src/app/cli/src/coda.exe src/app/kademlia-haskell/result/bin/kademlia _build/coda-daemon-macos
	@zip -r _build/coda-daemon-macos.zip _build/coda-daemon-macos/
	@echo Find coda-daemon-macos.zip inside _build/

update-graphql:
	@echo Make sure that the daemon is running with -rest-port 8080
	python scripts/introspection_query.py > graphql_schema.json

########################################
## Lint

reformat: git_hooks
	$(WRAPAPP) dune exec --profile=$(DUNE_PROFILE) src/app/reformat/reformat.exe -- -path .

reformat-diff:
	ocamlformat --doc-comments=before --inplace $(shell git status -s | cut -c 4- | grep '\.mli\?$$' | while IFS= read -r f; do stat "$$f" >/dev/null 2>&1 && echo "$$f"; done) || true

check-format:
	$(WRAPAPP) dune exec --profile=$(DUNE_PROFILE) src/app/reformat/reformat.exe -- -path . -check

check-snarky-submodule:
	./scripts/check-snarky-submodule.sh

########################################
## Merlin fixup for docker builds

merlin-fixup:
ifeq ($(USEDOCKER),TRUE)
	@echo "Fixing up .merlin files for Docker build"
	@./scripts/merlin-fixup.sh
else
	@echo "Not building in Docker, .merlin files unchanged"
endif

#######################################
## Environment setup

macos-setup-download:
	./scripts/macos-setup-brew.sh

macos-setup-compile:
	./scripts/macos-setup-opam.sh

macos-setup:
	./scripts/macos-setup-brew.sh
	./scripts/macos-setup-opam.sh

########################################
## Containers and container management


# push steps require auth on docker hub
docker-toolchain:
	@if git diff-index --quiet HEAD ; then \
		docker build --no-cache --file dockerfiles/Dockerfile-toolchain --tag codaprotocol/coda:toolchain-$(GITLONGHASH) . && \
		docker tag  codaprotocol/coda:toolchain-$(GITLONGHASH) codaprotocol/coda:toolchain-latest && \
		docker push codaprotocol/coda:toolchain-$(GITLONGHASH) && \
		docker push codaprotocol/coda:toolchain-latest ;\
	else \
		echo "Repo has uncommited changes, commit first to set hash." ;\
	fi

docker-toolchain-rust:
	@if git diff-index --quiet HEAD ; then \
		docker build --file dockerfiles/Dockerfile-toolchain-rust --tag codaprotocol/coda:toolchain-rust-$(GITLONGHASH) . && \
		docker tag  codaprotocol/coda:toolchain-rust-$(GITLONGHASH) codaprotocol/coda:toolchain-rust-latest && \
		docker push codaprotocol/coda:toolchain-rust-$(GITLONGHASH) && \
		docker push codaprotocol/coda:toolchain-rust-latest ;\
	else \
		echo "Repo has uncommited changes, commit first to set hash." ;\
	fi

# All in one step to build toolchain and binary for kademlia
# TODO: Rename to docker-toolchain-discovery
docker-toolchain-haskell:
	@echo "Building codaprotocol/coda:toolchain-haskell-$(KADEMLIA_SIG)" ;\
    docker build --file dockerfiles/Dockerfile-toolchain-haskell --tag codaprotocol/coda:toolchain-haskell-$(KADEMLIA_SIG) . ;\
    echo  'Extracting deb package' ;\
    mkdir -p _build ;\
    docker run --rm --entrypoint cat codaprotocol/coda:toolchain-haskell-$(KADEMLIA_SIG) /src/coda-discovery.deb > _build/coda-discovery.deb

update-deps:
	./scripts/update-toolchain-references.sh $(GITLONGHASH)
	make render-circleci

update-rust-deps:
	./scripts/update-rust-toolchain-references.sh $(GITLONGHASH)
	make render-circleci

# Local 'codabuilder' docker image (based off docker-toolchain)
codabuilder: git_hooks
	docker build --file dockerfiles/Dockerfile --tag codabuilder .

# Restarts codabuilder
containerstart: git_hooks
	@./scripts/container.sh restart

########################################
## Artifacts

publish-macos:
	@./scripts/publish-macos.sh

deb:
	$(WRAP) ./scripts/rebuild-deb.sh
	@mkdir -p /tmp/artifacts
	@cp _build/coda*.deb /tmp/artifacts/.
	@cp _build/coda_pvkeys_* /tmp/artifacts/.

publish_deb:
	@./scripts/publish-deb.sh

publish_debs: publish_deb

genesiskeys:
	@mkdir -p /tmp/artifacts
	@cp _build/default/src/lib/coda_base/sample_keypairs.ml /tmp/artifacts/.
	@cp _build/default/src/lib/coda_base/sample_keypairs.json /tmp/artifacts/.

codaslim:
	@# FIXME: Could not reference .deb file in the sub-dir in the docker build
	@cp _build/coda.deb .
	@./scripts/rebuild-docker.sh codaslim dockerfiles/Dockerfile-codaslim
	@rm coda.deb

##############################################
## Genesis ledger in OCaml from running daemon

genesis-ledger-ocaml:
	@./scripts/generate-genesis-ledger.py .genesis-ledger.ml.jinja

########################################
## Tests

render-circleci:
	./scripts/test.py render .circleci/config.yml.jinja .mergify.yml.jinja

test-ppx:
	$(MAKE) -C src/lib/ppx_coda/tests

web:
	./scripts/web.sh


########################################
## Benchmarks

benchmarks:
	dune build app/benchmarks/main.exe


########################################
# Coverage testing and output

test-coverage: SHELL := /bin/bash
test-coverage:
	source scripts/test_all.sh ; run_unit_tests_with_coverage

# we don't depend on test-coverage, which forces a run of all unit tests
coverage-html:
ifeq ($(shell find _build/default -name bisect\*.out),"")
	echo "No coverage output; run make test-coverage"
else
	bisect-ppx-report -I _build/default/ -html $(COVERAGE_DIR) `find . -name bisect\*.out`
endif

coverage-text:
ifeq ($(shell find _build/default -name bisect\*.out),"")
	echo "No coverage output; run make test-coverage"
else
	bisect-ppx-report -I _build/default/ -text $(COVERAGE_DIR)/coverage.txt `find . -name bisect\*.out`
endif

coverage-coveralls:
ifeq ($(shell find _build/default -name bisect\*.out),"")
	echo "No coverage output; run make test-coverage"
else
	bisect-ppx-report -I _build/default/ -coveralls $(COVERAGE_DIR)/coveralls.json `find . -name bisect\*.out`
endif

########################################
# Diagrams for documentation

docs/res/%.dot.png: docs/res/%.dot
	dot -Tpng $< > $@

docs/res/%.tex.pdf: docs/res/%.tex
	cd docs/res && pdflatex $(notdir $<)
	cp $(@:.tex.pdf=.pdf) $@

docs/res/%.tex.png: docs/res/%.tex.pdf
	convert -density 600x600 $< -quality 90 -resize 1080x1080 $@

doc_diagrams: $(addsuffix .png,$(wildcard docs/res/*.tex) $(wildcard docs/res/*.dot) $(wildcard rfcs/res/*.dot))

########################################
# Generate odoc documentation

ml-docs:
	$(WRAPAPP) dune build --profile=$(DUNE_PROFILE) @doc

########################################
# To avoid unintended conflicts with file names, always add to .PHONY
# unless there is a reason not to.
# https://www.gnu.org/software/make/manual/html_node/Phony-Targets.html
# HACK: cat Makefile | egrep '^\w.*' | sed 's/:/ /' | awk '{print $1}' | grep -v myprocs | sort | xargs
.PHONY: all base-docker base-googlecloud base-minikube build check-format ci-base-docker clean codaslim containerstart deb dev codabuilder kademlia coda-docker coda-googlecloud coda-minikube ocaml407-googlecloud pull-ocaml407-googlecloud reformat test test-all test-coda-block-production-sig test-coda-block-production-stake test-codapeers-sig test-codapeers-stake test-full-sig test-full-stake test-runtest test-transaction-snark-profiler-sig test-transaction-snark-profiler-stake update-deps render-circleci check-render-circleci docker-toolchain-rust toolchains doc_diagrams ml-docs macos-setup macos-setup-download macos-setup-compile