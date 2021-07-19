########################################
## Docker Wrapper
## Hint: export USEDOCKER=TRUE

MYUID = $(shell id -u)
DOCKERNAME = minabuilder-$(MYUID)

ifeq ($(USEDOCKER),TRUE)
 $(info INFO Using Docker Named $(DOCKERNAME))
 WRAP = docker exec -it $(DOCKERNAME)
 WRAPAPP = docker exec --workdir /home/opam/app -t $(DOCKERNAME)
else
 WRAP =
endif

########################################
## Configuration

# Current OCaml version
OCAML_VERSION = "4.11.2"

# machine word size
WORD_SIZE = "64"

# Default profile
ifeq ($(DUNE_PROFILE),)
DUNE_PROFILE := dev
endif

# Temp directory
TMPDIR ?= /tmp

# Genesis dir
GENESIS_DIR := $(TMPDIR)/coda_cache_dir

# Coverage directory
COVERAGE_DIR=_coverage

########################################
## Handy variables

# This commit hash
GITHASH = $(shell git rev-parse --short=8 HEAD)
GITLONGHASH = $(shell git rev-parse HEAD)

# Unique signature of libp2p code tree
LIBP2P_HELPER_SIG = $(shell cd src/app/libp2p_helper ; find . -type f -print0  | xargs -0 sha1sum | sort | sha1sum | cut -f 1 -d ' ')

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

all: clean minabuilder containerstart build

clean:
	$(info Removing previous build artifacts)
	@rm -rf _build
	@rm -rf src/$(COVERAGE_DIR)
	@rm -rf src/app/libp2p_helper/result

# enforces the OCaml version being used
ocaml_version:
	@if ! ocamlopt -config | grep "version:" | grep $(OCAML_VERSION); then echo "incorrect OCaml version, expected version $(OCAML_VERSION)" ; exit 1; fi

# enforce machine word size
ocaml_word_size:
	@if ! ocamlopt -config | grep "word_size:" | grep $(WORD_SIZE); then echo "invalid machine word size, expected $(WORD_SIZE)" ; exit 1; fi

ocaml_checks: ocaml_version ocaml_word_size

libp2p_helper:
	make -C src/app/libp2p_helper

genesis_ledger: ocaml_checks
	$(info Building runtime_genesis_ledger)
	ulimit -s 65532 && (ulimit -n 10240 || true) && $(WRAPAPP) env MINA_COMMIT_SHA1=$(GITLONGHASH) dune exec --profile=$(DUNE_PROFILE) src/app/runtime_genesis_ledger/runtime_genesis_ledger.exe -- --genesis-dir $(GENESIS_DIR)
	$(info Genesis ledger and genesis proof generated)

build: ocaml_checks git_hooks reformat-diff libp2p_helper
	$(info Starting Build)
	ulimit -s 65532 && (ulimit -n 10240 || true) && $(WRAPAPP) env MINA_COMMIT_SHA1=$(GITLONGHASH) dune build src/app/logproc/logproc.exe src/app/cli/src/mina.exe --profile=$(DUNE_PROFILE)
	$(info Build complete)

build_all_sigs: ocaml_checks git_hooks reformat-diff libp2p_helper
	$(info Starting Build)
	ulimit -s 65532 && (ulimit -n 10240 || true) && $(WRAPAPP) env MINA_COMMIT_SHA1=$(GITLONGHASH) dune build src/app/logproc/logproc.exe src/app/cli/src/mina.exe src/app/cli/src/mina_testnet_signatures.exe src/app/cli/src/mina_mainnet_signatures.exe --profile=$(DUNE_PROFILE)
	$(info Build complete)

build_archive: ocaml_checks git_hooks reformat-diff
	$(info Starting Build)
	ulimit -s 65532 && (ulimit -n 10240 || true) && dune build src/app/archive/archive.exe --profile=$(DUNE_PROFILE)
	$(info Build complete)

build_archive_all_sigs: ocaml_checks git_hooks reformat-diff
	$(info Starting Build)
	ulimit -s 65532 && (ulimit -n 10240 || true) && dune build src/app/archive/archive.exe src/app/archive/archive_testnet_signatures.exe src/app/archive/archive_mainnet_signatures.exe --profile=$(DUNE_PROFILE)
	$(info Build complete)

build_rosetta: ocaml_checks
	$(info Starting Build)
	ulimit -s 65532 && (ulimit -n 10240 || true) && dune build src/app/archive/archive.exe src/app/rosetta/rosetta.exe src/app/rosetta/ocaml-signer/signer.exe --profile=$(DUNE_PROFILE)
	$(info Build complete)

build_rosetta_all_sigs: ocaml_checks
	$(info Starting Build)
	ulimit -s 65532 && (ulimit -n 10240 || true) && dune build src/app/archive/archive.exe src/app/archive/archive_testnet_signatures.exe src/app/archive/archive_mainnet_signatures.exe src/app/rosetta/rosetta.exe src/app/rosetta/rosetta_testnet_signatures.exe src/app/rosetta/rosetta_mainnet_signatures.exe src/app/rosetta/ocaml-signer/signer.exe src/app/rosetta/ocaml-signer/signer_testnet_signatures.exe src/app/rosetta/ocaml-signer/signer_mainnet_signatures.exe --profile=$(DUNE_PROFILE)
	$(info Build complete)

build_intgtest: ocaml_checks
	$(info Starting Build)
	dune build --profile=integration_tests src/app/test_executive/test_executive.exe src/app/logproc/logproc.exe
	$(info Build complete)

client_sdk: ocaml_checks
	$(info Starting Build)
	ulimit -s 65532 && (ulimit -n 10240 || true) && dune build src/app/client_sdk/client_sdk.bc.js --profile=nonconsensus_mainnet
	$(info Build complete)

client_sdk_test_sigs: ocaml_checks
	$(info Starting Build)
	ulimit -s 65532 && (ulimit -n 10240 || true) && dune build src/app/client_sdk/tests/test_signatures.exe --profile=mainnet
	$(info Build complete)

client_sdk_test_sigs_nonconsensus: ocaml_checks
	$(info Starting Build)
	ulimit -s 65532 && (ulimit -n 10240 || true) && dune build src/app/client_sdk/tests/test_signatures_nonconsensus.exe --profile=nonconsensus_mainnet
	$(info Build complete)

rosetta_lib_encodings: ocaml_checks
	$(info Starting Build)
	ulimit -s 65532 && (ulimit -n 10240 || true) && dune build src/lib/rosetta_lib/test/test_encodings.exe --profile=mainnet
	$(info Build complete)

rosetta_lib_encodings_nonconsensus: ocaml_checks
	$(info Starting Build)
	ulimit -s 65532 && (ulimit -n 10240 || true) && dune build src/nonconsensus/rosetta_lib/test/test_encodings.exe --profile=nonconsensus_mainnet
	$(info Build complete)

dhall_types: ocaml_checks
	$(info Starting Build)
	ulimit -s 65532 && (ulimit -n 10240 || true) && dune build src/app/dhall_types/dump_dhall_types.exe --profile=dev
	$(info Build complete)

replayer: ocaml_checks
	$(info Starting Build)
	ulimit -s 65532 && (ulimit -n 10240 || true) && dune build src/app/replayer/replayer.exe --profile=testnet_postake_medium_curves
	$(info Build complete)

delegation_compliance: ocaml_checks
	$(info Starting Build)
	ulimit -s 65532 && (ulimit -n 10240 || true) && dune build src/app/delegation_compliance/delegation_compliance.exe --profile=testnet_postake_medium_curves
	$(info Build complete)

missing_blocks_auditor: ocaml_checks
	$(info Starting Build)
	ulimit -s 65532 && (ulimit -n 10240 || true) && dune build src/app/missing_blocks_auditor/missing_blocks_auditor.exe --profile=testnet_postake_medium_curves
	$(info Build complete)

extract_blocks: ocaml_checks
	$(info Starting Build)
	ulimit -s 65532 && (ulimit -n 10240 || true) && dune build src/app/extract_blocks/extract_blocks.exe --profile=testnet_postake_medium_curves
	$(info Build complete)

archive_blocks: ocaml_checks
	$(info Starting Build)
	ulimit -s 65532 && (ulimit -n 10240 || true) && dune build src/app/archive_blocks/archive_blocks.exe --profile=testnet_postake_medium_curves
	$(info Build complete)

patch_archive_test: ocaml_checks
	$(info Starting Build)
	ulimit -s 65532 && (ulimit -n 10240 || true) && dune build src/app/patch_archive_test/patch_archive_test.exe --profile=testnet_postake_medium_curves
	$(info Build complete)

genesis_ledger_from_tsv: ocaml_checks
	$(info Starting Build)
	ulimit -s 65532 && (ulimit -n 10240 || true) && dune build src/app/genesis_ledger_from_tsv/genesis_ledger_from_tsv.exe --profile=testnet_postake_medium_curves
	$(info Build complete)

swap_bad_balances: ocaml_checks
	$(info Starting Build)
	ulimit -s 65532 && (ulimit -n 10240 || true) && dune build src/app/swap_bad_balances/swap_bad_balances.exe --profile=testnet_postake_medium_curves
	$(info Build complete)

dev: minabuilder containerstart build

# update OPAM, pinned packages in Docker
update-opam:
	$(WRAPAPP) ./scripts/update-opam-in-docker.sh

macos-portable:
	@rm -rf _build/coda-daemon-macos/
	@rm -rf _build/coda-daemon-macos.zip
	@./scripts/macos-portable.sh _build/default/src/app/cli/src/mina.exe src/app/libp2p_helper/result/bin/libp2p_helper _build/coda-daemon-macos
	@cp -a package/keys/. _build/coda-daemon-macos/keys/
	@cd _build/coda-daemon-macos && zip -r ../coda-daemon-macos.zip .
	@echo Find coda-daemon-macos.zip inside _build/

update-graphql:
	@echo Make sure that the daemon is running with -rest-port 8080
	python3 scripts/introspection_query.py > graphql_schema.json

########################################
## Lint

reformat: ocaml_checks git_hooks
	$(WRAPAPP) dune exec --profile=$(DUNE_PROFILE) src/app/reformat/reformat.exe -- -path .

reformat-diff:
	@ocamlformat --doc-comments=before --inplace $(shell git status -s | cut -c 4- | grep '\.mli\?$$' | while IFS= read -r f; do stat "$$f" >/dev/null 2>&1 && echo "$$f"; done) || true

check-format: ocaml_checks
	$(WRAPAPP) dune exec --profile=$(DUNE_PROFILE) src/app/reformat/reformat.exe -- -path . -check

check-snarky-submodule:
	./scripts/check-snarky-submodule.sh

#######################################
## Environment setup

macos-setup-download:
	./scripts/macos-setup-brew.sh

setup-opam:
	eval $$(opam config env) && ./scripts/setup-opam.sh

macos-setup:
	./scripts/macos-setup-brew.sh
	./scripts/setup-opam.sh

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

update-deps:
	./scripts/update-toolchain-references.sh $(GITLONGHASH)

update-rust-deps:
	./scripts/update-rust-toolchain-references.sh $(GITLONGHASH)

# Local 'minabuilder' docker image (based off docker-toolchain)
minabuilder: git_hooks
	docker build --file dockerfiles/Dockerfile-toolchain --tag minabuilder .

# Restarts minabuilder
containerstart: git_hooks
	@./scripts/container.sh restart

docker-rosetta:
	docker build --file dockerfiles/Dockerfile-rosetta --tag codaprotocol/mina:rosetta-$(GITLONGHASH) .

########################################
## Artifacts

publish-macos:
	@./scripts/publish-macos.sh

deb:
	$(WRAP) ./scripts/rebuild-deb.sh
	$(WRAP) ./scripts/archive/build-release-archives.sh

build_pv_keys: ocaml_checks
	$(info Building keys)
	ulimit -s 65532 && (ulimit -n 10240 || true) && $(WRAPAPP) env MINA_COMMIT_SHA1=$(GITLONGHASH) dune exec --profile=$(DUNE_PROFILE) src/lib/snark_keys/gen_keys/gen_keys.exe -- --generate-keys-only
	$(info Keys built)

build_or_download_pv_keys: ocaml_checks
	$(info Building keys)
	ulimit -s 65532 && (ulimit -n 10240 || true) && $(WRAPAPP) env MINA_COMMIT_SHA1=$(GITLONGHASH) dune exec --profile=$(DUNE_PROFILE) src/lib/snark_keys/gen_keys/gen_keys.exe -- --generate-keys-only
	$(info Keys built)

publish_deb:
	@./scripts/publish-deb.sh

publish_debs:
	@./buildkite/scripts/publish-deb.sh

genesiskeys:
	@mkdir -p /tmp/artifacts
	@cp _build/default/src/lib/mina_base/sample_keypairs.ml /tmp/artifacts/.
	@cp _build/default/src/lib/mina_base/sample_keypairs.json /tmp/artifacts/.

##############################################
## Genesis ledger in OCaml from running daemon

genesis-ledger-ocaml:
	@./scripts/generate-genesis-ledger.py .genesis-ledger.ml.jinja

########################################
## Tests

test-ppx:
	$(MAKE) -C src/lib/ppx_coda/tests

web:
	./scripts/web.sh

########################################
## Benchmarks

benchmarks: ocaml_checks
	dune build src/app/benchmarks/main.exe

########################################
# Coverage testing and output

test-coverage: SHELL := /bin/bash
test-coverage: libp2p_helper
	scripts/create_coverage_profiles.sh

# we don't depend on test-coverage, which forces a run of all unit tests
coverage-html:
ifeq ($(shell find _build/default -name bisect\*.out),"")
	echo "No coverage output; run make test-coverage"
else
	bisect-ppx-report html --source-path=_build/default --coverage-path=_build/default
endif

coverage-summary:
ifeq ($(shell find _build/default -name bisect\*.out),"")
	echo "No coverage output; run make test-coverage"
else
	bisect-ppx-report summary --coverage-path=_build/default --per-file
endif

########################################
# Diagrams for documentation

%.dot.png: %.dot
	dot -Tpng $< > $@

%.tex.pdf: %.tex
	cd $(dir $@) && pdflatex -halt-on-error $(notdir $<)
	cp $(@:.tex.pdf=.pdf) $@

%.tex.png: %.tex.pdf
	convert -density 600x600 $< -quality 90 -resize 1080x1080 $@

%.conv.tex.png: %.conv.tex
	cd $(dir $@) && pdflatex -halt-on-error -shell-escape $(notdir $<)

doc_diagram_sources=$(addprefix docs/res/,*.dot *.tex *.conv.tex)
doc_diagram_sources+=$(addprefix rfcs/res/,*.dot *.tex *.conv.tex)
doc_diagrams: $(addsuffix .png,$(wildcard $(doc_diagram_sources)))

########################################
# Generate odoc documentation

ml-docs: ocaml_checks
	$(WRAPAPP) dune build --profile=$(DUNE_PROFILE) @doc

########################################
# To avoid unintended conflicts with file names, always add new targets to .PHONY
# unless there is a reason not to.
# https://www.gnu.org/software/make/manual/html_node/Phony-Targets.html
# HACK: cat Makefile | egrep '^\w.*' | sed 's/:/ /' | awk '{print $1}' | grep -v myprocs | sort | xargs

.PHONY: all build check-format clean client_sdk client_sdk_test_sigs containerstart deb dev minabuilder mina-docker reformat update-deps docker-toolchain-rust toolchains doc_diagrams ml-docs macos-setup macos-setup-download setup-opam libp2p_helper dhall_types replayer missing_blocks_auditor extract_blocks archive_blocks genesis_ledger_from_tsv ocaml_version ocaml_word_size ocaml_checks
