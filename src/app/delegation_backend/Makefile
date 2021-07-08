.PHONY: clean build test tidy docker

ifeq ($(GO),)
GO := go
endif

build:
	mkdir -p result/bin && cd src/delegation_backend && $(GO) build -o ../../result/bin/delegation_backend

clean:
	rm -rf result

tidy:
	cd src && $(GO) mod tidy

test:
	cd src && $(GO) test

docker:
	mkdir -p result && docker build -t delegation-backend-production -f Dockerfile.production . && docker save delegation-backend-production | gzip > result/delegation_backend.tar.gz
