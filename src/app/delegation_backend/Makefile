.PHONY: clean build test tidy docker

ifeq ($(GO),)
GO := go
endif

build:
	GO=$(GO) ./scripts/build.sh

clean:
	rm -rf result

tidy:
	cd src && $(GO) mod tidy

test:
	GO=$(GO) ./scripts/build.sh test

docker:
	mkdir -p result && docker build -t delegation-backend-production -f Dockerfile.production . && docker save delegation-backend-production | gzip > result/delegation_backend.tar.gz
