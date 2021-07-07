.PHONY: clean build

ifeq ($(GO),)
GO := go
endif

build:
	$(WRAPAPP) ../../../scripts/build-go-helper.sh delegation_backend

clean:
	rm -rf result

tidy:
	bash -c 'cd src; $(GO) mod tidy'

test:
	bash -c 'cd src; $(GO) test'
