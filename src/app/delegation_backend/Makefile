.PHONY: clean build

build:
	$(WRAPAPP) ../../../scripts/build-go-helper.sh delegation_backend

clean:
	rm -rf result

tidy:
	bash -c 'cd src; go mod tidy'
