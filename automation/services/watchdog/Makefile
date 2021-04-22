filepath        :=      $(PWD)
versionfile     :=      $(filepath)/version.txt
version         :=      $(shell cat $(versionfile))
branch          :=      $(shell git rev-parse --abbrev-ref HEAD)

release: 
	docker build -t codaprotocol/watchdog:$(version) -f Dockerfile  .
	docker tag codaprotocol/watchdog:$(version) gcr.io/o1labs-192920/watchdog:$(version)
	docker tag codaprotocol/watchdog:$(version) gcr.io/o1labs-192920/watchdog:$(version)-${branch}
	docker push gcr.io/o1labs-192920/watchdog:$(version)
	docker push gcr.io/o1labs-192920/watchdog:$(version)-${branch}

latest:
	docker tag codaprotocol/watchdog:$(version) gcr.io/o1labs-192920/watchdog:latest
	docker push gcr.io/o1labs-192920/watchdog:latest

build: 
	docker build -t codaprotocol/watchdog:$(version) -f Dockerfile .
