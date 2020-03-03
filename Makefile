#
# Depends on ahc-link which you can get here
#   https://asterius.netlify.com/building.html
#
# You can use the docker for compilation, but note it does not have Makefile by default
#
DIST_DIR= ./dist


all: build

server: copy-bins
	cd $(DIST_DIR) && warp -v

copy-bins:
	cp index.html $(DIST_DIR)

clean:
	find . -name *.hi -exec rm {} \;
	find . -name *.o -exec rm {} \;

BUILD_CMD=ahc-link --input-hs src/Main.hs --bundle --browser --output-dir=$(DIST_DIR)
docker-build:
	@echo "Booting docker to compile it"
	docker run --rm -it -v $(shell pwd):/hastea -w /hastea terrorjack/asterius /bin/bash -c "$(BUILD_CMD)"

build:
	$(BUILD_CMD)

