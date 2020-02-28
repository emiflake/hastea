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



build:
	ahc-link --input-hs Main.hs --bundle --browser --output-dir=$(DIST_DIR)
