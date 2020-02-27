#
# Depends on ahc-link which you can get here
#   https://asterius.netlify.com/building.html
#


all: build

server:
	wark -v

build:
	ahc-link --input-hs Main.hs --bundle --browser
