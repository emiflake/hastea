#
# Builds Hastea with GHC's WebAssembly backend.
#
# Needs the wasm32-wasi GHC cross-compiler from ghc-wasm-meta:
#   https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta
# See README.org for setup instructions.
#
GHC      ?= wasm32-wasi-ghc
DIST_DIR ?= ./dist
OUT_DIR  ?= ./build
PORT     ?= 8080

GHC_FLAGS = -isrc -O2 -outputdir $(OUT_DIR) \
	-no-hs-main -optl-mexec-model=reactor -optl-Wl,--export=hs_start

POST_LINK = $(shell $(GHC) --print-libdir)/post-link.mjs

all: build

build: $(DIST_DIR)
	$(GHC) $(GHC_FLAGS) src/Main.hs -o $(DIST_DIR)/hastea.wasm
	$(POST_LINK) -i $(DIST_DIR)/hastea.wasm -o $(DIST_DIR)/ghc_wasm_jsffi.js
	cp index.html index.css index.js $(DIST_DIR)/

$(DIST_DIR):
	mkdir -p $(DIST_DIR)

serve: build
	cd $(DIST_DIR) && python3 -m http.server $(PORT)

clean:
	rm -rf $(OUT_DIR) $(DIST_DIR)

.PHONY: all build serve clean
