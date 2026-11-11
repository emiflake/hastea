# Runtime

The JavaScript that every page needs. It is built into the `hastea` command (`Hastea.Cli.Runtime`), which writes it next to the module in `hastea build`, so that an application only brings its own HTML and styles.

## index.js

Instantiates `hastea.wasm` with the WASI shim and the FFI glue from GHC's post-linker (`ghc_wasm_jsffi.js`), and calls `hs_start`, the entry point that the application's `Main` exports. An application can replace it with an `index.js` of its own in `www/`.

## dev.js

What the page loads instead of `index.js` in `hastea dev`: it asks the command for a GHCi session, boots GHCi's interpreter in the page, and follows the command's events (show a compilation error, replace the stylesheets, load the page again).

## browser_wasi_shim.js

[browser_wasi_shim](https://github.com/bjorn3/browser_wasi_shim) 0.4.2 by bjorn3, licensed under MIT OR Apache-2.0. It provides the WASI system calls that a module built by GHC's WebAssembly backend imports.

The package is published as several small modules. They are bundled into one file here, unminified, so that a page fetches it in a single request and needs nothing from another host. To produce it again, for a new version say:

```bash
npm install @bjorn3/browser_wasi_shim@0.4.2 esbuild
npx esbuild node_modules/@bjorn3/browser_wasi_shim/dist/index.js --bundle --format=esm --outfile=browser_wasi_shim.js
```

Then put the comment that names the version and the license back at the top.
