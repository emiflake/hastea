# Vendored code

Code from other projects that is shipped with every page, kept here so that a page needs nothing from another host.

## browser_wasi_shim.js

[browser_wasi_shim](https://github.com/bjorn3/browser_wasi_shim) 0.4.2 by bjorn3, licensed under MIT OR Apache-2.0. It provides the WASI system calls that a module built by GHC's WebAssembly backend imports. `build.sh` copies it into `dist/`, where `index.js` loads it.

The package is published as several small modules. They are bundled into one file here, unminified, so that a page fetches it in a single request. To produce it again, for a new version say:

```bash
npm install @bjorn3/browser_wasi_shim@0.4.2 esbuild
npx esbuild node_modules/@bjorn3/browser_wasi_shim/dist/index.js --bundle --format=esm --outfile=browser_wasi_shim.js
```

Then put the comment that names the version and the license back at the top.
