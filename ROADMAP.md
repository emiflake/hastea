# Hastea roadmap

This lists the gaps between Hastea and React that are in our power to close. It leaves out what is not: the size of React's ecosystem, its JavaScript-native toolchain, and the size of GHC's WebAssembly runtime.

Items are grouped by how much they hurt a real application. The performance items come from [the benchmark](benchmark/README.md) and from timing the parts of a render once.

## Things you hit in the first real application

- [x] **Acting when a component appears or disappears.** `onMount` and `onUnmount`.
- [ ] **Effects** (partly done). There are timers, focus and HTTP. Still missing: routing and URL handling, local storage, keyboard and window subscriptions.
- [x] **Fragments.** `fragment`.
- [ ] **Portals**, to show a modal or a tooltip elsewhere on the page.
- [x] **SVG.** `Hastea.Svg`; elements inside `svg` are created in the SVG namespace.
- [ ] **DOM access** beyond focus and blur by id: measuring an element, scrolling to it, holding a reference to it.

## Robustness

- [x] **Errors vanish.** Failures in `update` and `view` are now caught and reported, with the page left as it was, and a decoder that fails gives a warning.
- [x] **Mistakes are not detected.** Duplicate keys and a fragment directly inside `lazy` now give a warning.
- [ ] **A `lazy` that never skips gives no hint** that it is not working.
- [ ] **Event lookup trusts the page.** If something else inserts a node under the root (a browser extension, say), events can miss their handlers. React ties each DOM node to its component directly.
- [ ] **Controlled inputs are naive.** `value` is assigned again on every render, with no special handling of text composition (IME).
- [ ] **Component identity is loose.** A different component with the same type of state in the same place inherits the old state.
- [ ] **Error boundaries per component**, so that one broken part does not take the page with it.

## Performance

- [x] **A benchmark**: [benchmark/](benchmark/README.md) measures a table of rows against React. Hastea started out 4 to 12 times slower and is now about twice as slow.
- [x] **Passing strings to JavaScript was the largest cost of building the page.** Text is now `Data.Text`, which is handed to JavaScript as one block of UTF-8.
- [x] **Each node was created by its own call.** New nodes are now described in one piece of text and built by a single call.
- [x] **Adding, removing and moving children was quadratic.** Rearranging the children of a node is now one call, done in JavaScript.
- [ ] **An unchanged list is still walked in full.** Selecting one row of 1,000 takes 11 ms in the benchmark, against 3 ms for React, though only two rows change.
- [ ] **Every render expands the whole tree again** before diffing, apart from `lazy` subtrees.
- [ ] **Rendering is all-or-nothing per frame.** Long renders cannot be interrupted or given a priority.
- [ ] **Download size** (partly done). The benchmark page is a 2.3 MB wasm module. `hastea build --optimize` (or `optimize: true` in `hastea.yaml`) runs `wasm-opt -Oz`, which brings TodoMVC from 2.2 MB to 1.2 MB; it is not on by default yet. Gzip, which halves it again, is up to the server. The page no longer depends on a CDN: the WASI shim is bundled and everything is fetched at once.

## Developer experience

- [ ] **Devtools** to inspect components, state and messages.
- [ ] **Hot reload** (partly done). GHC's wasm GHCi can run the program in the page itself (`-fghci-browser`), where `:reload` takes well under a second. Running `main` again now replaces the application instead of stacking a second one, since mounting unmounts whatever root was there. `hastea dev` sets all of this up and reloads on every change. Still missing: keeping the model across a reload.
- [ ] **Syntax friction** (partly done). Element names no longer clash with ordinary names, now that the examples import them qualified (`H.div`). Still there: `use counter ()` needs the unit, and `App` and `Component` share field names.
- [ ] **Attaching to server-rendered markup.** `renderHtml` produces markup, but the runtime cannot take it over.
- [ ] **A testing helper for users.** The stand-in root that drives components without a page lives only in our own test suite.
- [ ] **Documentation.** Two examples and a README; no tutorial, no published API documentation.
- [ ] **Continuous integration.**

## Suggested order

1.  Mount and unmount effects for components, and HTTP: without these most real applications cannot be written.
2.  Fragments and SVG: both are small changes to the tree and to how it is committed.
3.  Error reporting and development warnings: cheap, and they make everything else easier to debug.
4.  A benchmark, before touching performance.
