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

- [x] **A benchmark**: [benchmark/](benchmark/README.md) measures a table of rows against React. Hastea is 3 to 12 times slower.
- [ ] **Passing strings to JavaScript is the largest cost of building the page**: 5.6 µs for 20 characters, where a call without a string takes 0.1 µs. A row of the benchmark needs 17 of them, most of them tag and attribute names that never change.
- [ ] **Each node is created by its own call**, and holding on to it from Haskell costs 3.7 µs. Building a whole subtree in one call to JavaScript would avoid both this and most of the strings.
- [ ] **Adding, removing and moving children is quadratic.** Carrying out the patch looks children up by position in a linked list, and each child costs one DOM call to place. The benchmark shows it: 10,000 rows take 14 times as long as 1,000.
- [ ] **An unchanged list is still walked in full.** Selecting one row of 1,000 takes 20 ms in the benchmark, though only two rows change.
- [ ] **Every render expands the whole tree again** before diffing, apart from `lazy` subtrees.
- [ ] **Download size.** The benchmark page is a 2.1 MB wasm module; nothing has been done yet to shrink it (optimisation flags, `wasm-opt`, compression).
- [ ] **Rendering is all-or-nothing per frame.** Long renders cannot be interrupted or given a priority.

## Developer experience

- [ ] **Devtools** to inspect components, state and messages.
- [ ] **Hot reload.** Every change is a slow recompile and a full page reload that loses state.
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
