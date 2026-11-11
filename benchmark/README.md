# Benchmark

The table of rows that web frameworks are commonly measured with, written with Hastea ([`Main.hs`](Main.hs)) and with React 18 ([`react/app.js`](react/app.js)). Both versions give each row a key and skip rows that did not change (`lazy` and `memo`).

[`run.cjs`](run.cjs) opens both pages in headless Chromium, performs each operation several times, and reports the median time from the click until the browser has painted the result.

## Running it

```bash
./build.sh benchmark               # in the repository root
cd benchmark
npm install                        # React, to compare with
node run.cjs                       # or: node run.cjs 20, for 20 runs of each operation
```

It needs [Playwright](https://playwright.dev) installed where Node can find it.

## Results

Measured on a 4-core Intel Xeon virtual machine, with GHC 9.14.1 at `-O1` and React 18.3.1 (production build). The numbers will differ on other machines; the ratios are what to look at.

Median of 10 runs, in milliseconds, from the click until the browser has painted.

| Operation | Hastea | React | Hastea / React |
| --- | ---: | ---: | ---: |
| Load the page | 60.0 | 41.0 | 1.5× |
| Create 1,000 rows | 74.6 | 34.8 | 2.1× |
| Replace all 1,000 rows | 71.8 | 37.6 | 1.9× |
| Update every 10th row of 1,000 | 32.3 | 12.6 | 2.6× |
| Select a row of 1,000 | 11.4 | 2.6 | 4.3× |
| Swap two rows of 1,000 | 43.1 | 29.9 | 1.4× |
| Remove a row of 1,000 | 17.0 | 8.2 | 2.1× |
| Append 1,000 rows to 1,000 | 91.5 | 46.4 | 2.0× |
| Clear 1,000 rows | 4.9 | 6.8 | 0.7× |
| Create 10,000 rows | 692.1 | 522.2 | 1.3× |

| Download | Size |
| --- | ---: |
| Hastea: hastea.wasm | 2280 kB |
| React: react + react-dom (minified) | 139 kB |

## What the numbers say

Hastea is about twice as slow as React on most operations, and its download is 16 times larger. (Loading is measured as the median over fresh browser contexts; the other rows are from an earlier run of the same code.)

- **Building rows is within a factor of two**, and at 10,000 rows almost level, because that many rows are mostly work for the browser.
- **Selecting a row is the furthest behind**, at 11 ms against 3 ms. Only two rows change, but every render goes over all 1,000 rows to find that out: the view is computed, expanded and matched by key, and each `lazy` row is compared with the last one.
- **Loading the page takes half as long again**, which is the time to fetch and start a 2.3 MB WebAssembly module. This is with a server on the same machine; over a network the size of the download matters far more. The page fetches everything it needs at once and from its own host: the WASI shim is bundled rather than loaded from a CDN.

## How it got here

The first measurements were far worse. Timing the parts of a render with a temporary build showed where the time went, and three changes followed.

| Operation | At first | Without the quadratic check | With Text, and nodes built in one call |
| --- | ---: | ---: | ---: |
| Create 1,000 rows | 239 | 229 | 75 |
| Select a row of 1,000 | 58 | 20 | 11 |
| Swap two rows of 1,000 | 127 | 89 | 43 |
| Append 1,000 rows to 1,000 | 505 | 397 | 92 |
| Create 10,000 rows | 6,716 | 3,249 | 692 |

1. **A check for duplicate keys compared every key with every other one.** It cost 35 ms per render at 1,000 rows. It now counts keys in a map.
2. **Calls from Haskell to JavaScript were what building the page cost most.** A row took about 35 of them: one per node, per attribute and per child added. Passing a short `String` took 5.6 µs and creating an element 3.7 µs, against 0.1 µs for a call that does nothing. Now the nodes to create are described in one piece of text, and one call has JavaScript build them all; rearranging the children of a node is one call as well. That also did away with looking children up by position in a linked list, which was quadratic.
3. **Text is `Data.Text` instead of `String`.** It is stored as UTF-8, which JavaScript decodes in one go, where a `String` had to be walked character by character first.

The last two changes were made together, so the table cannot say how much each of them contributed.

Garbage collection turned out not to matter: a full collection with 1,000 rows on the page takes 2 to 7 ms.

The performance section of the [roadmap](../ROADMAP.md) lists what is left.
