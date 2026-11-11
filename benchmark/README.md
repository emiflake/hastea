# Benchmark

The table of rows that web frameworks are commonly measured with, written with Hastea ([`Main.hs`](Main.hs)) and with React 18 ([`react/app.js`](react/app.js)). Both versions give each row a key and skip rows that did not change (`lazy` and `memo`).

[`run.cjs`](run.cjs) opens both pages in headless Chromium, performs each operation several times, and reports the median time from the click until the browser has painted the result.

## Running it

```bash
./build.sh benchmark               # in the repository root
cd benchmark
npm install                        # React, and a local copy of the WASI shim
node run.cjs                       # or: node run.cjs 20, for 20 runs of each operation
```

It needs [Playwright](https://playwright.dev) installed where Node can find it.

## Results

Measured on a 4-core Intel Xeon virtual machine, with GHC 9.14.1 at `-O1` and React 18.3.1 (production build). The numbers will differ on other machines; the ratios are what to look at.

Median of 10 runs, in milliseconds, from the click until the browser has painted.

| Operation | Hastea | React | Hastea / React |
| --- | ---: | ---: | ---: |
| Load the page | 96.0 | 58.0 | 1.7× |
| Create 1,000 rows | 239.2 | 36.9 | 6.5× |
| Replace all 1,000 rows | 240.2 | 36.1 | 6.7× |
| Update every 10th row of 1,000 | 75.2 | 20.6 | 3.7× |
| Select a row of 1,000 | 58.3 | 6.8 | 8.6× |
| Swap two rows of 1,000 | 126.6 | 33.9 | 3.7× |
| Remove a row of 1,000 | 92.5 | 11.8 | 7.8× |
| Append 1,000 rows to 1,000 | 504.7 | 40.6 | 12.4× |
| Clear 1,000 rows | 7.9 | 6.6 | 1.2× |
| Create 10,000 rows | 6715.9 | 559.3 | 12.0× |

| Download | Size |
| --- | ---: |
| Hastea: hastea.wasm | 2113 kB |
| React: react + react-dom (minified) | 139 kB |

## What the numbers say

Hastea is between 4 and 12 times slower than React on everything except clearing the table, and its download is 15 times larger.

Two things stand out, and both point at work done outside the DOM:

- **Selecting a row takes 58 ms although only two rows change.** Every render goes over all 1,000 rows again to find that out: the tree is expanded, the children are matched by key, and each `lazy` row is compared with the last one.
- **The cost grows faster than the number of rows.** Creating 10,000 rows takes 28 times as long as creating 1,000, and appending 1,000 rows to 1,000 takes twice as long as creating 1,000. When children are added, removed or moved, the code that carries out the patch looks rows up by position in a linked list, which is quadratic.

These are readings of the code, not profiles. The performance section of the [roadmap](../ROADMAP.org) lists the suspects.
