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
| Load the page | 94.0 | 49.0 | 1.9× |
| Create 1,000 rows | 228.9 | 38.7 | 5.9× |
| Replace all 1,000 rows | 214.6 | 41.3 | 5.2× |
| Update every 10th row of 1,000 | 37.4 | 14.3 | 2.6× |
| Select a row of 1,000 | 20.2 | 1.6 | 12.2× |
| Swap two rows of 1,000 | 89.4 | 32.2 | 2.8× |
| Remove a row of 1,000 | 57.5 | 7.8 | 7.3× |
| Append 1,000 rows to 1,000 | 397.3 | 40.3 | 9.9× |
| Clear 1,000 rows | 7.1 | 6.6 | 1.1× |
| Create 10,000 rows | 3249.3 | 532.2 | 6.1× |

| Download | Size |
| --- | ---: |
| Hastea: hastea.wasm | 2121 kB |
| React: react + react-dom (minified) | 139 kB |

## What the numbers say

Hastea is between 3 and 12 times slower than React on everything except clearing the table, and its download is 15 times larger.

## Where the time goes

Measured once with a build that reported the time spent in each part of a render (not kept in the repository):

| Creating 1,000 rows | Time | |
| --- | ---: | --- |
| Computing the view, expanding and diffing it | 20–30 ms | The pure part |
| Building the rows in the DOM | 150–180 ms | About 150 µs per row |
| Layout and paint by the browser, and the rest | 20–40 ms | |

Building a row means creating 9 nodes and handing 17 strings to JavaScript: the tag and attribute names as well as the text. On the same build:

| One call from Haskell to JavaScript | Time |
| --- | ---: |
| That does nothing | 0.1 µs |
| That creates an element | 3.7 µs |
| That passes a `String` of 20 characters | 5.6 µs |

So most of the time to create rows goes into passing strings, most of which are the same few names over and over, and into holding on to each new node from Haskell.

Other things the measurements show:

- **Building 10,000 rows takes almost twice as long per row as building 1,000.** When children are added, removed or moved, the code that carries out the patch looks them up by position in a linked list, which is quadratic.
- **Selecting a row takes 20 ms although only two rows change**, about half of it in the pure part: every render goes over all 1,000 rows again to find that out.
- **Garbage collection is not the problem.** A full collection with 1,000 rows on the page takes 2 to 7 ms.

An earlier version of this table had selecting a row at 58 ms and 10,000 rows at 6.7 s. Most of the difference was a check for duplicate keys that compared every key with every other one; it now uses a map.

The performance section of the [roadmap](../ROADMAP.md) lists what follows from this.
