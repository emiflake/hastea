// Measures the same operations on the Hastea page and on the React page, in
// headless Chromium, and prints a table in Markdown.
//
//   ./build.sh benchmark              (in the repository root)
//   cd benchmark && npm install
//   node run.cjs [runs]
//
// Needs Playwright (https://playwright.dev) to be installed where Node can
// find it.
const http = require("http");
const fs = require("fs");
const path = require("path");
const { chromium } = require("playwright");

const runs = Number(process.argv[2] ?? 10);
const here = __dirname;
const modules = path.join(here, "node_modules");

// Where the files of the two pages come from.
const sites = {
  "/hastea/": [path.join(here, "..", "dist")],
  "/react/": [path.join(here, "react"), path.join(here, "www"), path.join(modules, "react", "umd"), path.join(modules, "react-dom", "umd")],
};
const types = { ".html": "text/html", ".js": "text/javascript", ".css": "text/css", ".wasm": "application/wasm" };

const server = http.createServer((request, response) => {
  for (const [prefix, folders] of Object.entries(sites)) {
    if (!request.url.startsWith(prefix)) continue;
    const name = path.basename(request.url.slice(prefix.length) || "index.html");
    for (const folder of folders) {
      const file = path.join(folder, name);
      if (fs.existsSync(file)) {
        response.writeHead(200, { "Content-Type": types[path.extname(file)] ?? "application/octet-stream" });
        return fs.createReadStream(file).pipe(response);
      }
    }
  }
  response.writeHead(404).end();
});

// Each operation: how to get the page ready, what to click, and how many
// rows there should be afterwards.
const tenth = "#tbody tr:nth-child(10)";
const operations = [
  { name: "Create 1,000 rows", setup: ["#clear"], click: "#run", rows: 1000 },
  { name: "Replace all 1,000 rows", setup: ["#run"], click: "#run", rows: 1000 },
  { name: "Update every 10th row of 1,000", setup: ["#run"], click: "#update", rows: 1000 },
  { name: "Select a row of 1,000", setup: ["#run"], click: tenth + " a.lbl", rows: 1000 },
  { name: "Swap two rows of 1,000", setup: ["#run"], click: "#swaprows", rows: 1000 },
  { name: "Remove a row of 1,000", setup: ["#run"], click: tenth + " a.remove", rows: 999 },
  { name: "Append 1,000 rows to 1,000", setup: ["#run"], click: "#add", rows: 2000 },
  { name: "Clear 1,000 rows", setup: ["#run"], click: "#clear", rows: 0 },
  { name: "Create 10,000 rows", setup: ["#clear"], click: "#runlots", rows: 10000 },
];

// Click, and wait until the browser has painted the result.
const clickAndWait = (page, selector) =>
  page.evaluate(async (selector) => {
    const element = document.querySelector(selector);
    const start = performance.now();
    element.click();
    await new Promise((resolve) => requestAnimationFrame(() => setTimeout(resolve, 0)));
    return performance.now() - start;
  }, selector);

const median = (numbers) => {
  const sorted = [...numbers].sort((a, b) => a - b);
  const middle = Math.floor(sorted.length / 2);
  return sorted.length % 2 ? sorted[middle] : (sorted[middle - 1] + sorted[middle]) / 2;
};

async function measure(browser, url) {
  const page = await browser.newPage();
  // The Hastea page loads this from a CDN; use the local copy instead.
  await page.route("https://cdn.jsdelivr.net/npm/@bjorn3/browser_wasi_shim@*/dist/**", (route) =>
    route.fulfill({ path: path.join(modules, "@bjorn3", "browser_wasi_shim", "dist", route.request().url().split("/dist/")[1]), contentType: "text/javascript" }),
  );
  const start = Date.now();
  await page.goto(url);
  await page.waitForSelector("#run");
  const results = { "Load the page": Date.now() - start };
  for (const operation of operations) {
    const times = [];
    // The first two runs are to warm up.
    for (let run = 0; run < runs + 2; run++) {
      for (const selector of operation.setup) await clickAndWait(page, selector);
      const time = await clickAndWait(page, operation.click);
      const rows = await page.locator("#tbody tr").count();
      if (rows !== operation.rows) throw Error(`${url}: ${operation.name}: expected ${operation.rows} rows, found ${rows}`);
      if (run >= 2) times.push(time);
    }
    results[operation.name] = median(times);
  }
  await page.close();
  return results;
}

(async () => {
  await new Promise((resolve) => server.listen(0, resolve));
  const base = `http://localhost:${server.address().port}`;
  const browser = await chromium.launch();
  const hastea = await measure(browser, base + "/hastea/");
  const react = await measure(browser, base + "/react/");
  await browser.close();
  server.close();

  console.log(`Median of ${runs} runs, in milliseconds, from the click until the browser has painted.\n`);
  console.log("| Operation | Hastea | React | Hastea / React |");
  console.log("| --- | ---: | ---: | ---: |");
  for (const name of Object.keys(hastea)) {
    console.log(`| ${name} | ${hastea[name].toFixed(1)} | ${react[name].toFixed(1)} | ${(hastea[name] / react[name]).toFixed(1)}× |`);
  }

  const size = (file) => (fs.statSync(file).size / 1024).toFixed(0) + " kB";
  console.log("\n| Download | Size |");
  console.log("| --- | ---: |");
  console.log(`| Hastea: hastea.wasm | ${size(path.join(here, "..", "dist", "hastea.wasm"))} |`);
  console.log(`| React: react + react-dom (minified) | ${(["react/umd/react.production.min.js", "react-dom/umd/react-dom.production.min.js"].reduce((sum, file) => sum + fs.statSync(path.join(modules, file)).size, 0) / 1024).toFixed(0)} kB |`);
})();
