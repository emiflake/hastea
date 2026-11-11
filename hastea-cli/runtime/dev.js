// Loads a Hastea application in `hastea dev`, in place of index.js.
//
// The hastea command hands out a GHCi session in browser mode: an address
// from which GHCi's interpreter boots in this page. GHCi then loads the
// modules here, and the hastea command runs the application, and runs it
// again after every reload. Events from the hastea command say when to show
// a compilation error, replace the stylesheets, or load the page again.

const overlay = document.createElement("div");
overlay.style.cssText =
  "position:fixed;inset:0;z-index:2147483647;overflow:auto;padding:2em;background:rgba(20,20,20,.94);" +
  "color:#eee;font:13px/1.5 ui-monospace,monospace;white-space:pre-wrap;display:none";

function show(summary, details) {
  overlay.textContent = "";
  const title = document.createElement("div");
  title.style.cssText = "color:#ff6b6b;font-weight:bold;margin-bottom:1em";
  title.textContent = `hastea dev: ${summary}`;
  overlay.append(title, details ?? "");
  overlay.style.display = "block";
  if (!overlay.isConnected) document.body.append(overlay);
}

function hide() {
  overlay.style.display = "none";
}

const events = new EventSource("/__hastea/events");
let lost = false;
// The connection drops when the hastea command stops; once it is back, the
// page has to start over.
events.addEventListener("error", () => { lost = true; });
events.addEventListener("open", () => { if (lost) location.reload(); });
events.addEventListener("reload", () => location.reload());
events.addEventListener("ok", hide);
events.addEventListener("failed", (event) => {
  const { summary, details } = JSON.parse(event.data);
  show(summary, details);
});
events.addEventListener("css", () => {
  for (const link of document.querySelectorAll('link[rel="stylesheet"]')) {
    const url = new URL(link.href);
    url.searchParams.set("hastea", Date.now());
    link.href = url.href;
  }
});

const response = await fetch("/__hastea/session", { method: "POST" });
if (response.ok) {
  const { origin } = await response.json();
  await import(`${origin}/main.js`);
} else {
  show("no GHCi session", await response.text());
}
