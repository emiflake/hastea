// The page of ../Main.hs, written with React. No JSX, so that it needs no
// build step: h is React.createElement.
const { createElement: h, memo, useReducer } = React;

const adjectives = "pretty large big small tall short long handsome plain quaint clean elegant easy angry crazy helpful mushy odd unsightly adorable important inexpensive cheap expensive fancy".split(" ");
const colours = "red yellow blue green pink brown purple brown white black orange".split(" ");
const nouns = "table chair house bbq desk car pony cookie sandwich burger pizza mouse keyboard".split(" ");

let nextId = 1;
let seed = 1;

function next() {
  seed = (seed * 75 + 74) % 65537;
  return seed;
}

function makeRows(count) {
  const rows = new Array(count);
  for (let i = 0; i < count; i++) {
    const label = adjectives[next() % adjectives.length] + " " + colours[next() % colours.length] + " " + nouns[next() % nouns.length];
    rows[i] = { id: nextId++, label };
  }
  return rows;
}

function reducer(state, action) {
  switch (action.type) {
    case "created":
      return { rows: makeRows(action.count), selected: 0 };
    case "appended":
      return { ...state, rows: state.rows.concat(makeRows(action.count)) };
    case "updatedEveryTenth":
      return { ...state, rows: state.rows.map((row, index) => (index % 10 === 0 ? { ...row, label: row.label + " !!!" } : row)) };
    case "cleared":
      return { rows: [], selected: 0 };
    case "swapped": {
      if (state.rows.length <= 998) return state;
      const rows = state.rows.slice();
      [rows[1], rows[998]] = [rows[998], rows[1]];
      return { ...state, rows };
    }
    case "selected":
      return { ...state, selected: action.id };
    case "removed":
      return { ...state, rows: state.rows.filter((row) => row.id !== action.id) };
    default:
      throw Error("Unknown action: " + action.type);
  }
}

// A row that did not change, and whose selection did not, is skipped.
const Row = memo(function Row({ row, isSelected, dispatch }) {
  return h(
    "tr",
    { className: isSelected ? "danger" : "" },
    h("td", { className: "id" }, row.id),
    h("td", null, h("a", { className: "lbl", onClick: () => dispatch({ type: "selected", id: row.id }) }, row.label)),
    h("td", null, h("a", { className: "remove", onClick: () => dispatch({ type: "removed", id: row.id }) }, "x")),
  );
});

function Action({ id, caption, onClick }) {
  return h("button", { type: "button", id, onClick }, caption);
}

function App() {
  const [state, dispatch] = useReducer(reducer, { rows: [], selected: 0 });
  return h(
    "div",
    { className: "container" },
    h(
      "div",
      { className: "jumbotron" },
      h("h1", null, "React"),
      h(
        "div",
        null,
        h(Action, { id: "run", caption: "Create 1,000 rows", onClick: () => dispatch({ type: "created", count: 1000 }) }),
        h(Action, { id: "runlots", caption: "Create 10,000 rows", onClick: () => dispatch({ type: "created", count: 10000 }) }),
        h(Action, { id: "add", caption: "Append 1,000 rows", onClick: () => dispatch({ type: "appended", count: 1000 }) }),
        h(Action, { id: "update", caption: "Update every 10th row", onClick: () => dispatch({ type: "updatedEveryTenth" }) }),
        h(Action, { id: "clear", caption: "Clear", onClick: () => dispatch({ type: "cleared" }) }),
        h(Action, { id: "swaprows", caption: "Swap Rows", onClick: () => dispatch({ type: "swapped" }) }),
      ),
    ),
    h(
      "table",
      { className: "table" },
      h(
        "tbody",
        { id: "tbody" },
        state.rows.map((row) => h(Row, { key: row.id, row, isSelected: row.id === state.selected, dispatch })),
      ),
    ),
  );
}

ReactDOM.createRoot(document.getElementById("main-node")).render(h(App));
