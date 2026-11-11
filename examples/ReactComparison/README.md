# React examples in Hastea

Ten examples from the [React documentation](https://react.dev), each showing a different React feature, rewritten with Hastea. Each one is a component with its own state, and all of them run on one page.

```bash
./build.sh react-comparison          # from the repository root
python3 -m http.server -d dist 8080  # then open http://localhost:8080
```

## The examples

| React feature | React original | Hastea version |
| --- | --- | --- |
| `useState` | [Counter](https://react.dev/reference/react/useState#counter-number) | [`Counter.hs`](ReactComparison/Counter.hs) |
| Lifting state up | [Accordion](https://react.dev/learn/sharing-state-between-components) | [`Accordion.hs`](ReactComparison/Accordion.hs) |
| Props, lists and keys, conditional rendering, controlled inputs | [Filterable product table](https://react.dev/learn/thinking-in-react) | [`ProductTable.hs`](ReactComparison/ProductTable.hs) |
| `useReducer` | [Task list](https://react.dev/learn/extracting-state-logic-into-a-reducer) | [`Tasks.hs`](ReactComparison/Tasks.hs) |
| `memo` | [Greeting](https://react.dev/reference/react/memo) | [`Greeting.hs`](ReactComparison/Greeting.hs) |
| Context | [Section headings](https://react.dev/learn/passing-data-deeply-with-context) | [`Headings.hs`](ReactComparison/Headings.hs) |
| Effects and refs (a timer) | [Stopwatch](https://react.dev/learn/referencing-values-with-refs) | [`Stopwatch.hs`](ReactComparison/Stopwatch.hs), and in React's own style in [`StopwatchRoot.hs`](ReactComparison/StopwatchRoot.hs) |
| Refs to DOM nodes | [Focusing a text input](https://react.dev/learn/manipulating-the-dom-with-refs) | [`Focus.hs`](ReactComparison/Focus.hs) |
| Asynchronous event handlers | [City quiz](https://react.dev/learn/reacting-to-input-with-state) | [`Quiz.hs`](ReactComparison/Quiz.hs) |
| `useEffect` to fetch data | [Fetching data with Effects](https://react.dev/reference/react/useEffect#fetching-data-with-effects) | [`Bio.hs`](ReactComparison/Bio.hs) |
| Using components | (the page itself) | [`Main.hs`](Main.hs) |

## Key differences in how the code is written

| | React | Hastea | The difference |
| --- | --- | --- | --- |
| **A component** | A function that returns JSX and calls hooks | `component initial update view` | A React component is code that runs on every render and asks for its state as it goes. A Hastea component is made of three pure functions, each with one job. |
| **Using a component** | `<Counter />` | `use counter ()` | The same idea: one line, and whatever shows the view keeps the state. Using it twice gives two states. |
| **Markup** | `<button onClick={f}>Hi</button>` | `H.button [onClick Pressed] [H.text "Hi"]` | No JSX: elements are ordinary functions taking a list of attributes and a list of children, imported qualified as `H`. Text is explicit. |
| **State** | `const [count, setCount] = useState(0)` | `initial () = 0` | One state value per component instead of one per `useState` call; several pieces of state go in a record. |
| **Changing state** | `setCount(count + 1)` inside an event handler | `update Pressed count = (count + 1, [])` | A handler does not change anything. It names what happened (`Pressed`), and `update` decides what that means. |
| **Event handlers** | A function: `onClick={handleClick}` | A value: `onClick Pressed` | Handlers are data, so they can be compared, logged and tested without a browser. |
| **Reading an input** | `onChange={e => setText(e.target.value)}` | `onInput Typed` | The event is decoded for you into a typed value; the message constructor is the callback. |
| **Props** | `function ProductTable({ products })` | `Component [Product] ...`, used as `use filterableProductTable products` | Props are the component's first type parameter, checked by the compiler. A part without state is just a function: `panel :: String -> Bool -> ...`. |
| **Callback props** | `<AddTask onAddTask={handleAddTask} />`, and the child calls `onAddTask(text)` | `Added <$> use addTask ()`, and the child returns `[Emit draft]` | The child does not receive a function. It emits a value, and the parent says with `<$>` which of its messages that becomes. |
| **Fragments** | `<>...</>` | `H.fragment [...]` | The same: several nodes without an element around them. |
| **Conditional rendering** | `{isActive ? <p>...</p> : <button>...</button>}` | `if isActive then H.p [] ... else H.button ...` | The same, in the host language without braces. |
| **Lists** | `tasks.map(t => <Task key={t.id} task={t} />)` | `[H.keyed (show t.id) (H.li [] [use taskRow t]) \| t <- tasks]` | `key` is a wrapper function rather than a special prop. As in React, it also decides which item a component's state belongs to. |
| **Lifting state up** | Move `useState` to the common parent, pass values and setters down | Move the state into the parent component, pass values and messages down | The same refactoring (`Accordion.hs`). |
| **`useReducer`** | Opt-in for components with complex state: `dispatch({ type: 'added', ... })` | The only way: `update` is the reducer | Actions are a data type (`Added String`, `Changed Task`, `Deleted Int`), so a misspelt or unhandled action is a compile error rather than a `throw` in `default:`. |
| **`memo`** | `memo(function Greeting({ name }) ...)` | `H.lazy greeting fields.name` | React assumes the component is pure. Here the type `String -> Html msg` guarantees it, and a skipped view is not even computed. `lazy` compares by identity, like React compares props with `Object.is`. |
| **Context** | `createContext`, `useContext`, a provider | Functions of the context: `heading :: String -> Leveled msg`, and `section [heading "Title", section [...]]` | No mechanism. Parts that need the level are functions of it, and `section` calls its children with a level one deeper. The page reads like the React one, and the type says which parts depend on the level. |
| **Effects in handlers** | The handler does it: `await submitForm(answer)` | `update` asks for it: `(state{status = Submitting}, [Run (submitForm state.answer)])` | `update` stays pure. It describes the effect, and the runtime carries it out and sends back the result as a message. |
| **Asynchronous work** | One `async` handler: set state, `await`, set state again | Two messages: `Submitted` starts it, `Answered result` finishes it | The steps of an async function become cases of `update`. The state in between is explicit, not held in a suspended function. |
| **`useEffect` on mount** | `useEffect(() => { fetchBio(person).then(setBio) }, [person])` | `onMount () = [Run (fetchBio "Alice")]`, and the same command from `update` when the person changes | React runs the effect again when a dependency changes. Here there is no dependency list: the request is started explicitly in the two places where it is needed. |
| **Ignoring a stale response** | A cleanup function sets `ignore = true` | `BioFetched person result` is dropped unless `person == page.person` | The response carries what it is about, so no cancellation flag is needed. |
| **Timers** | `setInterval` in a handler, the id kept in a `useRef`, `clearInterval` to stop | `subscriptions watch = if watch.running then Time.every 10 Ticked else Sub.none` | Declarative: say when ticks are wanted, as a function of the state. The runtime starts and stops the timer, so it cannot be leaked or started twice. |
| **Refs to DOM nodes** | `const inputRef = useRef(null)`, `<input ref={inputRef} />`, `inputRef.current.focus()` | `H.input [A.id_ inputId]` and `[Run (Dom.focus inputId)]` | Views never see DOM nodes. The node is named by its id and acted on by a command. |
| **Rules of hooks** | Hooks must be called in the same order on every render | None | There are no hooks, so views can use any control flow. |
| **One model for everything** | A state library such as Redux | `Hastea.App.runApp update initial view` | Built in, and it mixes with components. The [TodoMVC example](../todomvc) is written this way: one model, one `update`, no components. |
| **Showing the page** | `createRoot(element).render(<App />)` | `runView page` | The same. |
| **Escaping the architecture** | React is the lowest layer | `Hastea.Root`: `mount` and `render`, with any message type | Components and `Hastea.App` are both built on `Hastea.Root`. `StopwatchRoot.hs` uses the root directly with messages of type `IO ()`, which gives React's style of effectful handlers and `setState`. |

## What does not carry over yet

- **Subscriptions other than time**, such as key presses or window resizing. `Hastea.Sub.custom` is the way to add them.
- **Reading from the DOM**, such as measuring an element or scrolling to it. Only `focus` and `blur` exist.
- **Reading a component's state from outside.** As in React, a parent only learns what a component emits. State that two parts need has to live in their common parent.
- **Terseness of markup.** Elements and attributes are written `H.div` and `A.className`, which is noisier than JSX. The qualifiers are what keeps the element names (`a`, `i`, `title`, `time`, ...) from clashing with your own.
