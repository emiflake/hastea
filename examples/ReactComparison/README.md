# React examples in Hastea

Nine examples from the [React documentation](https://react.dev), each showing a different React feature, rewritten with Hastea. All of them run on one page.

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
| Composing components | (the page itself) | [`Main.hs`](Main.hs) |

## Key differences in how the code is written

| | React | Hastea | The difference |
| --- | --- | --- | --- |
| **A component** | A function that returns JSX and may call hooks | A `Model` type, a `Msg` type, `update :: Msg -> Model -> Model` and `view :: Model -> Html Msg` | A React component is one function that owns its state. A Hastea component is a few plain values and functions, and owns nothing. |
| **Markup** | `<button onClick={f}>Hi</button>` | `button [onClick Pressed] [text "Hi"]` | No JSX: elements are ordinary functions taking a list of attributes and a list of children. Text is explicit. |
| **State** | `const [count, setCount] = useState(0)` | `initial = 0`, stored by whoever uses the component | State is not attached to a place in the tree. Showing a counter twice means keeping two models (see `Main.hs`). |
| **Changing state** | `setCount(count + 1)` inside an event handler | `update Pressed count = count + 1` | A handler does not change anything. It names what happened (`Pressed`), and `update` decides what that means. |
| **Event handlers** | A function: `onClick={handleClick}` | A value: `onClick Pressed` | Handlers are data, so they can be compared, logged and tested without a browser. |
| **Reading an input** | `onChange={e => setText(e.target.value)}` | `onInput TextChanged` | The event is decoded for you into a typed value; the message constructor is the callback. |
| **Props** | `function Panel({ title, isActive })` | `panel :: String -> Bool -> ...` | Props are function arguments, checked by the compiler. |
| **Callback props** | `onShow={() => setActiveIndex(1)}` | `panel "Etymology" isActive (Shown 1)` | The parent passes the message to send instead of a function to call. |
| **Conditional rendering** | `{isActive ? <p>...</p> : <button>...</button>}` | `if isActive then p [] ... else button ...` | The same, in the host language without braces. |
| **Lists** | `items.map(x => <Row key={x.id} ... />)` | `[keyed (show (taskId t)) (viewTask t) \| t <- tasks]` | `key` is a wrapper function rather than a special prop. Leaving it out is allowed and means matching by position. |
| **Lifting state up** | Move `useState` to the common parent, pass values and setters down | Nothing to do | State already lives above the views, so there is no refactoring step. |
| **`useReducer`** | Opt-in for components with complex state: `dispatch({ type: 'added', ... })` | The only way: `update` is the reducer | Actions are a data type (`Added`, `Changed Task`, `Deleted Int`), so a misspelt or unhandled action is a compile error rather than a `throw` in `default:`. |
| **State local to a child** | `AddTask` keeps its draft text; each `Task` keeps `isEditing` | `draft` and `editing` are fields of the parent's model | The price of having no local state: even transient UI state has to be added to the model and given messages. |
| **Composition** | `<Counter />` | `FirstCounterMsg <$> Counter.view (firstCounter model)` | The parent stores the child's model, wraps its messages with `fmap`, and forwards them in `update`. More wiring than in React, but every state change in the app passes through one function. |
| **`memo`** | `memo(function Greeting({ name }) ...)` | `lazy greeting (name model)` | React assumes the component is pure. Here the type `String -> Html msg` guarantees it, and a skipped view is not even computed. `lazy` compares by identity, like React compares props with `Object.is`. |
| **Context** | `createContext`, `useContext`, a provider | A function argument: `section level $ \level2 -> ...` | No mechanism. The level is passed explicitly, which is more typing through many layers, but there is no hidden input. |
| **Effects in handlers** | The handler does it: `setInterval(...)`, `await submitForm(answer)` | `update` returns a command next to the new model: `(model{status = Submitting}, submitForm (answer model))` | `update` stays pure. It describes the effect, and the runtime carries it out and sends back the result as a message. |
| **Asynchronous work** | One `async` handler: set state, `await`, set state again | Two messages: `Submitted` starts it, `Answered result` finishes it | The steps of an async function become cases of `update`. The state in between is explicit in the model, not held in a suspended function. |
| **Timers** | `setInterval` in a handler, the id kept in a `useRef`, `clearInterval` to stop | `subscriptions model = if running model then Time.every 10 Ticked else Sub.none` | Declarative: say when ticks are wanted, as a function of the model. The runtime starts and stops the timer, so it cannot be leaked or started twice. |
| **Refs to DOM nodes** | `const inputRef = useRef(null)`, `<input ref={inputRef} />`, `inputRef.current.focus()` | `input [id_ inputId]` and the command `Dom.focus inputId` | Views never see DOM nodes. The node is named by its id and acted on by a command. |
| **Escaping the architecture** | React is the lowest layer | `Hastea.Root`: `mount` and `render`, with any message type | `Hastea.App` is a library on top of `Hastea.Root`. `StopwatchRoot.hs` uses the root directly with messages of type `IO ()`, which gives React's style of effectful handlers and `setState`. |
| **Rules of hooks** | Hooks must be called in the same order on every render | None | There are no hooks, so views can use any control flow. |

## What does not carry over yet

- **HTTP requests.** Commands can wait and run arbitrary `IO`, but there is no binding for `fetch` yet; the quiz only pretends to use the network, like the React original.
- **Subscriptions other than time**, such as key presses or window resizing. `Hastea.Sub.custom` is the way to add them.
- **Reading from the DOM**, such as measuring an element or scrolling to it. Only `focus` and `blur` exist.
- **Name clashes.** `Hastea.Html` exports a function for every tag, including `i`, `s`, `title`, `time` and `address`, so a module that imports it unqualified cannot use those names for its own variables without a warning.
