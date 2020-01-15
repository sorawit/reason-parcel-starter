type state_t = {
  counter: int,
  todos: list((int, string)),
};

type action_t =
  | Add(string)
  | Remove(int)
  | Clear;

let initialState = {counter: 1, todos: []};

let rec map = (ls, f) =>
  switch (ls) {
  | [] => []
  | [x, ...xs] => [f(x), ...map(xs, f)]
  };

let rec filter = (ls, f) =>
  switch (ls) {
  | [] => []
  | [x, ...xs] =>
    if (f(x)) {
      [x, ...filter(xs, f)];
    } else {
      filter(xs, f);
    }
  };

let reduce = state =>
  fun
  | Add(desc) => {counter: state.counter + 1, todos: [(state.counter, desc), ...state.todos]}
  | Remove(id) => {counter: state.counter, todos: filter(state.todos, ((idx, _)) => idx != id)}
  | Clear => initialState;

[@react.component]
let make = () => {
  let (state, dispatch) = React.useReducer(reduce, initialState);
  let (currentInput, setCurrentInput) = React.useState(_ => "");

  let todos = state.todos;

  <div>
    <h2> {"ReasonML TODO List Application" |> React.string} </h2>
    <input
      value=currentInput
      onChange={e => {
        let input = ReactEvent.Form.target(e)##value;
        setCurrentInput(_ => input);
      }}
    />
    <button
      onClick={_ => {
        dispatch(Add(currentInput));
        setCurrentInput(_ => "");
      }}>
      {"Add" |> React.string}
    </button>
    <button onClick={_ => dispatch(Clear)}> {"Clear" |> React.string} </button>
    <ul>
      {map(todos, ((idx, desc)) =>
         <li>
           {"#" ++ string_of_int(idx) ++ ": " ++ desc |> React.string}
           <button onClick={_ => dispatch(Remove(idx))}> {"Remove" |> React.string} </button>
         </li>
       )
       |> Belt.List.toArray
       |> React.array}
    </ul>
  </div>;
};
