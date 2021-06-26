// init i18n
I18n.init()->ignore

// render react
switch(ReactDOM.querySelector("#root")){
  | Some(root) => ReactDOM.render(
      <React.StrictMode>
        <React.Suspense fallback={<div>{ "Loading" -> React.string }</div>}>
          <App />
        </React.Suspense>
      </React.StrictMode>, 
    root)
  | None => () // do nothing
}