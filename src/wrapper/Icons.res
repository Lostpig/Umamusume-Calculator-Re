module Menu = {
  @react.component @module("@material-ui/icons/Menu")
  external make: (
    ~color: string=?,
    ~fontSize: string=?,
    ()
  ) => React.element = "default"
}

module Translate = {
  @react.component @module("@material-ui/icons/Translate")
  external make: (
    ~color: string=?,
    ~fontSize: string=?,
    ()
  ) => React.element = "default"
}