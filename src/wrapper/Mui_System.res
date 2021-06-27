module PaletteProps = {
  type t = {
    "theme": MaterialUi.Theme.t,
    "color": option<string>,
    "bgColor": option<string>,
  }
  @obj
  external make: (
    ~theme: MaterialUi.Theme.t,
    ~color: string=?,
    ~bgColor: string=?,
    unit
  ) => t = ""
}
type paletteResult = {
  color: option<string>,
  bgColor: option<string>,
}

@module("@material-ui/system")
external palette: PaletteProps.t => paletteResult = "palette"


let theme = MaterialUi.Theme.create(MaterialUi.ThemeOptions.make())
let getColor = (color: string) => {
  let props = PaletteProps.make(~theme=theme, ~color=color, ())
  let res = palette(props)

  switch res.color {
  | Some(v) => v
  | None => ""
  }
}

module Colors = {
  let black = getColor("common.black")
  let white = getColor("common.white")

  let primary = getColor("primary.main")
  let secondary = getColor("secondary.main")
  let error = getColor("error.main")
  let warning = getColor("warning.main")
  let info = getColor("info.main")
  let success = getColor("success.main")

  let divider = getColor("divider")
  let t_primary = getColor("text.primary")
  let t_secondary = getColor("text.secondary")
  let t_disabled = getColor("text.disabled")
}
