module Translation = {
  type t

  @send 
  external translate: (t, I18next.Tkey.t, ~options: WrapTypes.any=?, unit) => string = "t"
  @send
  external translateSimple: (t, string) => string = "t"
  @get
  external i18n: t => I18next.t = "i18n"
  @get
  external ready: t => bool = "ready"
}

module UseTranslationOptions = {
  type t = {"i18n": option<I18next.t>, "useSuspense": option<bool>}
  @obj
  external make: (~i18n: I18next.t=?, ~useSuspense: bool=?, unit) => t = ""
}

@module("react-i18next")
external useTranslation: (
  ~ns: I18next.NameSpace.t=?,
  ~options: UseTranslationOptions.t=?,
  unit
) => Translation.t = "useTranslation"

module Provider = {
  @react.component @module("react-i18next")
  external make: (
    ~i18n: I18next.t,
    ~defaultNS: bool=?,

    ~children: 'children
  ) => React.element = "I18nextProvider"
}

module Trans = {
  @react.component @module("react-i18next")
  external make: (
    ~count: int=?,
    ~defaults: string=?,
    ~i18n: I18next.t,
    ~i18nKey: string=?,
    ~ns: string=?,
    ~parent: string=?,
    ~tOptions: WrapTypes.any,
    ~values: WrapTypes.any,
    ~t: I18next.tFunction,

    ~children: 'children
  ) => React.element = "Trans"
}

@module("react-i18next")
external initReactI18next: I18next.I18nextModule.t = "initReactI18next"