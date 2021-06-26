

module Tkey = {
  type t

  external string: string => t = "%identity"
  external arrayOf: array<string> => t = "%identity"
}
module FallBackLng = {
  type t

  external single: string => t = "%identity"
  external multi: array<string> => t = "%identity"
  external object: {..} => t = "%identity"
  external callback: ((string) => t) => t = "%identity"
  external asFalse: (@as(json`false`) _) => t = "%identity"
}
module WhileList = {
  type t

  external arrayOf: array<string> => t = "%identity"
  external asFalse: (@as(json`false`) _) => t = "%identity"
}
module SupportedLngs = WhileList
module Preload = WhileList
module NameSpace = Tkey
module Language = Tkey
module FallbackNS = {
  type t

  external string: string => t = "%identity"
  external arrayOf: array<string> => t = "%identity"
  external asFalse: (@as(json`false`) _) => t = "%identity"
}
module PostProcess = FallbackNS
module JoinArrays = {
  type t

  external string: string => t = "%identity"
  external asFalse: (@as(json`false`) _) => t = "%identity"
}
module KeySeparator = JoinArrays
module NsSeparator = JoinArrays
module BindI18n = JoinArrays
module BindI18nStore = JoinArrays

type load = [#all | #currentOnly | #languageOnly]
type saveMissingTo = [#current | #all | #fallback]
type compatibilityJSON = [#v1 | #v2 | #v3]

type moduleType = [#backend | #logger | #languageDetector | #postProcessor | #i18nFormat | #"3rdParty"]
module I18nextModule = {
  type t = {
    @as("type") type_: moduleType
  }
  @obj
  external make: (
    ~type_: moduleType,
    unit
  ) => t = ""
}

module ReactOptions = {
  type t = {
    "wait": option<bool>,
    "nsMode": option<[#default | #fallback]>,
    "defaultTransParent": option<bool>,
    "bindI18n": option<BindI18n.t>,
    "bindI18nStore": option<BindI18nStore.t>,
    "transEmptyNodeValue": option<string>,
    "useSuspense": option<bool>,
    "transSupportBasicHtmlNodes": option<bool>,
    "transKeepBasicHtmlNodesFor": option<array<string>>,
    "hashTransKey": option<(WrapTypes.any) => WrapTypes.any>
  }
}
module InitOptions = {
  type rec t = {
    "debug": option<bool>,
    "resources": option<WrapTypes.any>,
    "partialBundledLanguages": option<bool>,
    "lng": option<string>,
    "fallbackLng": option<FallBackLng.t>,
    "whitelist": option<WhileList.t>,
    "nonExplicitWhitelist": option<bool>,
    "supportedLngs": option<SupportedLngs.t>,
    "nonExplicitSupportedLngs": option<bool>,
    "load": option<load>,
    "preload": option<Preload.t>,
    "lowerCaseLng": option<bool>,
    "cleanCode": option<bool>,
    "ns": option<NameSpace.t>,
    "defaultNS": option<string>,
    "fallbackNS": option<FallbackNS.t>,
    "saveMissing": option<bool>,
    "updateMissing": option<bool>,
    "missingKeyHandler": option<(array<string>, string, string, string) => unit>,
    "parseMissingKeyHandler": option<(string) => WrapTypes.any>,
    "appendNamespaceToMissingKey": option<bool>,
    "missingInterpolationHandler": option<(string, WrapTypes.any, t) => WrapTypes.any>,
    "simplifyPluralSuffix": option<bool>,
    "postProcess": option<PostProcess.t>,
    "postProcessPassResolved": option<bool>,
    "returnNull": option<bool>,
    "returnEmptyString": option<bool>,
    "returnObjects": option<bool>,
    "returnedObjectHandler": option<(string, string, WrapTypes.any) => unit>,
    "joinArrays": option<JoinArrays.t>,
    "overloadTranslationOptionHandler": option<(array<string>) => WrapTypes.any>,
    // TODO "interpolation"
    "react": option<ReactOptions.t>,
    "initImmediate": option<bool>,
    "keySeparator": option<KeySeparator.t>,
    "nsSeparator": option<NsSeparator.t>,
    "pluralSeparator": option<string>,
    "contextSeparator": option<string>,
    "appendNamespaceToCIMode": option<bool>,
    "compatibilityJSON": option<compatibilityJSON>,
    // TODO "editor"
    // TODO "locizeLastUsed"
    "ignoreJSONStructure": option<bool>,
  }
  @obj
  external make: (
    ~debug: bool=?,
    ~resources: WrapTypes.any=?,
    ~partialBundledLanguages: bool=?,
    ~lng: string=?,
    ~fallbackLng: FallBackLng.t=?,
    ~whitelist: WhileList.t=?,
    ~nonExplicitWhitelist: bool=?,
    ~supportedLngs: SupportedLngs.t=?,
    ~nonExplicitSupportedLngs: bool=?,
    ~load: load=?,
    ~preload: Preload.t=?,
    ~lowerCaseLng: bool=?,
    ~cleanCode: bool=?,
    ~ns: NameSpace.t=?,
    ~defaultNS: string=?,
    ~fallbackNS: FallbackNS.t=?,
    ~saveMissing: bool=?,
    ~updateMissing: bool=?,
    ~missingKeyHandler: (array<string>, string, string, string) => unit=?,
    ~parseMissingKeyHandler: (string) => WrapTypes.any=?,
    ~appendNamespaceToMissingKey: bool=?,
    ~missingInterpolationHandler: (string, WrapTypes.any, t) => WrapTypes.any=?,
    ~simplifyPluralSuffix: bool=?,
    ~postProcess: PostProcess.t=?,
    ~postProcessPassResolved: bool=?,
    ~returnNull: bool=?,
    ~returnEmptyString: bool=?,
    ~returnObjects: bool=?,
    ~returnedObjectHandler: (string, string, WrapTypes.any) => unit=?,
    ~joinArrays: JoinArrays.t=?,
    ~overloadTranslationOptionHandler: (array<string>) => WrapTypes.any=?,
    // TODO "interpolation"
    // TODO "react"
    ~initImmediate: bool=?,
    ~keySeparator: KeySeparator.t=?,
    ~nsSeparator: NsSeparator.t=?,
    ~pluralSeparator: string=?,
    ~contextSeparator: string=?,
    ~appendNamespaceToCIMode: bool=?,
    ~compatibilityJSON: compatibilityJSON=?,
    // TODO "editor"
    // TODO "locizeLastUsed"
    ~ignoreJSONStructure: bool=?,
    unit,
  ) => t = ""
}

type tFunction = (. ~key: Tkey.t, ~options: WrapTypes.any=?, unit) => string

type t
@module("i18next")
external instance: t = "default"

@send
external translate: (t, Tkey.t, ~options: {..}=?, ()) => string = "t"
@send
external trans: (t, string) => string = "t"
@send
external exists: (t, Tkey.t, ~options: {..}=?, ()) => bool = "exists"
@send
external getFixedT: (t, Tkey.t, ~ns: NameSpace.t=?, ()) => tFunction = "getFixedT"
@send
external getFixedTNs: (t, Tkey.t, @as(json`null`) _, NameSpace.t) => tFunction = "getFixedT"

@send
external use: (t) => t = "use"
@send
external useModule: (t, I18nextModule.t) => t = "use"

@send
external init: (t) => Js.Promise.t<tFunction> = "init"
@send
external initCallBack: (t, (WrapTypes.any, tFunction) => unit) => Js.Promise.t<tFunction> = "init"
@send
external initOptions: (t, InitOptions.t, ~callback: (WrapTypes.any, tFunction) => unit=?, ()) => Js.Promise.t<tFunction> = "init"

@send
external getDataByLanguage: (t, string) => option<WrapTypes.any> = "getDataByLanguage"
@send
external changeLanguage: (t, ~lng: string=?, ~callback: (WrapTypes.any, tFunction) => unit=?, unit) => Js.Promise.t<tFunction> = "changeLanguage"
@send 
external loadNamespaces: (t, NameSpace.t, ~callback: (WrapTypes.any, tFunction) => unit=?, ()) => Js.Promise.t<unit> = "loadNamespaces"
@send 
external loadLanguages: (t, Language.t, ~callback: (WrapTypes.any, tFunction) => unit=?, ()) => Js.Promise.t<unit> = "loadLanguages"
@send
external reloadResources: (t, ~lng: Language.t=?, ~ns: NameSpace.t=?, ~cb: () => unit=?, ()) => Js.Promise.t<unit> = "reloadResources"
@send
external reloadResourcesNs: (t, @as(json`null`) _, NameSpace.t, ~cb: () => unit=?, ()) => Js.Promise.t<unit> = "reloadResources"
@send
external setDefaultNamespace: (t, string) => unit = "setDefaultNamespace"
@send
external dir: (t, ~lng: string=?, ()) => string = "dir"

@send
external createInstance: (t, ~options: InitOptions.t=?, ~callback: (WrapTypes.any, tFunction) => unit=?, ()) => t = "createInstance"
@send
external cloneInstance: (t, ~options: InitOptions.t=?, ~callback: (WrapTypes.any, tFunction) => unit=?, ()) => t = "cloneInstance"

@send
external onInitialized: (t, @as("initialized") _, (InitOptions.t) => unit) => unit = "on"
@send
external onLoaded: (t, @as("loaded") _, (bool) => unit) => unit = "on"
@send
external onFailedLoading: (t, @as("failedLoading") _, (string, string, string) => unit) => unit = "on"
@send
external onMissingKey: (t, @as("missingKey") _, (array<string>, string, string, string) => unit) => unit = "on"
@send
external onAdded: (t, @as("added") _, (string, string) => unit) => unit = "on"
@send
external onRemoved: (t, @as("removed") _, (string, string) => unit) => unit = "on"
@send
external onLanguageChanged: (t, @as("languageChanged") _, (string) => unit) => unit = "on"
@send
external offEvent: (t, string, (WrapTypes.any) => unit) => unit = "off"
@send
external emitEvent: (t, string) => unit = "emit"

// TODO @send getResource
// TODO @send addResource
// TODO @send addResources
// TODO @send addResourceBundle
@send
external hasResourceBundle: (t, string, string) => bool = "hasResourceBundle"
@send
external getResourceBundle: (t, string, string) => WrapTypes.any = "getResourceBundle"
@send
external removeResourceBundle: (t, string, string) => t = "removeResourceBundle"

@get external language: t => string = "language"
@get external languages: t => array<string> = "languages"
// TODO @get modules
// TODO @get services
// TODO @get store
// TODO @get format
@get external options: t => InitOptions.t = "options"
@get external isInitialized: t => bool = "isInitialized"
