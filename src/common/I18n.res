let init = () => {
  open I18next

  let options = InitOptions.make(
    ~lng = "en",
    ~fallbackLng = FallBackLng.single("en"),
    ~ns = NameSpace.string("common"),
    ~defaultNS = "common",
    ()
  )
  instance
    -> useModule(I18next_Modules.httpBackend)
    -> useModule(I18next_Modules.browserLanguageDetector)
    -> useModule(I18next_React.initReactI18next)
    -> initOptions(options, ())
}

let useSimpleTranslation = () => {
  open I18next_React

  let trans = useTranslation()

  (
    text => trans->Translation.translateSimple(text)->React.string,
    lng => trans->Translation.i18n->I18next.changeLanguage(~lng, ())->ignore
  )
}

let languages = ["en", "ja-JP", "zh-CN"]