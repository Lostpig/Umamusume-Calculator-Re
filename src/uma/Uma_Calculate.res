open Uma_Variable
open Uma_Factor

let pack = Attribute.f_pack
let unpack = Attribute.f_unpack

type type_adjustAttrs = (Attribute.data, Preference.data, Status.data, Race.data) => Attribute.data

let adjustAttrs: type_adjustAttrs = (attrs, pref, status, race) => {
  let (speed, stamina, power, guts, knowledge) = Attribute.toFloats(attrs)
  let (mood, _) = status
  let (field, fstatus, _) = race
  let (_, _, strategy) = pref

  let adjust = val =>
    val
    ->MoodFactor.adjustWith(mood)
    ->FieldFactor.adjustAttribute(field, fstatus)
    ->PreferenceFactor.withStrategy(strategy)
    ->unpack

  let adjusted = (
    speed->pack(Attribute.Speed)->adjust,
    stamina->pack(Attribute.Stamina)->adjust,
    power->pack(Attribute.Power)->adjust,
    guts->pack(Attribute.Guts)->adjust,
    knowledge->pack(Attribute.Knowledge)->adjust,
  )
  Attribute.toInts(adjusted)
}
