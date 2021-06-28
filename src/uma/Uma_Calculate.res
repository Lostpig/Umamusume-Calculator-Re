open Uma_Variable
open Uma_Factor

let ofSpeed = Attribute.f_pack(Attribute.Speed)
let ofStamina = Attribute.f_pack(Attribute.Stamina)
let ofPower = Attribute.f_pack(Attribute.Power)
let ofGuts = Attribute.f_pack(Attribute.Guts)
let ofKnowledge = Attribute.f_pack(Attribute.Knowledge)
let unpack = Attribute.f_unpack

type type_attrCalc = Attribute.f_packed => Attribute.f_packed
type type_calcOf = (Preference.data, Status.data, Race.data) => type_attrCalc

let combToStr = (pref, status, race) => {
  let (mood, strategy) = status
  let (p_field, p_distance, p_strategy) = pref
  let (field, fstatus, length) = race

  let sign = `



    ${Mood.toString(mood)}|



    ${Strategy.toString(strategy)}|



    ${Field.toString(field)}|



    ${FieldStatus.toString(fstatus)}|



    ${Belt.Int.toString(length)}|



    ${Rank.toString(p_field)}|



    ${Rank.toString(p_distance)}|



    ${Rank.toString(p_strategy)}|



  `
  sign
}

let calcMap = Belt.HashMap.String.make(~hintSize=32)
let calcOf: type_calcOf = (pref, status, race) => {
  let sign = combToStr(pref, status, race)
  let t = calcMap->Belt.HashMap.String.get(sign)

  let calc = switch t {
  | Some(c) => c
  | None => {
      let (mood, _) = status
      let (field, fstatus, _) = race
      let (_, _, strategy) = pref

      let c = (val: Attribute.f_packed) =>
        val
        ->MoodFactor.adjustWith(mood)
        ->FieldFactor.adjustAttribute(field, fstatus)
        ->PreferenceFactor.withStrategy(strategy)
      calcMap->Belt.HashMap.String.set(sign, c)
      c
    }
  }
  calc
}
let adjustAttrs = (attrs, calc: type_attrCalc) => {
  let (speed, stamina, power, guts, knowledge) = Attribute.toFloats(attrs)

  let adjusted = (
    ofSpeed(speed)->calc->unpack,
    ofStamina(stamina)->calc->unpack,
    ofPower(power)->calc->unpack,
    ofGuts(guts)->calc->unpack,
    ofKnowledge(knowledge)->calc->unpack,
  )

  adjusted
}

let c_baseSpeed = length => 20.0 -. (length -. 2000.0) /. 1000.0
let computeBaseAbility = (adjAttrs, status, race) => {
  let (_, stamina, _, guts, _) = adjAttrs
  let (_, strategy) = status
  let (field, fstatus, length) = race
  let f_length = length->Belt.Int.toFloat

  let baseSpeed = c_baseSpeed(f_length)
  let baseHp = f_length +. 0.8 *. StrategyFactor.ofHp(stamina, strategy)
  let skillPlusHp = baseHp +. 150.0 // TODO
  let hpCoef = FieldFactor.adjustCoef(1.0, field, fstatus)
  let spurtCoef = 1.0 +. 200.0 /. Js.Math.sqrt(guts *. 600.0)

  (baseSpeed, baseHp, skillPlusHp, hpCoef, spurtCoef)
}
