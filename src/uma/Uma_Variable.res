// Game data structure

// #region Enums
module type IsEnum = {
  type t

  let items: list<t>
  let toString: t => string
  let toEnum: string => t
}
let toEnumTuple = (type enumType, enum: module(IsEnum with type t = enumType)) => {
  let module(Enum) = enum
  (Enum.items, Enum.toString, Enum.toEnum)
}
type enumTuple<'a> = (list<'a>, 'a => string, string => 'a)

module Rank = {
  type t = S | A | B | C | D | E | F | G

  let items = list{S, A, B, C, D, E, F, G}
  let toString = (val: t) => {
    switch val {
    | S => "S"
    | A => "A"
    | B => "B"
    | C => "C"
    | D => "D"
    | E => "E"
    | F => "F"
    | G => "G"
    }
  }
  let toEnum = (str: string) => {
    switch str {
    | "S" => S
    | "A" => A
    | "B" => B
    | "C" => C
    | "D" => D
    | "E" => E
    | "F" => F
    | "G" => G
    | _ => G
    }
  }
}
let enumRank = toEnumTuple(module(Rank))

module Mood = {
  type t = Perfect | Good | Normal | Bad | Terrible

  let items = list{Perfect, Good, Normal, Bad, Terrible}
  let toString = (val: t) => {
    switch val {
    | Perfect => "Perfect"
    | Good => "Good"
    | Normal => "Normal"
    | Bad => "Bad"
    | Terrible => "Terrible"
    }
  }
  let toEnum = (val: string) => {
    switch val {
    | "Perfect" => Perfect
    | "Good" => Good
    | "Normal" => Normal
    | "Bad" => Bad
    | "Terrible" => Terrible
    | _ => Perfect
    }
  }
}
let enumMood = toEnumTuple(module(Mood))

module Strategy = {
  type t = Escape | Leading | Between | Pushing

  let items = list{Escape, Leading, Between, Pushing}
  let toString = (val: t) => {
    switch val {
    | Escape => "Escape"
    | Leading => "Leading"
    | Between => "Between"
    | Pushing => "Pushing"
    }
  }
  let toEnum = (val: string) => {
    switch val {
    | "Escape" => Escape
    | "Leading" => Leading
    | "Between" => Between
    | "Pushing" => Pushing
    | _ => Leading
    }
  }
}
let enumStrategy = toEnumTuple(module(Strategy))

module Field = {
  type t = Turf | Dirt

  let items = list{Turf, Dirt}
  let toString = (val: t) => {
    switch val {
    | Turf => "Turf"
    | Dirt => "Dirt"
    }
  }
  let toEnum = (val: string) => {
    switch val {
    | "Turf" => Turf
    | "Dirt" => Dirt
    | _ => Turf
    }
  }
}
let enumField = toEnumTuple(module(Field))

module FieldStatus = {
  type t = Good | SlightlyPoor | Poor | Bad

  let items = list{Good, SlightlyPoor, Poor, Bad}
  let toString = (val: t) => {
    switch val {
    | Good => "Good"
    | SlightlyPoor => "SlightlyPoor"
    | Poor => "Poor"
    | Bad => "Bad"
    }
  }
  let toEnum = (val: string) => {
    switch val {
    | "Good" => Good
    | "SlightlyPoor" => SlightlyPoor
    | "Poor" => Poor
    | "Bad" => Bad
    | _ => Good
    }
  }
}
let enumFieldStatus = toEnumTuple(module(FieldStatus))

module Distance = {
  type t = Short | Mile | Medium | Long

  let items = list{Short, Mile, Medium, Long}
  let toString = (val: t) => {
    switch val {
    | Short => "Short"
    | Mile => "Mile"
    | Medium => "Medium"
    | Long => "Long"
    }
  }
  let toEnum = (val: string) => {
    switch val {
    | "Short" => Short
    | "Mile" => Mile
    | "Medium" => Medium
    | "Long" => Long
    | _ => Medium
    }
  }
  let fromLength = (len: int) => {
    switch len {
    | v if v > 2400 => Long
    | v if v > 1800 => Medium
    | v if v > 1400 => Mile
    | _ => Short
    }
  }
}
let enumDistance = toEnumTuple(module(Distance))

// not used
module Weather = {
  type t = Sunny | Rain | Cloudy
  let items = list{Sunny, Rain, Cloudy}

  let toString = (val: t) => {
    switch val {
    | Sunny => "Sunny"
    | Rain => "Rain"
    | Cloudy => "Cloudy"
    }
  }
  let toEnum = (val: string) => {
    switch val {
    | "Sunny" => Sunny
    | "Rain" => Rain
    | "Cloudy" => Cloudy
    | _ => Sunny
    }
  }
}
// #endregion

module RaceStage = {
  type t = First | Middle | Last
}

module Attribute = {
  type kind = Speed | Stamina | Power | Guts | Knowledge
  type data<'a> = ('a, 'a, 'a, 'a, 'a)
  type dataInt = data<int>
  type dataFloat = data<float>
  type kindValue<'a> = ('a, kind)

  let toFloats: dataInt => dataFloat = (data) => {
    let fi = Belt.Float.fromInt
    let (speed, stamina, power, guts, knowledge) = data
    (speed->fi, stamina->fi, power->fi, guts->fi, knowledge->fi)
  }
  let toInts: dataFloat => dataInt = (data) => {
    let ti = Belt.Float.toInt
    let (speed, stamina, power, guts, knowledge) = data
    (speed->ti, stamina->ti, power->ti, guts->ti, knowledge->ti)
  }
  let update = (data: data<'a>, kind, value: 'a) => {
    let (speed, stamina, power, guts, knowledge) = data
    switch kind {
    | Speed => (value, stamina, power, guts, knowledge)
    | Stamina => (speed, value, power, guts, knowledge)
    | Power => (speed, stamina, value, guts, knowledge)
    | Guts => (speed, stamina, power, value, knowledge)
    | Knowledge => (speed, stamina, power, guts, value)
    }
  }
  let get = (data: data<'a>, kind) => {
    let (speed, stamina, power, guts, knowledge) = data
    switch kind {
    | Speed => speed
    | Stamina => stamina
    | Power => power
    | Guts => guts
    | Knowledge => knowledge
    }
  }

  let bindKind = (kind, v: 'a) => {
    (v, kind)
  }
  let extractValue = (kv: kindValue<'a>) => {
    let (v, _) = kv
    v
  }
}

module Preference = {
  type kind = Field | Distance | Strategy
  type data = (Rank.t, Rank.t, Rank.t)

  let update = (data, kind, value: Rank.t) => {
    let (field, distance, strategy) = data
    switch kind {
    | Field => (value, distance, strategy)
    | Distance => (field, value, strategy)
    | Strategy => (field, distance, value)
    }
  }
  let get = (data, kind) => {
    let (field, distance, strategy) = data
    switch kind {
    | Field => field
    | Distance => distance
    | Strategy => strategy
    }
  }
}

module Status = {
  type kind = Mood(Mood.t) | Strategy(Strategy.t)
  type data = (Mood.t, Strategy.t)

  let update = (data, kind) => {
    let (mood, strategy) = data
    switch kind {
    | Mood(v) => (v, strategy)
    | Strategy(v) => (mood, v)
    }
  }
}

module Race = {
  type kind = Field(Field.t) | FStatus(FieldStatus.t) | Length(int) | Distance(Distance.t)
  type data = (Field.t, FieldStatus.t, int)

  let update = (data, kind) => {
    let (field, fstatus, length) = data
    switch kind {
    | Field(v) => (v, fstatus, length)
    | FStatus(v) => (field, v, length)
    | Length(v) => (field, fstatus, v)
    | _ => (field, fstatus, length)
    }
  }
}
