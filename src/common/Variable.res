
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
      | v when v > 2400 => Long
      | v when v > 1800 => Medium
      | v when v > 1400 => Mile
      | _ => Short
    }
  }
}
let enumDistance = toEnumTuple(module(Distance))

