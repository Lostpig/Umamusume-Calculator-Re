module Rank = {
  type t = S | A | B | C | D | E | F | G

  let list = list{S, A, B, C, D, E, F, G}
  let string = (val: t) => {
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
  let convert = (str: string) => {
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

module Mood = {
  type t = Perfect | Good | Normal | Bad | Terrible

  let list = list{Perfect, Good, Normal, Bad, Terrible}
  let string = (val: t) => {
    switch val {
    | Perfect => "Perfect"
    | Good => "Good"
    | Normal => "Normal"
    | Bad => "Bad"
    | Terrible => "Terrible"
    }
  }
  let convert = (val: string) => {
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

module Strategy = {
  type t = Escape | Leading | Between | Pushing
  let list = list{Escape, Leading, Between, Pushing}
  let string = (val: t) => {
    switch val {
    | Escape => "Escape"
    | Leading => "Leading"
    | Between => "Between"
    | Pushing => "Pushing"
    }
  }
  let convert = (val: string) => {
    switch val {
    | "Escape" => Escape
    | "Leading" => Leading
    | "Between" => Between
    | "Pushing" => Pushing
    | _ => Leading
    }
  }
}

module Field = {
  type t = Turf | Dirt
  let list = list{Turf, Dirt}
  let string = (val: t) => {
    switch val {
    | Turf => "Turf"
    | Dirt => "Dirt"
    }
  }
  let convert = (val: string) => {
    switch val {
    | "Turf" => Turf
    | "Dirt" => Dirt
    | _ => Turf
    }
  }
}

module FieldStatus = {
  type t = Good | SlightlyPoor | Poor | Bad
  let list = list{Good, SlightlyPoor, Poor, Bad}
  let string = (val: t) => {
    switch val {
    | Good => "Good"
    | SlightlyPoor => "SlightlyPoor"
    | Poor => "Poor"
    | Bad => "Bad"
    }
  }
  let convert = (val: string) => {
    switch val {
    | "Good" => Good
    | "SlightlyPoor" => SlightlyPoor
    | "Poor" => Poor
    | "Bad" => Bad
    | _ => Good
    }
  }
}

module Distance = {
  type t = Short | Mile | Medium | Long
  let list = list{Short, Mile, Medium, Long}
  let string = (val: t) => {
    switch val {
    | Short => "Short"
    | Mile => "Mile"
    | Medium => "Medium"
    | Long => "Long"
    }
  }
  let convert = (val: string) => {
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
