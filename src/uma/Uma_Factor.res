open Uma_Variable

module PreferenceFactor = {
  let withStrategy = (packed, rank) => {
    let (v, attr) = packed
    let adjv = switch attr {
    | Attribute.Knowledge =>
      switch rank {
      | Rank.S => v *. 1.1
      | Rank.A => v
      | Rank.B => v *. 0.85
      | Rank.C => v *. 0.75
      | Rank.D => v *. 0.6
      | Rank.E => v *. 0.4
      | Rank.F => v *. 0.2
      | Rank.G => v *. 0.1
      }
    | _ => v
    }

    (adjv, attr)
  }
}

module MoodFactor = {
  let adjustWith = (packed, mood) => {
    let (v, attr) = packed
    let adjv = switch mood {
    | Mood.Perfect => v *. 1.04
    | Mood.Good => v *. 1.02
    | Mood.Normal => v
    | Mood.Bad => v *. 0.98
    | Mood.Terrible => v *. 0.96
    }

    (adjv, attr)
  }
}

module FieldFactor = {
  let adjustAttribute = (packed, field: Field.t, fstatus: FieldStatus.t) => {
    let (v, attr) = packed
    let adjv = switch (attr, field, fstatus) {
    | (Attribute.Speed, _, FieldStatus.Bad) => v -. 50.0
    | (Attribute.Power, Field.Turf, FieldStatus.Good) => v
    | (Attribute.Power, Field.Turf, _)
    | (Attribute.Power, Field.Dirt, FieldStatus.SlightlyPoor) =>
      v -. 50.0
    | (Attribute.Power, Field.Dirt, _) => v -. 100.0
    | _ => v
    }

    (adjv, attr)
  }
  let adjustCoef = (coef, field: Field.t, fstatus: FieldStatus.t) => {
    switch (field, fstatus) {
    | (Field.Turf, FieldStatus.Poor | FieldStatus.Bad)
    | (Field.Dirt, FieldStatus.Bad) =>
      coef *. 1.02
    | (Field.Dirt, FieldStatus.Poor) => coef *. 1.01
    | _ => coef
    }
  }
}

module StrategyFactor = {
  let ofHp = (param, strategy) => {
    param *.
    switch strategy {
    | Strategy.Escape => 0.95
    | Strategy.Leading => 0.89
    | Strategy.Between => 1.0
    | Strategy.Pushing => 0.995
    }
  }
}
