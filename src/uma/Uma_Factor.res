open Uma_Variable

module PreferenceFactor = {
  let forAttributeOfStrategy = (packed, rank) => {
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
  let forAccelerationOfField = (param, rank) => {
    param *.
    switch rank {
    | Rank.S => 1.05
    | Rank.A => 1.00
    | Rank.B => 0.90
    | Rank.C => 0.80
    | Rank.D => 0.70
    | Rank.E => 0.50
    | Rank.F => 0.30
    | Rank.G => 0.10
    }
  }
  let forAccelerationOfDistance = (param, rank) => {
    param *.
    switch rank {
    | Rank.S => 1.0
    | Rank.A => 1.0
    | Rank.B => 1.0
    | Rank.C => 1.0
    | Rank.D => 1.0
    | Rank.E => 0.6
    | Rank.F => 0.5
    | Rank.G => 0.4
    }
  }
  let forSpeedOfDistance = (param, rank) => {
    param *.
    switch rank {
    | Rank.S => 1.05
    | Rank.A => 1.0
    | Rank.B => 0.9
    | Rank.C => 0.8
    | Rank.D => 0.6
    | Rank.E => 0.4
    | Rank.F => 0.2
    | Rank.G => 0.1
    }
  }
}

module MoodFactor = {
  let forAttribute = (packed, mood) => {
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
  let forAttribute = (packed, field: Field.t, fstatus: FieldStatus.t) => {
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
  let forHpCoef = (baseCoef, field: Field.t, fstatus: FieldStatus.t) => {
    baseCoef *.
    switch (field, fstatus) {
    | (Field.Turf, FieldStatus.Poor | FieldStatus.Bad)
    | (Field.Dirt, FieldStatus.Bad) => 1.02
    | (Field.Dirt, FieldStatus.Poor) => 1.01
    | _ => 1.0
    }
  }
}

module StrategyFactor = {
  let forHp = (param, strategy) => {
    param *.
    switch strategy {
    | Strategy.Escape => 0.95
    | Strategy.Leading => 0.89
    | Strategy.Between => 1.0
    | Strategy.Pushing => 0.995
    }
  }
  let forAcceleration = (param, strategy, stage) => {
    open Strategy
    open RaceStage

    switch (strategy, stage) {
    | (Escape, First) => param *. 1.0
    | (Escape, Middle) => param *. 1.0
    | (Escape, Last) => param *. 0.996
    | (Leading, First) => param *. 0.985
    | (Leading, Middle) => param *. 1.0
    | (Leading, Last) => param *. 0.996
    | (Between, First) => param *. 0.975
    | (Between, Middle) => param *. 1.0
    | (Between, Last) => param *. 1.0
    | (Pushing, First) => param *. 0.945
    | (Pushing, Middle) => param *. 1.0
    | (Pushing, Last) => param *. 0.997
    }
  }
  let forSpeed = (param, strategy, stage) => {
    open Strategy
    open RaceStage

    switch (strategy, stage) {
    | (Escape, First) => param *. 1.0
    | (Escape, Middle) => param *. 0.98
    | (Escape, Last) => param *. 0.962
    | (Leading, First) => param *. 0.978
    | (Leading, Middle) => param *. 0.991
    | (Leading, Last) => param *. 0.975
    | (Between, First) => param *. 0.938
    | (Between, Middle) => param *. 0.998
    | (Between, Last) => param *. 0.994
    | (Pushing, First) => param *. 0.931
    | (Pushing, Middle) => param *. 1.0
    | (Pushing, Last) => param *. 1.0
    }
  }
}
