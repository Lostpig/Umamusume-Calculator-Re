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

  let sign =
    `|${Mood.toString(mood)}|${Strategy.toString(strategy)}` ++
    `|${Field.toString(field)}|${FieldStatus.toString(fstatus)}` ++
    `|${Belt.Int.toString(length)}|${Rank.toString(p_field)}` ++
    `|${Rank.toString(p_distance)}|${Rank.toString(p_strategy)}|`
  sign
}

let calcOf: type_calcOf = (pref, status, race) => {
  let (mood, _) = status
  let (field, fstatus, _) = race
  let (_, _, strategy) = pref

  (val: Attribute.f_packed) =>
    val
    ->MoodFactor.forAttribute(mood)
    ->FieldFactor.forAttribute(field, fstatus)
    ->PreferenceFactor.forAttributeOfStrategy(strategy)
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

module BaseParameter = {
  type t = {
    baseSpeed: float,
    hp: float,
    hpCoef: float,
    spurtCoef: float,
    minSpeed: float,
  }

  let c_baseSpeed = distacne => 20.0 -. (distacne -. 2000.0) /. 1000.0
  let c_hp = (distacne, stamina, strategy: Strategy.t) =>
    distacne +. 0.8 *. StrategyFactor.forHp(stamina, strategy)

  let c_hpCoef = (field, fstatus) => FieldFactor.forHpCoef(1.0, field, fstatus)
  let c_spurtCoef = guts => 1.0 +. 200.0 /. sqrt(guts *. 600.0)
  let c_minSpeed = (baseSpeed, guts) => 0.85 *. baseSpeed +. 0.01 *. sqrt(guts *. 200.0)
  let c_hpWithSkill = (hp, normal, gold) => {
    let f_n = normal->Belt.Int.toFloat
    let f_g = gold->Belt.Int.toFloat

    hp +. 150.0 *. f_n +. 500.0 *. f_g
  }

  let make = (~attrs: Attribute.f_data, ~race: Race.data, ~status: Status.data) => {
    let (field, fstatus, distacne) = race
    let (_, stamina, _, guts, _) = attrs
    let (_, strategy) = status
    let f_dis = distacne->Belt.Int.toFloat

    let baseSpeed = c_baseSpeed(f_dis)
    {
      baseSpeed: baseSpeed,
      hp: c_hp(f_dis, stamina, strategy),
      hpCoef: c_hpCoef(field, fstatus),
      spurtCoef: c_spurtCoef(guts),
      minSpeed: c_minSpeed(baseSpeed, guts),
    }
  }
}

let pow = (base, exp) => Js.Math.pow_float(~base, ~exp)
let sqrt = num => Js.Math.sqrt(num)
let log10 = num => Js.Math.log10(num)

let common_target_speed = (~stage, ~baseSpeed, ~speed, ~knowledge, ~strategy, ~distanceRank) => {
  let temp =
    baseSpeed->StrategyFactor.forSpeed(strategy, stage) +.
      (knowledge /. 5500.0 *. log10(knowledge *. 0.1) -. 0.65 /. 2.0) *. 0.01 *. baseSpeed

  switch stage {
  | RaceStage.Last =>
    temp +. (sqrt(500.0 *. speed) *. 0.002)->PreferenceFactor.forSpeedOfDistance(distanceRank)
  | _ => temp
  }
}
let common_acceleration = (~stage, ~power, ~strategy, ~fieldRank, ~distanceRank) => {
  0.0006 *.
  sqrt(500.0 *. power)
  ->StrategyFactor.forAcceleration(strategy, stage)
  ->PreferenceFactor.forAccelerationOfField(fieldRank)
  ->PreferenceFactor.forAccelerationOfDistance(distanceRank)
}
let hp_cost = (speed, baseSpeed, hpCoef) => {
  20.0 *. hpCoef *. pow(speed -. baseSpeed +. 12.0, 2.0) /. 144.0
}
let hp_cost_change = (start_speed, end_speed, acc, baseSpeed, hpCoef) => {
  let c =
    (pow(end_speed -. baseSpeed +. 12.0, 3.0) -. pow(start_speed -. baseSpeed +. 12.0, 3.0)) /.
    (3.0 *. acc) /. 144.0
  20.0 *. hpCoef *. c
}

module StageStarting = {
  type t = {
    time: float,
    distance: float,
    acceleration: float,
    speed: (float, float),
    hp_cost: float,
  }

  let c_start_speed = () => 3.0
  let c_target_speed = baseSpeed => baseSpeed *. 0.85
  let c_acceleration = (power, strategy, fieldRank, distanceRank) => {
    24.0 +.
    common_acceleration(~stage=RaceStage.First, ~power, ~strategy, ~fieldRank, ~distanceRank)
  }

  let make = (
    ~attrs: Attribute.f_data,
    ~status: Status.data,
    ~preferences: Preference.data,
    ~bp: BaseParameter.t,
  ) => {
    let (_, _, power, _, _) = attrs
    let (_, strategy) = status
    let (fieldRank, disRank, _) = preferences

    let v0 = c_start_speed()
    let v1 = c_target_speed(bp.baseSpeed)
    let a = c_acceleration(power, strategy, fieldRank, disRank)
    let time = (v1 -. v0) /. a
    let dis = (v1 +. v0) *. time *. 0.5

    {
      time: time,
      distance: dis,
      acceleration: a,
      speed: (v0, v1),
      hp_cost: hp_cost_change(v0, v1, a, bp.baseSpeed, bp.hpCoef),
    }
  }
}

module StageFirst = {
  type t = {
    time: float,
    distance: float,
    acc: (float, float),
    cru: (float, float),
    acceleration: float,
    speed: (float, float, float), // start, target, real_end
    hp_cost: float,
  }

  let c_start_speed = startingTargetSpeed => startingTargetSpeed
  let c_target_speed = (baseSpeed, strategy, distanceRank, speed, knowledge) => {
    common_target_speed(
      ~stage=RaceStage.First,
      ~baseSpeed,
      ~speed,
      ~knowledge,
      ~strategy,
      ~distanceRank,
    )
  }
  let c_acceleration = (power, strategy, fieldRank, distanceRank) => {
    common_acceleration(~stage=RaceStage.First, ~power, ~strategy, ~fieldRank, ~distanceRank)
  }
  let c_stage_max_distance = (startingDistance, distance) => {
    distance /. 6.0 -. startingDistance
  }

  let make = (
    ~starting: StageStarting.t,
    ~race: Race.data,
    ~attrs: Attribute.f_data,
    ~status: Status.data,
    ~preferences: Preference.data,
    ~bp: BaseParameter.t,
  ) => {
    let (_, startingEndSpeed) = starting.speed
    let (_, strategy) = status
    let (fieldRank, disRank, _) = preferences
    let (speed, _, power, _, knowledge) = attrs
    let (_, _, dis) = race
    let f_dis = dis->Belt.Int.toFloat

    let v0 = c_start_speed(startingEndSpeed)
    let vt = c_target_speed(bp.baseSpeed, strategy, disRank, speed, knowledge)
    let a = vt < v0 ? -1.2 : c_acceleration(power, strategy, fieldRank, disRank)

    let max_dis = c_stage_max_distance(starting.distance, f_dis)
    let acc_dis = (v0 +. vt) *. 0.5 *. (vt -. v0) /. a
    let v1 = if acc_dis > max_dis {
      sqrt(a *. max_dis *. 2.0 +. v0 *. v0)
    } else {
      vt
    }

    let acc_time = (v1 -. v0) /. a
    let real_acc_dis = (v0 +. v1) *. 0.5 *. acc_time
    let cru_dis = max_dis -. real_acc_dis
    let cru_time = cru_dis /. v1

    let hp =
      hp_cost_change(v0, v1, a, bp.baseSpeed, bp.hpCoef) +.
      hp_cost(v1, bp.baseSpeed, bp.hpCoef) *. cru_time

    {
      time: acc_time +. cru_time,
      distance: max_dis,
      acc: (acc_time, real_acc_dis),
      cru: (cru_time, cru_dis),
      acceleration: a,
      speed: (v0, vt, v1),
      hp_cost: hp,
    }
  }
}

module StageMiddle = {
  type t = {
    time: float,
    acceleration: float,
    speed: (float, float),
    distance: float,
    acc: (float, float),
    cru: (float, float),
    hp_cost: float,
  }

  let c_stage_distance = distance => {
    distance *. 0.5
  }

  let make = (
    ~first: StageFirst.t,
    ~attrs: Attribute.f_data,
    ~bp: BaseParameter.t,
    ~status: Status.data,
    ~preferences: Preference.data,
    ~race: Race.data,
  ) => {
    let (_, _, firstEndSpeed) = first.speed
    let (speed, _, power, _, knowledge) = attrs
    let (_, strategy) = status
    let (fieldRank, disRank, _) = preferences
    let (_, _, dis) = race
    let f_dis = dis->Belt.Int.toFloat

    let v0 = firstEndSpeed
    let v1 = common_target_speed(
      ~stage=RaceStage.Middle,
      ~baseSpeed=bp.baseSpeed,
      ~speed,
      ~knowledge,
      ~strategy,
      ~distanceRank=disRank,
    )
    let a =
      v1 < v0
        ? -0.8
        : common_acceleration(
            ~stage=RaceStage.Middle,
            ~power,
            ~strategy,
            ~fieldRank,
            ~distanceRank=disRank,
          )
    let stage_dis = c_stage_distance(f_dis)

    let acc_time = (v1 -. v0) /. a
    let acc_dis = (v1 +. v0) *. acc_time *. 0.5

    let cru_dis = stage_dis -. acc_dis
    let cru_time = cru_dis /. v1

    let hp =
      hp_cost_change(v0, v1, a, bp.baseSpeed, bp.hpCoef) +.
      hp_cost(v1, bp.baseSpeed, bp.hpCoef) *. cru_time

    {
      time: acc_time +. cru_time,
      acceleration: a,
      speed: (v0, v1),
      distance: stage_dis,
      acc: (acc_time, acc_dis),
      cru: (cru_time, cru_dis),
      hp_cost: hp,
    }
  }
}

module StageLast = {
  type t = {
    time: float,
    distance: float,
    acceleration: float,
    speed: (float, float, float),
    acc: (float, float),
    cru: (float, float),
    hp_cost: float,
  }
  type data = {
    final: t,
    spurt: t,
    exhaustion: t,
  }

  let c_hp_to_acc_time = (v0, a, hp, baseSpeed, hpCoef) => {
    let temp1 = 432.0 *. a *. hp /. 20.0 /. hpCoef +. pow(v0 -. baseSpeed +. 12.0, 3.0)
    let temp2 = pow(temp1, 1.0 /. 3.0)
    temp2 -. 12.0 -. v0 +. baseSpeed
  }
  let c_spurt_max_speed = (baseSpeed, speed, strategy, disRank) => {
    (baseSpeed->StrategyFactor.forSpeed(strategy, RaceStage.Last) +.
      sqrt(speed /. 500.0)->PreferenceFactor.forSpeedOfDistance(disRank)) *. 1.05 +.
      sqrt(speed /. 500.0)->PreferenceFactor.forSpeedOfDistance(disRank)
  }

  let c_exhaustion_part = (v0, v1, dis) => {
    let d = -1.2
    let max_dec_dis = (v0 +. v1) *. 0.5 *. (v1 -. v0) /. d
    let vr = max_dec_dis > dis ? sqrt(d *. dis *. 2.0 +. v0 *. v0) : v1
    let dec_time = (vr -. v0) /. d
    let dec_dis = (vr +. v0) *. dec_time *. 0.5
    let cru_dis = dis -. dec_dis
    let cru_time = cru_dis /. vr

    {
      time: dec_time +. cru_time,
      distance: dis,
      acceleration: d,
      speed: (v0, v1, vr),
      acc: (dec_time, dec_dis),
      cru: (cru_time, cru_dis),
      hp_cost: 0.0,
    }
  }
  let c_final_part = (v0, v1, a, dis, exhp, baseSpeed, hpCoef, spurtCoef) => {
    let max_acc_dis = (v0 +. v1) *. 0.5 *. (v1 -. v0) /. a
    let vr = max_acc_dis > dis ? sqrt(a *. dis *. 2.0 +. v0 *. v0) : v1
    let acc_time = (vr -. v0) /. a
    let acc_need_hp = hp_cost_change(v0, vr, a, baseSpeed, hpCoef) *. spurtCoef
    if acc_need_hp > exhp {
      let real_acc_time = c_hp_to_acc_time(v0, a, acc_need_hp /. spurtCoef, baseSpeed, hpCoef)
      let ex_v = v0 +. a *. real_acc_time
      let acc_dis = (v0 +. ex_v) *. real_acc_time *. 0.5
      let p = {
        time: real_acc_time,
        distance: acc_dis,
        acceleration: a,
        speed: (v0, v1, ex_v),
        acc: (real_acc_time, acc_dis),
        cru: (0.0, 0.0),
        hp_cost: exhp,
      }
      p
    } else {
      let acc_dis = (v0 +. vr) *. acc_time *. 0.5
      let cru_ex_hp = exhp -. acc_need_hp

      let cru_dis = dis -. acc_dis
      let cru_time = cru_dis /. vr
      let cru_hp_pers = hp_cost(vr, baseSpeed, hpCoef) *. spurtCoef
      let cru_need_hp = cru_hp_pers *. cru_time
      if cru_need_hp > cru_ex_hp {
        let real_cru_time = cru_ex_hp /. cru_hp_pers
        let real_cru_dis = vr *. real_cru_time
        let p = {
          time: acc_time +. real_cru_time,
          acceleration: a,
          distance: acc_dis +. real_cru_dis,
          speed: (v0, v1, vr),
          acc: (acc_time, acc_dis),
          cru: (real_cru_time, real_cru_dis),
          hp_cost: exhp,
        }
        p
      } else {
        let p = {
          time: acc_time +. cru_time,
          distance: acc_dis +. cru_dis,
          acceleration: a,
          speed: (v0, v1, vr),
          acc: (acc_time, acc_dis),
          cru: (cru_time, cru_dis),
          hp_cost: acc_need_hp +. cru_need_hp,
        }
        p
      }
    }
  }
  let c_spurt_dis = (vMax, v1, exHp, stageDis, hpCoef, spurtCoef, baseSpeed) => {
    let f1 =
      exHp -.
      (stageDis -. 60.0) *.
      20.0 *.
      hpCoef *.
      spurtCoef *.
      pow(v1 -. baseSpeed +. 12.0, 2.0) /.
      144.0 /.
      v1
    let f2 =
      20.0 *.
      hpCoef *.
      spurtCoef /.
      144.0 *.
      (pow(vMax -. baseSpeed +. 12.0, 2.0) /. vMax -. pow(v1 -. baseSpeed +. 12.0, 2.0) /. v1)
    let p_dis = f1 /. f2 +. 60.0

    p_dis
  }

  type ti = {
    time: float,
    d: data,
  }
  let c_all_parts = (
    vMax,
    v0,
    v1,
    a,
    exHp,
    stageDis,
    spurtDis,
    hpCoef,
    spurtCoef,
    baseSpeed,
    vMin,
  ) => {
    let final_dis = stageDis -. spurtDis
    let final_part = c_final_part(v0, v1, a, final_dis, exHp, baseSpeed, hpCoef, spurtCoef)

    let (_, _, vf) = final_part.speed
    let real_spurt_dis = stageDis -. final_part.distance
    let final_ex_hp = exHp -. final_part.hp_cost
    let spurt_part = c_final_part(
      vf,
      vMax,
      a,
      real_spurt_dis,
      final_ex_hp,
      baseSpeed,
      hpCoef,
      spurtCoef,
    )

    let (_, _, vs) = spurt_part.speed
    let real_ex_dis = real_spurt_dis -. spurt_part.distance
    let ex_part = c_exhaustion_part(vs, vMin, real_ex_dis)

    {
      final: final_part,
      spurt: spurt_part,
      exhaustion: ex_part,
    }
  }
  let c_by_list = (
    vMax,
    v0,
    v1,
    a,
    exHp,
    stageDis,
    hpCoef,
    spurtCoef,
    baseSpeed,
    knowledge,
    vMin,
  ) => {
    let count = ((vMax -. v1) /. 0.1)->Belt.Int.fromFloat

    let arr =
      Belt.Array.range(1, count)
      ->Belt.Array.keepMap(i => {
        let dif = i->Belt.Int.toFloat *. 0.1
        let vs = vMax -. dif
        let spurt_dis = c_spurt_dis(vs, v1, exHp, stageDis, hpCoef, spurtCoef, baseSpeed)
        if spurt_dis <= 0.0 {
          None
        } else {
          let real_spurt_dis = spurt_dis > stageDis ? stageDis : spurt_dis
          let d = c_all_parts(
            vs,
            v0,
            v1,
            a,
            exHp,
            stageDis,
            real_spurt_dis,
            hpCoef,
            spurtCoef,
            baseSpeed,
            vMin,
          )

          let sum_time = d.final.time +. d.spurt.time +. d.exhaustion.time
          let temp = {
            time: sum_time,
            d: d,
          }
          Some(temp)
        }
      })
      ->Js.Array2.sortInPlaceWith((a, b) => a.time > b.time ? 1 : -1)

    let arrs = if Belt.Array.length(arr) == 0 {
      let d = c_all_parts(v1, v0, v1, a, exHp, stageDis, 0.0, hpCoef, spurtCoef, baseSpeed, vMin)
      [
        {
          d: d,
          time: d.final.time +. d.spurt.time +. d.exhaustion.time,
        },
      ]
    } else {
      arr
    }

    let perc = 0.15 +. 0.0005 *. knowledge
    let idx = (Js.Array2.length(arrs)->Belt.Int.toFloat *. (1.0 -. perc))->Belt.Int.fromFloat

    let res = if idx >= Js.Array2.length(arrs) {
      let end = Js.Array2.length(arrs) - 1
      arrs[end]
    } else {
      arrs[idx]
    }
    res.d
  }

  let make = (
    ~middle: StageMiddle.t,
    ~attrs: Attribute.f_data,
    ~bp: BaseParameter.t,
    ~status: Status.data,
    ~preferences: Preference.data,
    ~race: Race.data,
    ~costedHp: float,
  ) => {
    let (_, middleEndSpeed) = middle.speed
    let (speed, _, power, _, knowledge) = attrs
    let (_, strategy) = status
    let (fieldRank, disRank, _) = preferences
    let (_, _, dis) = race
    let f_dis = dis->Belt.Int.toFloat

    let v0 = middleEndSpeed
    let v1 = common_target_speed(
      ~stage=RaceStage.Last,
      ~baseSpeed=bp.baseSpeed,
      ~speed,
      ~knowledge,
      ~strategy,
      ~distanceRank=disRank,
    )
    let vmax = c_spurt_max_speed(bp.baseSpeed, speed, strategy, disRank)
    let a = common_acceleration(
      ~stage=RaceStage.Last,
      ~power,
      ~strategy,
      ~fieldRank,
      ~distanceRank=disRank,
    )
    let stage_dis = f_dis /. 3.0
    let ex_hp = bp.hp -. costedHp

    let spurt_dis = c_spurt_dis(vmax, v1, ex_hp, stage_dis, bp.hpCoef, bp.spurtCoef, bp.baseSpeed)
    if spurt_dis >= stage_dis {
      c_all_parts(
        vmax,
        v0,
        v1,
        a,
        ex_hp,
        stage_dis,
        stage_dis,
        bp.hpCoef,
        bp.spurtCoef,
        bp.baseSpeed,
        bp.minSpeed,
      )
    } else {
      c_by_list(
        vmax,
        v0,
        v1,
        a,
        ex_hp,
        stage_dis,
        bp.hpCoef,
        bp.spurtCoef,
        bp.baseSpeed,
        knowledge,
        bp.minSpeed,
      )
    }
  }
}

type raceResult = {
  attrs: Attribute.data,
  base: BaseParameter.t,
  starting: StageStarting.t,
  first: StageFirst.t,
  middle: StageMiddle.t,
  final: StageLast.t,
  spurt: StageLast.t,
  exhaustion: StageLast.t,
}
let clac_race = (
  ~attrs: Attribute.data,
  ~preferences: Preference.data,
  ~status: Status.data,
  ~race: Race.data,
) => {
  let calc = calcOf(preferences, status, race)
  let adjAttrs = adjustAttrs(attrs, calc)
  let bp = BaseParameter.make(~attrs=adjAttrs, ~race, ~status)
  let starting = StageStarting.make(~attrs=adjAttrs, ~status, ~preferences, ~bp)
  let first = StageFirst.make(~starting, ~attrs=adjAttrs, ~status, ~preferences, ~race, ~bp)
  let middle = StageMiddle.make(~first, ~attrs=adjAttrs, ~status, ~preferences, ~race, ~bp)

  let hp_costed = starting.hp_cost +. first.hp_cost +. middle.hp_cost
  let last = StageLast.make(
    ~middle,
    ~attrs=adjAttrs,
    ~status,
    ~preferences,
    ~race,
    ~bp,
    ~costedHp=hp_costed,
  )
  {
    attrs: Attribute.toInts(adjAttrs),
    base: bp,
    starting: starting,
    first: first,
    middle: middle,
    final: last.final,
    spurt: last.spurt,
    exhaustion: last.exhaustion,
  }
}
