open Uma_Variable
open Uma_Factor

// math function
let pow = (base, exp) => Js.Math.pow_float(~base, ~exp)
let sqrt = num => Js.Math.sqrt(num)
let log10 = num => Js.Math.log10(num)

// attribute box
let ofSpeed = Attribute.bindKind(Attribute.Speed)
let ofStamina = Attribute.bindKind(Attribute.Stamina)
let ofPower = Attribute.bindKind(Attribute.Power)
let ofGuts = Attribute.bindKind(Attribute.Guts)
let ofKnowledge = Attribute.bindKind(Attribute.Knowledge)
let extract = Attribute.extractValue

type raceOptions = {
  race: Race.data,
  preference: Preference.data,
  status: Status.data,
  attribute: Attribute.dataInt,
}

module Parameters = {
  type base = {
    baseSpeed: float,
    hp: float,
    hpCoef: float,
    spurtCoef: float,
    vMin: float,
  }
  type t = {
    base: base,
    attribute: Attribute.dataFloat,
    race: Race.data,
    preference: Preference.data,
    status: Status.data,
  }

  let makeAdjustAttribute = (options: raceOptions) => {
    let (mood, _) = options.status
    let (field, fstatus, _) = options.race
    let (_, _, strategy) = options.preference

    let adjust = (val: Attribute.kindValue<float>) =>
      val
      ->MoodFactor.forAttribute(mood)
      ->FieldFactor.forAttribute(field, fstatus)
      ->PreferenceFactor.forAttributeOfStrategy(strategy)
      ->Attribute.extractValue

    let (speed, stamina, power, guts, knowledge) = options.attribute->Attribute.toFloats
    (
      ofSpeed(speed)->adjust,
      ofStamina(stamina)->adjust,
      ofPower(power)->adjust,
      ofGuts(guts)->adjust,
      ofKnowledge(knowledge)->adjust,
    )
  }
  let makeBase = (options: raceOptions, adjustAttrs) => {
    let (field, fstatus, distacne) = options.race
    let (_, stamina, _, guts, _) = adjustAttrs
    let (_, strategy) = options.status
    let floatDistance = distacne->Belt.Int.toFloat

    let baseSpeed = 20.0 -. (floatDistance -. 2000.0) /. 1000.0
    let hp = floatDistance +. 0.8 *. StrategyFactor.forHp(stamina, strategy)
    let hpCoef = FieldFactor.forHpCoef(1.0, field, fstatus)
    let spurtCoef = 1.0 +. 200.0 /. sqrt(guts *. 600.0)
    let minSpeed = 0.85 *. baseSpeed +. 0.01 *. sqrt(guts *. 200.0)

    {
      baseSpeed: baseSpeed,
      hp: hp,
      hpCoef: hpCoef,
      spurtCoef: spurtCoef,
      vMin: minSpeed,
    }
  }

  let make = (options: raceOptions) => {
    let adjustAttrs = makeAdjustAttribute(options)
    let base = makeBase(options, adjustAttrs)

    {
      base: base,
      attribute: adjustAttrs,
      race: options.race,
      preference: options.preference,
      status: options.status,
    }
  }
}

let targetSpeed = (param: Parameters.t, stage: RaceStage.t) => {
  let {baseSpeed} = param.base
  let (speed, _, _, _, knowledge) = param.attribute
  let (_, strategy) = param.status

  let temp =
    baseSpeed->StrategyFactor.forSpeed(strategy, stage) +.
      (knowledge /. 5500.0 *. log10(knowledge *. 0.1) -. 0.65 /. 2.0) *. 0.01 *. baseSpeed

  switch stage {
  | RaceStage.Last => {
      let distanceRank = param.preference->Preference.get(Preference.Distance)
      temp +. (sqrt(500.0 *. speed) *. 0.002)->PreferenceFactor.forSpeedOfDistance(distanceRank)
    }
  | _ => temp
  }
}
let acceleration = (param: Parameters.t, stage: RaceStage.t) => {
  let power = param.attribute->Attribute.get(Attribute.Power)
  let (_, strategy) = param.status
  let (fieldRank, distanceRank, _) = param.preference

  0.0006 *.
  sqrt(500.0 *. power)
  ->StrategyFactor.forAcceleration(strategy, stage)
  ->PreferenceFactor.forAccelerationOfField(fieldRank)
  ->PreferenceFactor.forAccelerationOfDistance(distanceRank)
}
let hpCost = (~v: float, ~time: float, ~base: Parameters.base, ~isSpurt: bool) => {
  let {baseSpeed, hpCoef, spurtCoef} = base
  let perSecond = 20.0 *. hpCoef *. pow(v -. baseSpeed +. 12.0, 2.0) /. 144.0

  if isSpurt {
    perSecond *. spurtCoef *. time
  } else {
    perSecond *. time
  }
}
let hpCostChanging = (
  ~v0: float,
  ~v1: float,
  ~a: float,
  ~base: Parameters.base,
  ~isSpurt: bool,
) => {
  let {baseSpeed, hpCoef, spurtCoef} = base
  let cost =
    20.0 *.
    hpCoef *.
    (pow(v1 -. baseSpeed +. 12.0, 3.0) -. pow(v0 -. baseSpeed +. 12.0, 3.0)) /.
    (3.0 *. a) /. 144.0

  if isSpurt {
    cost *. spurtCoef
  } else {
    cost
  }
}
let realReachSpeed = (~v0: float, ~vTarget: float, ~a: float, ~distance: float) => {
  let reachNeedDistance = (v0 +. vTarget) *. 0.5 *. (vTarget -. v0) /. a
  if reachNeedDistance > distance {
    sqrt(a *. distance *. 2.0 +. v0 *. v0)
  } else {
    vTarget
  }
}

type stage = {
  speed: (float, float, float),
  acceleration: float,
  time: float,
  distance: float,
  cost: float,
  accelerateTime: float,
  accelerateDistance: float,
  accelerateCost: float,
  cruiseTime: float,
  cruiseDistance: float,
  cruiseCost: float,
}

module StageStarting = {
  let make = (param: Parameters.t) => {
    let {baseSpeed} = param.base

    let v0 = 3.0
    let v1 = baseSpeed *. 0.85
    let a = param->acceleration(RaceStage.First) +. 24.0
    let time = (v1 -. v0) /. a
    let distance = (v1 +. v0) *. time *. 0.5
    let cost = hpCostChanging(~v0, ~v1, ~a, ~base=param.base, ~isSpurt=false)

    {
      speed: (v0, v1, v1),
      acceleration: a,
      time: time,
      distance: distance,
      cost: cost,
      accelerateTime: time,
      accelerateDistance: distance,
      accelerateCost: cost,
      cruiseTime: 0.0,
      cruiseDistance: 0.0,
      cruiseCost: 0.0,
    }
  }
}
module StageFirst = {
  let make = (param: Parameters.t, ~starting: stage) => {
    let (_, startingV1, _) = starting.speed
    let (_, _, distance) = param.race
    let floatDistance = distance->Belt.Int.toFloat
    let stageDistance = floatDistance /. 6.0 -. starting.distance

    let v0 = startingV1
    let vTarget = param->targetSpeed(RaceStage.First)
    let a = vTarget < v0 ? -1.2 : param->acceleration(RaceStage.First)
    let v1 = realReachSpeed(~v0, ~vTarget, ~a, ~distance=stageDistance)

    let accelerateTime = (v1 -. v0) /. a
    let accelerateDistance = (v0 +. v1) *. 0.5 *. accelerateTime
    let cruiseDistance = stageDistance -. accelerateDistance
    let cruiseTime = cruiseDistance /. v1

    let accelerateCost = hpCostChanging(~v0, ~v1, ~a, ~base=param.base, ~isSpurt=false)
    let cruiseCost = hpCost(~v=v1, ~time=cruiseTime, ~base=param.base, ~isSpurt=false)

    {
      speed: (v0, v1, vTarget),
      acceleration: a,
      time: accelerateTime +. cruiseTime,
      distance: accelerateDistance +. cruiseDistance,
      cost: accelerateCost +. cruiseCost,
      accelerateTime: accelerateTime,
      accelerateDistance: accelerateDistance,
      accelerateCost: accelerateCost,
      cruiseTime: cruiseTime,
      cruiseDistance: cruiseDistance,
      cruiseCost: cruiseCost,
    }
  }
}
module StageMiddle = {
  let make = (param: Parameters.t, ~first: stage) => {
    let (_, firstV1, _) = first.speed
    let (_, _, distance) = param.race
    let floatDistance = distance->Belt.Int.toFloat
    let stageDistance = floatDistance /. 2.0

    let v0 = firstV1
    let vTarget = param->targetSpeed(RaceStage.Middle)
    let a = vTarget < v0 ? -0.8 : param->acceleration(RaceStage.Middle)
    let v1 = realReachSpeed(~v0, ~vTarget, ~a, ~distance=stageDistance)

    let accelerateTime = (v1 -. v0) /. a
    let accelerateDistance = (v0 +. v1) *. 0.5 *. accelerateTime
    let cruiseDistance = stageDistance -. accelerateDistance
    let cruiseTime = cruiseDistance /. v1

    let accelerateCost = hpCostChanging(~v0, ~v1, ~a, ~base=param.base, ~isSpurt=false)
    let cruiseCost = hpCost(~v=v1, ~time=cruiseTime, ~base=param.base, ~isSpurt=false)

    {
      speed: (v0, v1, vTarget),
      acceleration: a,
      time: accelerateTime +. cruiseTime,
      distance: accelerateDistance +. cruiseDistance,
      cost: accelerateCost +. cruiseCost,
      accelerateTime: accelerateTime,
      accelerateDistance: accelerateDistance,
      accelerateCost: accelerateCost,
      cruiseTime: cruiseTime,
      cruiseDistance: cruiseDistance,
      cruiseCost: cruiseCost,
    }
  }
}
module StageLast = {
  type parts = {
    normal: stage,
    spurt: stage,
    exhaustion: stage,
  }
  type listTemp = {
    time: float,
    result: parts,
  }

  let calcV1ForSurplusHp = (
    ~v0: float,
    ~a: float,
    ~surplusHp: float,
    ~base: Parameters.base,
  ) => {
    let {baseSpeed, hpCoef, spurtCoef} = base

    let temp1 =
      432.0 *. a *. surplusHp /. 20.0 /. hpCoef /. spurtCoef +. pow(v0 -. baseSpeed +. 12.0, 3.0)
    let temp2 = pow(temp1, 1.0 /. 3.0)
    temp2 -. 12.0 +. baseSpeed
  }
  let calcCruiseTimeForSurplusHp = (~v: float, ~surplusHp: float, ~base: Parameters.base) => {
    let {baseSpeed, hpCoef, spurtCoef} = base
    let perSecond = 20.0 *. hpCoef *. pow(v -. baseSpeed +. 12.0, 2.0) /. 144.0

    surplusHp /. (perSecond *. spurtCoef)
  }
  let calcMaxSpurtSpeed = (param: Parameters.t) => {
    let {baseSpeed} = param.base
    let speed = param.attribute->Attribute.get(Attribute.Speed)
    let (_, strategy) = param.status
    let (_, disRank, _) = param.preference

    (baseSpeed *. (StrategyFactor.forSpeed(1.0, strategy, RaceStage.Last) +. 0.01) +.
      sqrt(speed /. 500.0)->PreferenceFactor.forSpeedOfDistance(disRank)) *. 1.05 +.
      sqrt(speed /. 500.0)->PreferenceFactor.forSpeedOfDistance(disRank)
  }
  let calcSpurtDistance = (
    ~vSpurt: float,
    ~vTarget: float,
    ~surplusHp: float,
    ~stageDistance: float,
    ~base: Parameters.base,
  ) => {
    let {baseSpeed, hpCoef, spurtCoef} = base

    let f1 =
      surplusHp -.
      (stageDistance -. 60.0) *.
      20.0 *.
      hpCoef *.
      spurtCoef *.
      pow(vTarget -. baseSpeed +. 12.0, 2.0) /.
      144.0 /.
      vTarget
    let f2 =
      20.0 *.
      hpCoef *.
      spurtCoef /.
      144.0 *.
      (pow(vSpurt -. baseSpeed +. 12.0, 2.0) /. vSpurt -.
        pow(vTarget -. baseSpeed +. 12.0, 2.0) /. vTarget)
    let spurtDistance = f1 /. f2 +. 60.0

    spurtDistance
  }

  let makePart = (
    ~v0: float,
    ~vTarget: float,
    ~a: float,
    ~distance: float,
    ~surplusHp: float,
    ~base: Parameters.base,
  ) => {
    let v1 = realReachSpeed(~v0, ~vTarget, ~a, ~distance)
    let accelerateTime = (v1 -. v0) /. a
    let accelerateCost = hpCostChanging(~v0, ~v1, ~a, ~base, ~isSpurt=true)

    // surplusHp can not support full accelerate
    if accelerateCost > surplusHp {
      let realV1 = calcV1ForSurplusHp(~v0, ~a, ~surplusHp, ~base)
      let realAccelerateTime = (realV1 -. v0) /. a
      let realAccelerateDistance = (v0 +. realV1) *. realAccelerateTime *. 0.5
      {
        speed: (v0, realV1, vTarget),
        acceleration: a,
        time: realAccelerateTime,
        distance: realAccelerateDistance,
        cost: surplusHp,
        accelerateTime: realAccelerateTime,
        accelerateDistance: realAccelerateDistance,
        accelerateCost: surplusHp,
        cruiseTime: 0.0,
        cruiseDistance: 0.0,
        cruiseCost: 0.0,
      }
    } else {
      let accelerateDistance = (v0 +. v1) *. accelerateTime *. 0.5
      let hpAfterAcc = surplusHp -. accelerateCost

      let cruiseDistance = distance -. accelerateDistance
      let cruiseTime = cruiseDistance /. v1
      let cruiseCost = hpCost(~v=v1, ~time=cruiseTime, ~base, ~isSpurt=true)
      // surplusHp after accelerate can not support cruise surplus distance
      if cruiseCost > hpAfterAcc {
        let realCruiseTime = calcCruiseTimeForSurplusHp(~v=v1, ~surplusHp=hpAfterAcc, ~base)
        let realCruiseDistance = v1 *. realCruiseTime
        {
          speed: (v0, v1, vTarget),
          acceleration: a,
          time: accelerateTime +. realCruiseTime,
          distance: accelerateDistance +. realCruiseDistance,
          cost: accelerateCost +. hpAfterAcc,
          accelerateTime: accelerateTime,
          accelerateDistance: accelerateDistance,
          accelerateCost: accelerateCost,
          cruiseTime: realCruiseTime,
          cruiseDistance: realCruiseDistance,
          cruiseCost: hpAfterAcc,
        }
      } else {
        {
          speed: (v0, v1, vTarget),
          acceleration: a,
          time: accelerateTime +. cruiseTime,
          distance: accelerateDistance +. cruiseDistance,
          cost: accelerateCost +. cruiseCost,
          accelerateTime: accelerateTime,
          accelerateDistance: accelerateDistance,
          accelerateCost: accelerateCost,
          cruiseTime: cruiseTime,
          cruiseDistance: cruiseDistance,
          cruiseCost: cruiseCost,
        }
      }
    }
  }
  let makeExhaustion = (~v0: float, ~vMin: float, ~a: float, ~distance: float) => {
    let acc = v0 > vMin ? -1.2 : a
    let accDistance = (v0 +. vMin) *. 0.5 *. (vMin -. v0) /. acc
    let v1 = accDistance > distance ? sqrt(acc *. distance *. 2.0 +. v0 *. v0) : vMin
    let accTime = (v1 -. v0) /. acc
    let accDistance = (v1 +. v0) *. accTime *. 0.5
    let cruiseDistance = distance -. accDistance
    let cruiseTime = cruiseDistance /. v1

    {
      speed: (v0, v1, vMin),
      acceleration: acc,
      time: accTime +. cruiseTime,
      distance: accDistance +. cruiseDistance,
      cost: 0.0,
      accelerateTime: accTime,
      accelerateDistance: accDistance,
      accelerateCost: 0.0,
      cruiseTime: cruiseTime,
      cruiseDistance: cruiseDistance,
      cruiseCost: 0.0,
    }
  }
  let makeParts = (
    ~v0: float,
    ~vTarget: float,
    ~vSpurt: float,
    ~a: float,
    ~surplusHp: float,
    ~stageDistance: float,
    ~spurtDistance: float,
    ~base: Parameters.base,
  ) => {
    let normalPartDistance = stageDistance -. spurtDistance
    let normalPart = makePart(~v0, ~vTarget, ~a, ~distance=normalPartDistance, ~surplusHp, ~base)

    let (_, v1, _) = normalPart.speed
    let realSpurtDistance = stageDistance -. normalPart.distance
    let hpForSpurt = surplusHp -. normalPart.cost
    let spurtPart = makePart(
      ~v0=v1,
      ~vTarget=vSpurt,
      ~a,
      ~distance=realSpurtDistance,
      ~surplusHp=hpForSpurt,
      ~base,
    )

    let (_, vs, _) = spurtPart.speed
    let exhaustionDistance = realSpurtDistance -. spurtPart.distance
    let exhaustionPart = makeExhaustion(~v0=vs, ~vMin=base.vMin, ~a, ~distance=exhaustionDistance)

    {
      normal: normalPart,
      spurt: spurtPart,
      exhaustion: exhaustionPart,
    }
  }
  let makeInList = (
    ~v0: float,
    ~vTarget: float,
    ~vSpurtMax: float,
    ~a: float,
    ~surplusHp: float,
    ~stageDistance: float,
    ~param: Parameters.t,
  ) => {
    let knowledge = param.attribute->Attribute.get(Attribute.Knowledge)

    let count = ((vSpurtMax -. vTarget) /. 0.1)->Belt.Int.fromFloat
    let makePossible = vSpurt => {
      let spurtDistance = calcSpurtDistance(
        ~vSpurt,
        ~vTarget,
        ~surplusHp,
        ~stageDistance,
        ~base=param.base,
      )
      if spurtDistance <= 0.0 {
        None
      } else {
        let realSpurtDistance = spurtDistance > stageDistance ? stageDistance : spurtDistance
        let instance = makeParts(
          ~v0,
          ~vTarget,
          ~vSpurt,
          ~a,
          ~surplusHp,
          ~stageDistance,
          ~spurtDistance=realSpurtDistance,
          ~base=param.base,
        )
        let res = {
          time: instance.normal.time +. instance.spurt.time +. instance.exhaustion.time,
          result: instance,
        }
        Some(res)
      }
    }

    let possibleList = Belt.Array.range(1, count)->Belt.Array.keepMap(i => {
      let vPossible = vSpurtMax -. i->Belt.Int.toFloat *. 0.1
      makePossible(vPossible)
    })

    let sortedList = if Belt.Array.length(possibleList) == 0 {
      let worst = makeParts(
        ~v0,
        ~vTarget,
        ~vSpurt=vTarget,
        ~a,
        ~surplusHp,
        ~stageDistance,
        ~spurtDistance=0.0,
        ~base=param.base,
      )
      [
        {
          time: 0.0,
          result: worst,
        },
      ]
    } else {
      possibleList->Js.Array2.sortInPlaceWith((a, b) => a.time > b.time ? 1 : -1)
    }

    let factor = 0.15 +. 0.0005 *. knowledge
    let index =
      (Js.Array2.length(sortedList)->Belt.Int.toFloat *. (1.0 -. factor))->Belt.Int.fromFloat

    let selectedItem = if index >= Js.Array2.length(sortedList) {
      let end = Js.Array2.length(sortedList) - 1
      sortedList[end]
    } else {
      sortedList[index]
    }

    selectedItem.result
  }

  let make = (param: Parameters.t, ~middle: stage, ~surplusHp: float) => {
    let (_, middleV1, _) = middle.speed
    let (_, _, distance) = param.race
    let floatDistance = distance->Belt.Int.toFloat
    let stageDistance = floatDistance /. 3.0

    let v0 = middleV1
    let vTarget = param->targetSpeed(RaceStage.Last)
    let a = param->acceleration(RaceStage.Last)
    let vSpurtMax = calcMaxSpurtSpeed(param)
    let spurtDistanceMax = calcSpurtDistance(
      ~vSpurt=vSpurtMax,
      ~vTarget,
      ~surplusHp,
      ~stageDistance,
      ~base=param.base,
    )

    // surplusHp enough to spurt full stage
    if spurtDistanceMax >= stageDistance {
      makeParts(
        ~v0,
        ~vTarget,
        ~vSpurt=vSpurtMax,
        ~a,
        ~surplusHp,
        ~stageDistance,
        ~spurtDistance=stageDistance,
        ~base=param.base,
      )
    } else {
      makeInList(~v0, ~vTarget, ~vSpurtMax, ~a, ~surplusHp, ~stageDistance, ~param)
    }
  }
}

type raceInstance = {
  parameters: Parameters.t,
  starting: stage,
  first: stage,
  middle: stage,
  last: stage,
  spurt: stage,
  exhaustion: stage,
}
let calcRaceInstance = (options: raceOptions) => {
  let param = Parameters.make(options)

  let starting = StageStarting.make(param)
  let first = StageFirst.make(param, ~starting)
  let middle = StageMiddle.make(param, ~first)

  let surplusHp = param.base.hp -. starting.cost -. first.cost -. middle.cost
  let lastParts = StageLast.make(param, ~middle, ~surplusHp)

  {
    parameters: param,
    starting: starting,
    first: first,
    middle: middle,
    last: lastParts.normal,
    spurt: lastParts.spurt,
    exhaustion: lastParts.exhaustion,
  }
}
