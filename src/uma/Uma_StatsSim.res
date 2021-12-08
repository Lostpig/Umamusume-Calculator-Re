open Uma_Variable
open Uma_Calculate

type statsOptions = {
  total: int,
  step: int,
  minValues: Attribute.dataInt,
  maxValues: Attribute.dataInt,
  preferences: Preference.data,
  status: Status.data,
  race: Race.data,
}

module SortedList = {
  type rec node<'a> = {
    value: 'a,
    mutable prev: option<node<'a>>,
    mutable next: option<node<'a>>
  }
  type t<'a> = {
    head: node<'a>,
    tail: node<'a>,
    length: int,
    maxLength: int
  }
  type compareRes = Greater | Less
  type compare<'a> = ('a, 'a) => compareRes

  let makeNode = (val, prev: option<node<'a>>, next: option<node<'a>>) => {
    {
      value: val,
      prev: prev,
      next: next
    }
  }
  let make = (val: 'a, maxLength: int) => {
    let initNode = makeNode(val, None, None)
    {
      head: initNode,
      tail: initNode,
      length: 1,
      maxLength: maxLength
    }
  }

  let add = (items: t<'a>, val: 'a, compare) => {
    let rec addNode = (node, prev, val) => {
      let c = compare(val, node.value)
      switch (c, node.next) {
        | (Greater, _) => 
          let newNode = makeNode(val, prev, Some(node))
          node.prev = Some(newNode)
          newNode
        | (Less, Some(n)) =>
          node.next = Some(addNode(n, Some(node), val))
          node
        | (Less, None) =>
          node.next = Some(makeNode(val, Some(node), None))
          node
      }
    }
    let rec reTail = (node) => {
      switch (node.next) {
        | Some(n) => reTail(n)
        | _ => node
      }
    }

    let newHead = addNode(items.head, None, val)
    let (newTail, length) = {
      let tail = reTail(items.tail)
      switch (tail.prev, items.length === items.maxLength) {
        | (Some(n), true) =>
          n.next = None
          (n, items.length)
        | _ => (tail, items.length + 1)
      }
    }

    {
      head: newHead,
      tail: newTail,
      length: length,
      maxLength: items.maxLength
    }
  }
  let each = (items: t<'a>, func: ('a) => unit) => {
    let head = items.head
    let rec eachNode = (node) => {
      func(node.value)
      switch (node.next) {
        | Some(n) => eachNode(n)
        | None => ignore()
      }
    }
    eachNode(head)
  }
  let toArray = (items: t<'a>) => {
    let arr = []
    items->each(val => arr->Js.Array2.push(val)->ignore)
    arr
  }
}

type countRecord = {
  mutable count: int,
  mutable list: SortedList.t<raceInstance>
}
type simResult = {
  count: int,
  result: array<raceInstance>
}
let raceCompare = (a, b) => {
  if (a.summary.time > b.summary.time) {
    SortedList.Less
  } else {
    SortedList.Greater
  }
}

let eachStatsSimList = (minVals, maxVals, total, step, caller) => {
  let (minSpeed, minStamina, minPower, minGuts, minKnowledge) = minVals
  let (maxSpeed, maxStamina, maxPower, maxGuts, maxKnowledge) = maxVals

  let minArr = [minSpeed, minStamina, minPower, minGuts, minKnowledge]
  let maxArr = [maxSpeed, maxStamina, maxPower, maxGuts, maxKnowledge]

  let arrLen = Js.Array.length(minArr)
  let useableMinArr = Belt.Array.make(arrLen, 0)
  let useableMaxArr = Belt.Array.make(arrLen, 0)
  for n in 0 to (arrLen - 1) {
    useableMinArr[n] = maxArr->Js.Array2.reducei((p,c,i) => p + if(i > n) { c } else { 0 }, 0)
    useableMaxArr[n] = minArr->Js.Array2.reducei((p,c,i) => p + if(i > n) { c } else { 0 }, 0)
  }

  let rec creator = (used: array<int>, usedSum: int, index: int) => {
    let minVal = minArr[index]
    let maxVal = maxArr[index]

    if (index === 4) {
      used[index] = total - usedSum
      caller(used)
    } else {
      let useableMin = total - usedSum - useableMinArr[index]
      let useableMax = total - usedSum - useableMaxArr[index]
      let start = Js.Math.max_int(minVal, useableMin)
      let end = Js.Math.min_int(maxVal, useableMax)
      let c = (end - start) / step

      for i in 0 to c {
        used[index] = start + i * step
        creator(used, usedSum + used[index], index + 1)
      }
    }
  }

  creator([0,0,0,0,0], 0, 0)
}

let getStatsSimList = (option: statsOptions) => {
  let initRaceOpt: raceOptions = {
    attribute: option.minValues,
    preference: option.preferences,
    status: option.status,
    race: option.race
  }
  let initRaceInst = calcRaceInstance(initRaceOpt)
  let record: countRecord = {
    count: 0,
    list: SortedList.make(initRaceInst, 10)
  }

  let caller = (used: array<int>) => {
    let raceOpt: raceOptions = {
      attribute: (used[0], used[1], used[2], used[3], used[4]),
      preference: option.preferences,
      status: option.status,
      race: option.race
    }
    let inst = calcRaceInstance(raceOpt)

    record.list = record.list->SortedList.add(inst, raceCompare)
    record.count = record.count + 1
  }
  eachStatsSimList(option.minValues, option.maxValues, option.total, option.step, caller)

  {
    count: record.count,
    result: record.list->SortedList.toArray
  }
}
