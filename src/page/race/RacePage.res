open MaterialUi
open Uma.Variable
module Rs = ReactDOMStyle

let dvInt = TextField.DefaultValue.int
let ff3 = (num: float) => Js.Float.toFixedWithPrecision(num, ~digits=3)->React.string

module Styles = %makeStyles({
  header: Rs.make(~margin="2rem 0", ()),
  formContainer: Rs.make(~display="flex", ~alignItems="center", ()),
  rowLable: Rs.make(~width="180px", ()),
  rowContent: Rs.make(~flex="1", ()),
  formItem: Rs.make(~margin="10px", ~width="110px", ()),
  displayItem: Rs.make(
    ~margin="10px 10px 18px",
    ~width="110px",
    ~flexDirection="column",
    ~position="relative",
    ~verticalAlign="top",
    ~display="inline-flex",
    ~borderWidth="1px",
    ~borderStyle="solid",
    ~borderColor=Mui_System.Colors.divider,
    ~backgroundColor=Mui_System.Colors.white,
    ~borderRadius="8px",
    (),
  ),
  displayLabel: Rs.make(
    ~position="absolute",
    ~top="-6px",
    ~right="10px",
    ~lineHeight="16px",
    ~fontSize="0.75rem",
    ~color=Mui_System.Colors.t_secondary,
    ~backgroundColor=Mui_System.Colors.white,
    ~padding="0 4px",
    (),
  ),
  displaySub: Rs.make(
    ~position="absolute",
    ~bottom="-9px",
    ~left="9px",
    ~lineHeight="16px",
    ~fontSize="0.6rem",
    ~color=Mui_System.Colors.t_secondary,
    ~backgroundColor=Mui_System.Colors.white,
    ~padding="0 6px",
    ~textAlign="center",
    (),
  ),
  displayValue: Rs.make(
    ~marginTop="16px",
    ~height="1.1876rem;",
    ~padding="5px 5px 8px 10px",
    ~color=Mui_System.Colors.t_primary,
    (),
  ),
  dpGroup: Rs.make(~position="relative", ~display="inline-flex", ()),
  dpGroupTitle: Rs.make(
    ~position="absolute",
    ~fontSize="0.75rem",
    ~top="-14px",
    ~left="5px",
    ~padding="1px 6px",
    ~color=Mui_System.Colors.primary,
    ~backgroundColor=Mui_System.Colors.white,
    (),
  ),
  dpGroupLine: Rs.make(
    ~position="absolute",
    ~top="-3px",
    ~left="12px",
    ~right="27px",
    ~height="38px",
    ~borderStyle="solid",
    ~borderColor=Mui_System.Colors.divider,
    ~borderWidth="1px 1px 1px 0",
    (),
  ),
})

module UmaEnumSelector = {
  @react.component
  let make = (
    ~id: string,
    ~label: React.element,
    ~value: 'a,
    ~onChange: 'a => unit,
    ~enum: enumTuple<'a>,
  ) => {
    let (items, toString, toEnum) = enum
    let classes = Styles.useStyles()
    let wrapOnChange = (evt: ReactEvent.Form.t, _) => {
      ReactEvent.Form.target(evt)["value"]->toEnum->onChange
    }
    let selectVal = value->toString->Select.Value.string

    <FormControl className=classes.formItem>
      <InputLabel htmlFor="" id> {label} </InputLabel>
      <Select labelId=id value=selectVal onChange=wrapOnChange>
        {items
        ->Belt.List.map(val => {
          let strVal = val->toString
          <MenuItem value={strVal->MenuItem.Value.string}> {strVal->React.string} </MenuItem>
        })
        ->Belt.List.toArray}
      </Select>
    </FormControl>
  }
}

module UmaFormContainer = {
  @react.component
  let make = (~label: React.element, ~children: 'children) => {
    let classes = Styles.useStyles()

    <div className=classes.formContainer>
      <div className=classes.rowLable> {label} </div>
      <div className=classes.rowContent> {children} </div>
    </div>
  }
}

type raceProps = {
  attrs: Attribute.data,
  preferences: Preference.data,
  status: Status.data,
  race: Race.data,
}

type patch =
  | PatchAttr(Attribute.data)
  | PatchPref(Preference.data)
  | PatchStatus(Status.data)
  | PatchRace(Race.data)

module UmaAttributeForm = {
  @react.component
  let make = (~attrs: Attribute.data, ~dispatch: patch => unit) => {
    let classes = Styles.useStyles()
    let (trans, _) = I18n.useSimpleTranslation()

    let handleChange = (kind, evt: ReactEvent.Form.t) => {
      let val = ReactEvent.Form.target(evt)["value"]->Belt.Int.fromString
      switch val {
      | Some(v) => attrs->Attribute.update(kind, v)->PatchAttr->dispatch
      | None => ()
      }
    }

    let (speed, stamina, power, guts, knowledge) = attrs
    <UmaFormContainer label={"Base Attributes"->trans}>
      <TextField
        required={true}
        className=classes.formItem
        label={"Speed"->trans}
        defaultValue={speed->dvInt}
        onChange={Attribute.Speed->handleChange}
      />
      <TextField
        required={true}
        className=classes.formItem
        label={"Stamina"->trans}
        defaultValue={stamina->dvInt}
        onChange={Attribute.Stamina->handleChange}
      />
      <TextField
        required={true}
        className=classes.formItem
        label={"Power"->trans}
        defaultValue={power->dvInt}
        onChange={Attribute.Power->handleChange}
      />
      <TextField
        required={true}
        className=classes.formItem
        label={"Guts"->trans}
        defaultValue={guts->dvInt}
        onChange={Attribute.Guts->handleChange}
      />
      <TextField
        required={true}
        className=classes.formItem
        label={"Knowledge"->trans}
        defaultValue={knowledge->dvInt}
        onChange={Attribute.Knowledge->handleChange}
      />
    </UmaFormContainer>
  }
}
module UmaPreferenceForm = {
  @react.component
  let make = (~pref: Preference.data, ~dispatch: patch => unit) => {
    let (trans, _) = I18n.useSimpleTranslation()

    let (field, distance, strategy) = pref
    let handleChange = (kind, value) => {
      pref->Preference.update(kind, value)->PatchPref->dispatch
    }

    <UmaFormContainer label={"Preference"->trans}>
      <UmaEnumSelector
        id="pref-field"
        label={"Field"->trans}
        value={field}
        onChange={Preference.Field->handleChange}
        enum=enumRank
      />
      <UmaEnumSelector
        id="pref-distance"
        label={"Distance"->trans}
        value={distance}
        onChange={Preference.Distance->handleChange}
        enum=enumRank
      />
      <UmaEnumSelector
        id="pref-strategy"
        label={"Strategy"->trans}
        value={strategy}
        onChange={Preference.Strategy->handleChange}
        enum=enumRank
      />
    </UmaFormContainer>
  }
}
module UmaStatusForm = {
  @react.component
  let make = (~status: Status.data, ~dispatch: patch => unit) => {
    let (trans, _) = I18n.useSimpleTranslation()
    let (mood, strategy) = status

    let statusChange = (val: Status.kind) => {
      status->Status.update(val)->PatchStatus->dispatch
    }

    <UmaFormContainer label={"Umamusume Status"->trans}>
      <UmaEnumSelector
        id="status-mood"
        label={"Mood"->trans}
        value=mood
        onChange={v => v->Status.Mood->statusChange}
        enum=enumMood
      />
      <UmaEnumSelector
        id="status-strategy"
        label={"Strategy"->trans}
        value=strategy
        onChange={v => v->Status.Strategy->statusChange}
        enum=enumStrategy
      />
    </UmaFormContainer>
  }
}
module RaceForm = {
  @react.component
  let make = (~race: Race.data, ~dispatch: patch => unit) => {
    let classes = Styles.useStyles()
    let (trans, _) = I18n.useSimpleTranslation()
    let (field, fstatus, length) = race

    let fieldChange = val => {
      race->Race.update(Race.Field(val))->PatchRace->dispatch
    }
    let fstatusChange = val => {
      race->Race.update(Race.FStatus(val))->PatchRace->dispatch
    }
    let distanceChange = (evt: ReactEvent.Form.t) => {
      let newVal = ReactEvent.Form.target(evt)["value"]->Belt.Int.fromString
      switch newVal {
      | Some(v) => race->Race.update(Race.Length(v))->PatchRace->dispatch
      | None => ()
      }
    }

    <UmaFormContainer label={"Race Settings"->trans}>
      <UmaEnumSelector
        id="race-field" label={"Field"->trans} value=field onChange=fieldChange enum=enumField
      />
      <UmaEnumSelector
        id="race-fstatus"
        label={"Field Status"->trans}
        value=fstatus
        onChange=fstatusChange
        enum=enumFieldStatus
      />
      <TextField
        className=classes.formItem
        id="race-distance"
        label={"Distance"->trans}
        defaultValue={length->dvInt}
        helperText={length->Distance.fromLength->Distance.toString->React.string}
        onChange=distanceChange
      />
    </UmaFormContainer>
  }
}

type valueColor = Normal | Surplus | Warning | Danger
module ValueDisplayer = {
  @react.component
  let make = (
    ~label: React.element,
    ~value: React.element,
    ~sub: option<React.element>=?,
    ~color=Normal,
  ) => {
    let classes = Styles.useStyles()

    let cstr = switch color {
    | Normal => Mui_System.Colors.divider
    | Surplus => Mui_System.Colors.success
    | Warning => Mui_System.Colors.warning
    | Danger => Mui_System.Colors.error
    }
    let style = Rs.make(~borderColor=cstr, ())

    <div className=classes.displayItem style>
      <label className=classes.displayLabel> {label} </label>
      <div className=classes.displayValue> {value} </div>
      {switch sub {
      | Some(s) => <span className=classes.displaySub> {s} </span>
      | None => React.null
      }}
    </div>
  }
}
module VDGroup = {
  @react.component
  let make = (~caption: React.element, ~children: 'children) => {
    let classes = Styles.useStyles()
    <div className=classes.dpGroup>
      <span className=classes.dpGroupLine />
      {children}
      <span className=classes.dpGroupTitle> {caption} </span>
    </div>
  }
}

module AdjustedAttributes = {
  @react.component
  let make = (~adjAttrs: Attribute.data) => {
    let (trans, _) = I18n.useSimpleTranslation()
    let (speed, stamina, power, guts, knowledge) = adjAttrs

    <UmaFormContainer label={"Adjusted Attributes"->trans}>
      <ValueDisplayer label={"Speed"->trans} value={speed->React.int} />
      <ValueDisplayer label={"Stamina"->trans} value={stamina->React.int} />
      <ValueDisplayer label={"Power"->trans} value={power->React.int} />
      <ValueDisplayer label={"Guts"->trans} value={guts->React.int} />
      <ValueDisplayer label={"Knowledge"->trans} value={knowledge->React.int} />
    </UmaFormContainer>
  }
}
module BaseAbilities = {
  module BP = Uma_Calculate.BaseParameter

  @react.component
  let make = (~base: BP.t) => {
    let (trans, _) = I18n.useSimpleTranslation()

    <UmaFormContainer label={"Base Ability"->trans}>
      <ValueDisplayer
        label={"Base Speed"->trans} sub={"m/s"->React.string} value={base.baseSpeed->ff3}
      />
      <VDGroup caption={"Hp"->trans}>
        <ValueDisplayer label={"Base"->trans} value={base.hp->ff3} />
        <ValueDisplayer label={"With Skill"->trans} value={0.0->ff3} />
      </VDGroup>
      <VDGroup caption={"Hp Consumption Coef"->trans}>
        <ValueDisplayer label={"Usually"->trans} value={base.hpCoef->ff3} />
        <ValueDisplayer label={"Spurt"->trans} value={base.spurtCoef->ff3} />
      </VDGroup>
    </UmaFormContainer>
  }
}
module RaceSummary = {
  @react.component
  let make = () => {
    let (trans, _) = I18n.useSimpleTranslation()

    <UmaFormContainer label={"Summary"->trans}>
      <ValueDisplayer
        label={"Spurt Distance"->trans} sub={"m"->React.string} value={333.33->ff3}
      />
      <ValueDisplayer label={"Time"->trans} value={"1:13.44"->React.string} />
      <ValueDisplayer label={"Display Time"->trans} value={"2:27.41"->React.string} />
      <ValueDisplayer label={"Hp Remained"->trans} value={450.5->ff3} />
      <VDGroup caption={"Exhaustion"->trans}>
        <ValueDisplayer
          label={"Time"->trans} sub={"s"->React.string} color=Danger value={"1.56"->React.string}
        />
        <ValueDisplayer
          label={"Distance"->trans}
          sub={"m"->React.string}
          color=Warning
          value={"45.00"->React.string}
        />
      </VDGroup>
    </UmaFormContainer>
  }
}

type raceStage = {
  stage: string,
  spStart: float,
  spTarget: float,
  acceleration: float,
  time: float,
  distance: float,
  hpConsumption: float,
}
module RaceDetail = {
  @react.component
  let make = (~result: Uma_Calculate.raceResult) => {
    let (trans, _) = I18n.useSimpleTranslation()
    let { starting, first, middle, final, spurt, exhaustion } = result

    let (v0, v1) = starting.speed
    let (v2, v3, v4) = first.speed
    let (v5, v6) = middle.speed
    let (v7, v8, v9) = final.speed
    let (vs1, vs2, vs3) = spurt.speed
    let (ve1, ve2, ve3) = exhaustion.speed

    let sum_time = starting.time +. first.time +. middle.time +. final.time +. spurt.time +. exhaustion.time
    let sum_dis = starting.distance +. first.distance +. middle.distance +. final.distance +. spurt.distance +. exhaustion.distance
    let sum_hp = starting.hp_cost +. first.hp_cost +. middle.hp_cost +. final.hp_cost +. spurt.hp_cost +. exhaustion.hp_cost
    <>
      <UmaFormContainer label={"Stage Starting"->trans}>
        <VDGroup caption={"Speed"->trans}>
          <ValueDisplayer label={"Start"->trans} value={v0->ff3} />
          <ValueDisplayer label={"Target"->trans} value={v1->ff3} />
          <ValueDisplayer label={"End"->trans} value={v1->ff3} />
        </VDGroup>
        <ValueDisplayer label={"Acceleration"->trans} value={starting.acceleration->ff3} />
        <ValueDisplayer label={"Time"->trans} value={starting.time->ff3} />
        <ValueDisplayer label={"Distance"->trans} value={starting.distance->ff3} />
        <ValueDisplayer label={"HP Cost"->trans} value={starting.hp_cost->ff3} />
      </UmaFormContainer>
      <UmaFormContainer label={"Stage First"->trans}>
        <VDGroup caption={"Speed"->trans}>
          <ValueDisplayer label={"Start"->trans} value={v2->ff3} />
          <ValueDisplayer label={"Target"->trans} value={v3->ff3} />
          <ValueDisplayer label={"End"->trans} value={v4->ff3} />
        </VDGroup>
        <ValueDisplayer label={"Acceleration"->trans} value={first.acceleration->ff3} />
        <ValueDisplayer label={"Time"->trans} value={first.time->ff3} />
        <ValueDisplayer label={"Distance"->trans} value={first.distance->ff3} />
        <ValueDisplayer label={"HP Cost"->trans} value={first.hp_cost->ff3} />
      </UmaFormContainer>
      <UmaFormContainer label={"Stage Middle"->trans}>
        <VDGroup caption={"Speed"->trans}>
          <ValueDisplayer label={"Start"->trans} value={v5->ff3} />
          <ValueDisplayer label={"Target"->trans} value={v6->ff3} />
          <ValueDisplayer label={"End"->trans} value={v6->ff3} />
        </VDGroup>
        <ValueDisplayer label={"Acceleration"->trans} value={middle.acceleration->ff3} />
        <ValueDisplayer label={"Time"->trans} value={middle.time->ff3} />
        <ValueDisplayer label={"Distance"->trans} value={middle.distance->ff3} />
        <ValueDisplayer label={"HP Cost"->trans} value={middle.hp_cost->ff3} />
      </UmaFormContainer>
      <UmaFormContainer label={"Stage Last"->trans}>
        <VDGroup caption={"Speed"->trans}>
          <ValueDisplayer label={"Start"->trans} value={v7->ff3} />
          <ValueDisplayer label={"Target"->trans} value={v8->ff3} />
          <ValueDisplayer label={"End"->trans} value={v9->ff3} />
        </VDGroup>
        <ValueDisplayer label={"Acceleration"->trans} value={final.acceleration->ff3} />
        <ValueDisplayer label={"Time"->trans} value={final.time->ff3} />
        <ValueDisplayer label={"Distance"->trans} value={final.distance->ff3} />
        <ValueDisplayer label={"HP Cost"->trans} value={final.hp_cost->ff3} />
      </UmaFormContainer>
      <UmaFormContainer label={"Stage Spurt"->trans}>
        <VDGroup caption={"Speed"->trans}>
          <ValueDisplayer label={"Start"->trans} value={vs1->ff3} />
          <ValueDisplayer label={"Target"->trans} value={vs2->ff3} />
          <ValueDisplayer label={"End"->trans} value={vs3->ff3} />
        </VDGroup>
        <ValueDisplayer label={"Acceleration"->trans} value={spurt.acceleration->ff3} />
        <ValueDisplayer label={"Time"->trans} value={spurt.time->ff3} />
        <ValueDisplayer label={"Distance"->trans} value={spurt.distance->ff3} />
        <ValueDisplayer label={"HP Cost"->trans} value={spurt.hp_cost->ff3} />
      </UmaFormContainer>
      <UmaFormContainer label={"Stage Exhaustion"->trans}>
        <VDGroup caption={"Speed"->trans}>
          <ValueDisplayer label={"Start"->trans} value={ve1->ff3} />
          <ValueDisplayer label={"Target"->trans} value={ve2->ff3} />
          <ValueDisplayer label={"End"->trans} value={ve3->ff3} />
        </VDGroup>
        <ValueDisplayer label={"Acceleration"->trans} value={exhaustion.acceleration->ff3} />
        <ValueDisplayer label={"Time"->trans} value={exhaustion.time->ff3} />
        <ValueDisplayer label={"Distance"->trans} value={exhaustion.distance->ff3} />
        <ValueDisplayer label={"HP Cost"->trans} value={exhaustion.hp_cost->ff3} />
      </UmaFormContainer>

      <UmaFormContainer label={"Sum"->trans}>
        <ValueDisplayer label={"Time"->trans} value={sum_time->ff3} />
        <ValueDisplayer label={"Distance"->trans} value={sum_dis->ff3} />
        <ValueDisplayer label={"HP Cost"->trans} value={sum_hp->ff3} />
      </UmaFormContainer>
    </>
  }
}

let toAdjAttrs = (r: raceProps) => {
  let attrCalc = Uma_Calculate.calcOf(r.preferences, r.status, r.race)
  r.attrs->Uma_Calculate.adjustAttrs(attrCalc)
}

@react.component
let make = () => {
  let (trans, _) = I18n.useSimpleTranslation()
  let classes = Styles.useStyles()

  let (raceState, dispatch) = React.useReducer(
    (prev, patch) => {
      switch patch {
      | PatchAttr(attrs) => {...prev, attrs: attrs}
      | PatchPref(preferences) => {...prev, preferences: preferences}
      | PatchStatus(status) => {...prev, status: status}
      | PatchRace(race) => {...prev, race: race}
      }
    },
    {
      attrs: (600, 600, 600, 600, 600),
      preferences: (Rank.A, Rank.A, Rank.A),
      status: (Mood.Good, Strategy.Leading),
      race: (Field.Turf, FieldStatus.Good, 1600),
    },
  )
  let (result, setResult) = React.useState(_ => None)
  let onClick = _ => {
    let res = Uma_Calculate.clac_race(
      ~attrs=raceState.attrs,
      ~preferences=raceState.preferences,
      ~status=raceState.status,
      ~race=raceState.race,
    )
    setResult(_ => Some(res))
  }

  <>
    <h2 className=classes.header> {"Input"->trans} </h2>
    <UmaAttributeForm attrs={raceState.attrs} dispatch />
    <UmaPreferenceForm pref={raceState.preferences} dispatch />
    <UmaStatusForm status={raceState.status} dispatch />
    <RaceForm race={raceState.race} dispatch />
    <div> <Button onClick> {"Calculate"->trans} </Button> </div>
    {
      switch result {
        | Some(v) => {
          <>
            <Divider />
            <h2 className=classes.header> {"Result"->trans} </h2>
            <AdjustedAttributes adjAttrs={v.attrs} />
            <BaseAbilities base={v.base} />
            <RaceSummary />
            <Divider />
            <h2 className=classes.header> {"Details"->trans} </h2>
            <RaceDetail result={v} />
          </>
        }
        | None => React.null
      }
    }
  </>
}
