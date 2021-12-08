open MaterialUi
open Uma.Variable
open PageComponents
module Rs = ReactDOMStyle
module Calculate = Uma_Calculate

let dvInt = TextField.DefaultValue.int
let ff3 = (num: float) => Js.Float.toFixedWithPrecision(num, ~digits=3)->React.string

module Styles = %makeStyles({
  header: Rs.make(~margin="2rem 0", ()),
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
  let make = (~instance: Calculate.raceInstance) => {
    let (trans, _) = I18n.useSimpleTranslation()
    let (speed, stamina, power, guts, knowledge) = instance.parameters.attribute->Attribute.toInts

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
  @react.component
  let make = (~instance: Calculate.raceInstance) => {
    let (trans, _) = I18n.useSimpleTranslation()
    let base = instance.parameters.base

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
  let make = (~instance: Calculate.raceInstance) => {
    let (trans, _) = I18n.useSimpleTranslation()
    let {spurt, exhaustion, summary} = instance

    let exTimeColor = exhaustion.time > 1.0 ? Danger : exhaustion.time > 0.1 ? Warning : Surplus
    let exDisColor =
      exhaustion.distance > 50.0 ? Danger : exhaustion.distance > 10.0 ? Warning : Surplus

    <UmaFormContainer label={"Summary"->trans}>
      <ValueDisplayer
        label={"Spurt Distance"->trans} sub={"m"->React.string} value={spurt.distance->ff3}
      />
      <ValueDisplayer label={"Time"->trans} sub={"s"->React.string} value={summary.time->ff3} />
      <ValueDisplayer
        label={"Display Time"->trans} sub={"s"->React.string} value={summary.displayTime->ff3}
      />
      <ValueDisplayer label={"Hp Remained"->trans} value={summary.surplusHp->ff3} />
      <VDGroup caption={"Exhaustion"->trans}>
        <ValueDisplayer
          label={"Time"->trans}
          sub={"s"->React.string}
          color=exTimeColor
          value={exhaustion.time->ff3}
        />
        <ValueDisplayer
          label={"Distance"->trans}
          sub={"m"->React.string}
          color=exDisColor
          value={exhaustion.distance->ff3}
        />
      </VDGroup>
    </UmaFormContainer>
  }
}
module StageDetail = {
  @react.component
  let make = (~instance: Calculate.raceInstance) => {
    let (trans, _) = I18n.useSimpleTranslation()
    let {starting, first, middle, last, spurt, exhaustion} = instance

    let stageNames = ["Starting", "First", "Middle", "Last", "Spurt", "Exhaustion"]
    let stages = [starting, first, middle, last, spurt, exhaustion]

    let sumTime = stages->Belt.Array.reduce(0.0, (p, c) => p +. c.time)
    let sumDistance = stages->Belt.Array.reduce(0.0, (p, c) => p +. c.distance)
    let sumCost = stages->Belt.Array.reduce(0.0, (p, c) => p +. c.cost)

    <Table>
      <TableHead>
        <TableRow>
          <TableCell> {"Stage"->trans} </TableCell>
          <TableCell> {"Start Speed"->trans} </TableCell>
          <TableCell> {"Target Speed"->trans} </TableCell>
          <TableCell> {"End Speed"->trans} </TableCell>
          <TableCell> {"Acceleration"->trans} </TableCell>
          <TableCell> {"Time"->trans} </TableCell>
          <TableCell> {"Distance"->trans} </TableCell>
          <TableCell> {"Hp Decrease"->trans} </TableCell>
        </TableRow>
      </TableHead>
      <TableBody>
        {stages
        ->Belt.Array.mapWithIndex((i, s) => {
          let (v0, v1, vTarget) = s.speed
          <TableRow key={stageNames[i]}>
            <TableCell> {stageNames[i]->trans} </TableCell>
            <TableCell> {v0->ff3} </TableCell>
            <TableCell> {vTarget->ff3} </TableCell>
            <TableCell> {v1->ff3} </TableCell>
            <TableCell> {s.acceleration->ff3} </TableCell>
            <TableCell> {s.time->ff3} </TableCell>
            <TableCell> {s.distance->ff3} </TableCell>
            <TableCell> {s.cost->ff3} </TableCell>
          </TableRow>
        })
        ->React.array}
        <TableRow>
          <TableCell colSpan={5}> {"Sum"->trans} </TableCell>
          <TableCell> {sumTime->ff3} </TableCell>
          <TableCell> {sumDistance->ff3} </TableCell>
          <TableCell> {sumCost->ff3} </TableCell>
        </TableRow>
      </TableBody>
    </Table>
  }
}

type raceProps = {
  attrs: Attribute.dataInt,
  preferences: Preference.data,
  status: Status.data,
  race: Race.data,
}
type patch =
  | PatchAttr(Attribute.dataInt)
  | PatchPref(Preference.data)
  | PatchStatus(Status.data)
  | PatchRace(Race.data)

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
    let options: Calculate.raceOptions = {
      race: raceState.race,
      preference: raceState.preferences,
      status: raceState.status,
      attribute: raceState.attrs,
    }
    let instance = Calculate.calcRaceInstance(options)

    setResult(_ => Some(instance))
  }

  <>
    <h2 className=classes.header> {"Input"->trans} </h2>
    <UmaAttributeForm title={"Base Attribute"->trans} attrs={raceState.attrs} dispatch={v => v->PatchAttr->dispatch} />
    <UmaPreferenceForm pref={raceState.preferences} dispatch={v => v->PatchPref->dispatch} />
    <UmaStatusForm status={raceState.status} dispatch={v => v->PatchStatus->dispatch} />
    <RaceSettingForm race={raceState.race} dispatch={v => v->PatchRace->dispatch} />
    <div> <Button onClick> {"Calculate"->trans} </Button> </div>
    {switch result {
    | Some(v) => <>
        <Divider />
        <h2 className=classes.header> {"Result"->trans} </h2>
        <AdjustedAttributes instance={v} />
        <BaseAbilities instance={v} />
        <RaceSummary instance={v} />
        <Divider />
        <h2 className=classes.header> {"Details"->trans} </h2>
        <StageDetail instance={v} />
      </>
    | None => React.null
    }}
  </>
}
