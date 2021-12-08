open MaterialUi
open Uma.Variable
open PageComponents
module StatsSim = Uma_StatsSim

let dvInt = TextField.DefaultValue.int
let ff3 = (num: float) => Js.Float.toFixedWithPrecision(num, ~digits=3)->React.string

module Styles = %makeStyles({
  header: Rs.make(~margin="2rem 0", ()),
  formItem: Rs.make(~margin="10px", ~width="110px", ()),
})

module TotalValueForm = {
  @react.component
  let make = (~total: int, ~dispatch: int => unit) => {
    let (trans, _) = I18n.useSimpleTranslation()
    let classes = Styles.useStyles()
    
    let onChange = (evt: ReactEvent.Form.t) => {
      let newVal = ReactEvent.Form.target(evt)["value"]->Belt.Int.fromString
      switch newVal {
      | Some(v) => v->dispatch
      | None => ()
      }
    }

    <>
      <UmaFormContainer label={"Total Attribute Values"->trans}>
        <TextField
          required={true}
          className=classes.formItem
          label={"Sum"->trans}
          defaultValue={total->dvInt}
          onChange=onChange
        />
      </UmaFormContainer>
    </>
  }
}
module SimResultView = {
  let attrString = (attrs: Attribute.dataInt) => {
    let (a,b,c,d,e) = attrs
    [a,b,c,d,e]
      ->Js.Array2.map(s => s->Js.Int.toString)
      ->Js.Array2.joinWith(",")
      ->React.string
  }

  @react.component
  let make = (~result: StatsSim.simResult) => {
    let (trans, _) = I18n.useSimpleTranslation()
    let resultArr = result.result

    <Table>
      <TableHead>
        <TableRow>
          <TableCell> {"Attributes"->trans} </TableCell>
          <TableCell> {"Time"->trans} </TableCell>
          <TableCell> {"Spurt Distance"->trans} </TableCell>
          <TableCell> {"Hp Remained"->trans} </TableCell>
          <TableCell> {"Exhaustion Time"->trans} </TableCell>
          <TableCell> {"Exhaustion Distance"->trans} </TableCell>
        </TableRow>
      </TableHead>
      <TableBody>
        {resultArr
        ->Belt.Array.mapWithIndex((i, s) => {
          let {spurt, exhaustion, summary, parameters} = s
          <TableRow key={i->Belt.Int.toString}>
            <TableCell> {parameters.originAttribute->attrString} </TableCell>
            <TableCell> {summary.time->ff3} </TableCell>
            <TableCell> {spurt.distance->ff3} </TableCell>
            <TableCell> {summary.surplusHp->ff3} </TableCell>
            <TableCell> {exhaustion.time->ff3} </TableCell>
            <TableCell> {exhaustion.distance->ff3} </TableCell>
          </TableRow>
        })
        ->React.array}
        <TableRow>
          <TableCell colSpan={6}> <b>{"The fastest top 10 of "->trans}{result.count->React.int}</b> </TableCell>
        </TableRow>
      </TableBody>
    </Table>
  }
}

type statsProps = {
  total: int,
  minValues: Attribute.dataInt,
  maxValues: Attribute.dataInt,
  preferences: Preference.data,
  status: Status.data,
  race: Race.data,
}
type patch =
  | PatchTotal(int)
  | PatchMin(Attribute.dataInt)
  | PatchMax(Attribute.dataInt)
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
      | PatchTotal(total) => {...prev, total: total}
      | PatchMin(attrs) => {...prev, minValues: attrs}
      | PatchMax(attrs) => {...prev, maxValues: attrs}
      | PatchPref(preferences) => {...prev, preferences: preferences}
      | PatchStatus(status) => {...prev, status: status}
      | PatchRace(race) => {...prev, race: race}
      }
    },
    {
      total: 4000,
      minValues: (900, 600, 300, 300, 300),
      maxValues: (1200, 1200, 1200, 900, 900),
      preferences: (Rank.A, Rank.A, Rank.A),
      status: (Mood.Good, Strategy.Leading),
      race: (Field.Turf, FieldStatus.Good, 1600),
    },
  )
  let (result, setResult) = React.useState(_ => None)
  let onClick = (_) => {
    let statOption: StatsSim.statsOptions = {
      total: raceState.total,
      step: 50,
      minValues: raceState.minValues,
      maxValues: raceState.maxValues,
      preferences: raceState.preferences,
      status: raceState.status,
      race: raceState.race,
    }
    let result = StatsSim.getStatsSimList(statOption)

    setResult(_ => Some(result))
  }

  <>
    <h2 className=classes.header> {"Settings"->trans} </h2>
    <TotalValueForm total={raceState.total} dispatch={v => v->PatchTotal->dispatch} />
    <UmaAttributeForm title={"Min Attributes"->trans} attrs={raceState.minValues} dispatch={v => v->PatchMin->dispatch} />
    <UmaAttributeForm title={"Max Attributes"->trans} attrs={raceState.maxValues} dispatch={v => v->PatchMax->dispatch} />
    <UmaPreferenceForm pref={raceState.preferences} dispatch={v => v->PatchPref->dispatch} />
    <UmaStatusForm status={raceState.status} dispatch={v => v->PatchStatus->dispatch} />
    <RaceSettingForm race={raceState.race} dispatch={v => v->PatchRace->dispatch} />
    <hr/>
    <div> <Button onClick> {"Calculate"->trans} </Button> </div>
    {switch result {
      | Some(v) => <>
          <Divider />
          <h2 className=classes.header> {"Result"->trans} </h2>
          <SimResultView result={v} />
        </>
      | None => React.null
    }}
  </>
}