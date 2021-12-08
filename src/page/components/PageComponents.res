open MaterialUi
open Uma.Variable
module Rs = ReactDOMStyle

let dvInt = TextField.DefaultValue.int

module Styles = %makeStyles({
  formItem: Rs.make(~margin="10px", ~width="110px", ()),
  formContainer: Rs.make(~display="flex", ~alignItems="center", ()),
  rowLable: Rs.make(~width="180px", ()),
  rowContent: Rs.make(~flex="1", ()),
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

module UmaAttributeForm = {
  @react.component
  let make = (~attrs: Attribute.dataInt, ~dispatch: Attribute.dataInt => unit, ~title: React.element) => {
    let classes = Styles.useStyles()
    let (trans, _) = I18n.useSimpleTranslation()

    let handleChange = (kind, evt: ReactEvent.Form.t) => {
      let val = ReactEvent.Form.target(evt)["value"]->Belt.Int.fromString
      switch val {
      | Some(v) => attrs->Attribute.update(kind, v)->dispatch
      | None => ()
      }
    }

    let (speed, stamina, power, guts, knowledge) = attrs
    <UmaFormContainer label={title}>
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
  let make = (~pref: Preference.data, ~dispatch: Preference.data => unit) => {
    let (trans, _) = I18n.useSimpleTranslation()

    let (field, distance, strategy) = pref
    let handleChange = (kind, value) => {
      pref->Preference.update(kind, value)->dispatch
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
  let make = (~status: Status.data, ~dispatch: Status.data => unit) => {
    let (trans, _) = I18n.useSimpleTranslation()
    let (mood, strategy) = status

    let statusChange = (val: Status.kind) => {
      status->Status.update(val)->dispatch
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

module RaceSettingForm = {
  @react.component
  let make = (~race: Race.data, ~dispatch: Race.data => unit) => {
    let classes = Styles.useStyles()
    let (trans, _) = I18n.useSimpleTranslation()
    let (field, fstatus, length) = race

    let fieldChange = val => {
      race->Race.update(Race.Field(val))->dispatch
    }
    let fstatusChange = val => {
      race->Race.update(Race.FStatus(val))->dispatch
    }
    let distanceChange = (evt: ReactEvent.Form.t) => {
      let newVal = ReactEvent.Form.target(evt)["value"]->Belt.Int.fromString
      switch newVal {
      | Some(v) => race->Race.update(Race.Length(v))->dispatch
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