open MaterialUi
open Variable
module Rs = ReactDOMStyle

let dvInt = TextField.DefaultValue.int

let itemStyle = Rs.make(~margin="10px", ~width="110px", ())
module Styles = %makeStyles({
  formContainer: Rs.make(~display="flex", ~alignItems="center", ()),
  rowLable: Rs.make(~width="180px", ()),
  rowContent: Rs.make(~flex="1", ()),
})

type umaAttr = Speed | Stamina | Power | Guts | Knowledge
type umaAttrs = (int, int, int, int, int)
module UmaAttrInputer = {
  @react.component
  let make = (~label, ~defaultValue, ~onChange) => {
    <TextField required={true} style=itemStyle label defaultValue onChange />
  }
}
module UmaAttributeForm = {
  @react.component
  let make = (~attrs: umaAttrs) => {
    let (trans, _) = I18n.useSimpleTranslation()
    let classes = Styles.useStyles()
    let (tuple, setTuple) = React.useState(_ => attrs)

    let updateAttr = (umaAttr, val: int) => {
      setTuple(prev => {
        let (a1, a2, a3, a4, a5) = prev
        switch umaAttr {
        | Speed => (val, a2, a3, a4, a5)
        | Stamina => (a1, val, a3, a4, a5)
        | Power => (a1, a2, val, a4, a5)
        | Guts => (a1, a2, a3, val, a5)
        | Knowledge => (a1, a2, a3, a4, val)
        }
      })
    }
    let handleChange = (umaAttr, evt: ReactEvent.Form.t) => {
      let newVal = ReactEvent.Form.target(evt)["value"]->Belt.Int.fromString
      switch newVal {
      | Some(v) =>
        switch umaAttr {
        | Speed => Speed->updateAttr(v)
        | Stamina => Stamina->updateAttr(v)
        | Power => Power->updateAttr(v)
        | Guts => Guts->updateAttr(v)
        | Knowledge => Knowledge->updateAttr(v)
        }
      | None => ()
      }
    }

    let (speed, stamina, power, guts, knowledge) = tuple

    <div className=classes.formContainer>
      <div className=classes.rowLable> {"Base Attributes"->trans} </div>
      <div className=classes.rowContent>
        <UmaAttrInputer
          label={"Speed"->trans} defaultValue={speed->dvInt} onChange={Speed->handleChange}
        />
        <UmaAttrInputer
          label={"Stamina"->trans} defaultValue={stamina->dvInt} onChange={Stamina->handleChange}
        />
        <UmaAttrInputer
          label={"Power"->trans} defaultValue={power->dvInt} onChange={Power->handleChange}
        />
        <UmaAttrInputer
          label={"Guts"->trans} defaultValue={guts->dvInt} onChange={Guts->handleChange}
        />
        <UmaAttrInputer
          label={"Knowledge"->trans}
          defaultValue={knowledge->dvInt}
          onChange={Knowledge->handleChange}
        />
      </div>
    </div>
  }
}

type umaPreference = Field | Distance | Strategy
type umaPreferences = (Rank.t, Rank.t, Rank.t)
module UmaPreferenceSelector = {
  @react.component
  let make = (~id: string, ~label: React.element, ~value: Rank.t, ~onChange: Rank.t => unit) => {
    let wrapOnChange = (evt: ReactEvent.Form.t, _) => {
      let val = ReactEvent.Form.target(evt)["value"]
      let str = switch val {
      | Some(v) => v
      | None => "G"
      }
      str->Rank.convert->onChange
    }
    let strVal = value->Rank.string->Select.Value.string

    <FormControl style=itemStyle>
      <InputLabel htmlFor="" id={id}> {label} </InputLabel>
      <Select labelId={id} value=strVal onChange=wrapOnChange>
        {Rank.list
        ->Belt.List.map(val => {
          let str = Rank.string(val)
          <MenuItem value={str->MenuItem.Value.string}> {str->React.string} </MenuItem>
        })
        ->Belt.List.toArray}
      </Select>
    </FormControl>
  }
}
module UmaPreferenceForm = {
  @react.component
  let make = (~pref: umaPreferences) => {
    let classes = Styles.useStyles()
    let (trans, _) = I18n.useSimpleTranslation()
    let (tuple, setTuple) = React.useState(_ => pref)
    let handleChange = (umaPreference, rank) => {
      setTuple(prev => {
        let (p1, p2, p3) = prev
        switch umaPreference {
        | Field => (rank, p2, p3)
        | Distance => (p1, rank, p3)
        | Strategy => (p1, p2, rank)
        }
      })
    }

    let (field, distance, strategy) = tuple
    <div className=classes.formContainer>
      <div className=classes.rowLable> {"Preference"->trans} </div>
      <div className=classes.rowContent>
        <UmaPreferenceSelector
          id="pref-field" label={"Field"->trans} value={field} onChange={Field->handleChange}
        />
        <UmaPreferenceSelector
          id="pref-distance"
          label={"Distance"->trans}
          value={distance}
          onChange={Distance->handleChange}
        />
        <UmaPreferenceSelector
          id="pref-strategy"
          label={"Strategy"->trans}
          value={strategy}
          onChange={Strategy->handleChange}
        />
      </div>
    </div>
  }
}

type umaStatus = Mood | Strategy
type statusTuple = (Mood.t, Strategy.t)
module UmaStatusForm = {
  @react.component
  let make = (~status: statusTuple) => {
    let classes = Styles.useStyles()
    let (trans, _) = I18n.useSimpleTranslation()
    let (tuple, setTuple) = React.useState(_ => status)

    let moodChange = (evt: ReactEvent.Form.t, _) => {
      let val = ReactEvent.Form.target(evt)["value"]
      setTuple(prev => {
        let (_, s) = prev
        (Mood.convert(val), s)
      })
    }
    let strategyChange = (evt: ReactEvent.Form.t, _) => {
      let val = ReactEvent.Form.target(evt)["value"]
      setTuple(prev => {
        let (m, _) = prev
        (m, Strategy.convert(val))
      })
    }

    let (mood, strategy) = tuple
    <div className=classes.formContainer>
      <div className=classes.rowLable> {"Umamusume Status"->trans} </div>
      <div className=classes.rowContent>
        <FormControl style=itemStyle>
          <InputLabel htmlFor="" id="mood"> {"Mood"->trans} </InputLabel>
          <Select labelId="mood" value={mood->Mood.string->Select.Value.string} onChange=moodChange>
            {Mood.list
            ->Belt.List.map(val => {
              let str = Mood.string(val)
              <MenuItem value={str->MenuItem.Value.string}> {str->React.string} </MenuItem>
            })
            ->Belt.List.toArray}
          </Select>
        </FormControl>
        <FormControl style=itemStyle>
          <InputLabel htmlFor="" id="strategy"> {"Strategy"->trans} </InputLabel>
          <Select
            labelId="strategy"
            value={strategy->Strategy.string->Select.Value.string}
            onChange=strategyChange>
            {Strategy.list
            ->Belt.List.map(val => {
              let str = Strategy.string(val)
              <MenuItem value={str->MenuItem.Value.string}> {str->React.string} </MenuItem>
            })
            ->Belt.List.toArray}
          </Select>
        </FormControl>
      </div>
    </div>
  }
}

type raceSettings = (Field.t, FieldStatus.t, int)
module RaceForm = {
  @react.component
  let make = (~settings: raceSettings) => {
    let classes = Styles.useStyles()
    let (trans, _) = I18n.useSimpleTranslation()
    let (tuple, setTuple) = React.useState(_ => settings)

    let fieldChange = (evt: ReactEvent.Form.t, _) => {
      let val = ReactEvent.Form.target(evt)["value"]
      setTuple(prev => {
        let (_, s, l) = prev
        (Field.convert(val), s, l)
      })
    }
    let fstatusChange = (evt: ReactEvent.Form.t, _) => {
      let val = ReactEvent.Form.target(evt)["value"]
      setTuple(prev => {
        let (f, _, l) = prev
        (f, FieldStatus.convert(val), l)
      })
    }
    let distanceChange = (evt: ReactEvent.Form.t) => {
      let newVal = ReactEvent.Form.target(evt)["value"]->Belt.Int.fromString
      switch newVal {
      | Some(v) =>
        setTuple(prev => {
          let (f, s, _) = prev
          (f, s, v)
        })
      | None => ()
      }
    }

    let (field, fstatus, length) = tuple
    <div className=classes.formContainer>
      <div className=classes.rowLable> {"Race Settings"->trans} </div>
      <div className=classes.rowContent>
        <FormControl style=itemStyle>
          <InputLabel htmlFor="" id="field"> {"Field"->trans} </InputLabel>
          <Select
            labelId="field" value={field->Field.string->Select.Value.string} onChange=fieldChange>
            {Field.list
            ->Belt.List.map(val => {
              let str = Field.string(val)
              <MenuItem value={str->MenuItem.Value.string}> {str->React.string} </MenuItem>
            })
            ->Belt.List.toArray}
          </Select>
        </FormControl>
        <FormControl style=itemStyle>
          <InputLabel htmlFor="" id="field-status"> {"Field Status"->trans} </InputLabel>
          <Select
            labelId="field-status"
            value={fstatus->FieldStatus.string->Select.Value.string}
            onChange=fstatusChange>
            {FieldStatus.list
            ->Belt.List.map(val => {
              let str = FieldStatus.string(val)
              <MenuItem value={str->MenuItem.Value.string}> {str->React.string} </MenuItem>
            })
            ->Belt.List.toArray}
          </Select>
        </FormControl>
        <TextField
          style=itemStyle
          id="race-distance"
          label={"Distance"->trans}
          defaultValue={TextField.DefaultValue.int(length)}
          helperText={length->Distance.fromLength->Distance.string->React.string}
          onChange=distanceChange
        />
      </div>
    </div>
  }
}

@react.component
let make = () => {
  <>
    <UmaAttributeForm attrs={(600, 600, 600, 600, 600)} />
    <UmaPreferenceForm pref={(Rank.A, Rank.A, Rank.A)} />
    <UmaStatusForm status={(Mood.Good, Strategy.Leading)} />
    <RaceForm settings={(Field.Turf, FieldStatus.Good, 1600)} />
  </>
}
