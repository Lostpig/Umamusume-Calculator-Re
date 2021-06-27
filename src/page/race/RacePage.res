open MaterialUi
open Variable
module Rs = ReactDOMStyle

let dvInt = TextField.DefaultValue.int

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
    ~padding="0 3px",
    (),
  ),
  displayValue: Rs.make(
    ~marginTop="16px",
    ~height="1.1876rem;",
    ~padding="5px 5px 8px 10px",
    ~color=Mui_System.Colors.t_primary,
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

type umaAttr = Speed | Stamina | Power | Guts | Knowledge
type umaAttrs = (int, int, int, int, int)
type umaPreference = Field | Distance | Strategy
type umaPreferences = (Rank.t, Rank.t, Rank.t)
type umaStatus = Mood | Strategy
type statusTuple = (Mood.t, Strategy.t)
type raceSettings = (Field.t, FieldStatus.t, int)
type patch =
  | UmaAttr(umaAttrs)
  | Preferences(umaPreferences)
  | UmaStatus(statusTuple)
  | RaceSetting(raceSettings)

module UmaAttributeForm = {
  @react.component
  let make = (~attrs: umaAttrs, ~dispatch: patch => unit) => {
    let classes = Styles.useStyles()
    let (trans, _) = I18n.useSimpleTranslation()

    let (speed, stamina, power, guts, knowledge) = attrs
    let updateAttr = (umaAttr, val: int) => {
      switch umaAttr {
      | Speed => (val, stamina, power, guts, knowledge)
      | Stamina => (speed, val, power, guts, knowledge)
      | Power => (speed, stamina, val, guts, knowledge)
      | Guts => (speed, stamina, power, val, knowledge)
      | Knowledge => (speed, stamina, power, guts, val)
      }
      ->UmaAttr
      ->dispatch
    }
    let handleChange = (umaAttr, evt: ReactEvent.Form.t) => {
      let newVal = ReactEvent.Form.target(evt)["value"]->Belt.Int.fromString
      switch newVal {
      | Some(v) => umaAttr->updateAttr(v)
      | None => ()
      }
    }

    <UmaFormContainer label={"Base Attributes"->trans}>
      <TextField
        required={true}
        className=classes.formItem
        label={"Speed"->trans}
        defaultValue={speed->dvInt}
        onChange={Speed->handleChange}
      />
      <TextField
        required={true}
        className=classes.formItem
        label={"Stamina"->trans}
        defaultValue={stamina->dvInt}
        onChange={Stamina->handleChange}
      />
      <TextField
        required={true}
        className=classes.formItem
        label={"Power"->trans}
        defaultValue={power->dvInt}
        onChange={Power->handleChange}
      />
      <TextField
        required={true}
        className=classes.formItem
        label={"Guts"->trans}
        defaultValue={guts->dvInt}
        onChange={Guts->handleChange}
      />
      <TextField
        required={true}
        className=classes.formItem
        label={"Knowledge"->trans}
        defaultValue={knowledge->dvInt}
        onChange={Knowledge->handleChange}
      />
    </UmaFormContainer>
  }
}
module UmaPreferenceForm = {
  @react.component
  let make = (~pref: umaPreferences, ~dispatch: patch => unit) => {
    let (trans, _) = I18n.useSimpleTranslation()

    let (field, distance, strategy) = pref
    let handleChange = (umaPreference, rank) => {
      switch umaPreference {
      | Field => (rank, distance, strategy)
      | Distance => (field, rank, strategy)
      | Strategy => (field, distance, rank)
      }
      ->Preferences
      ->dispatch
    }

    <UmaFormContainer label={"Preference"->trans}>
      <UmaEnumSelector
        id="pref-field"
        label={"Field"->trans}
        value={field}
        onChange={Field->handleChange}
        enum=enumRank
      />
      <UmaEnumSelector
        id="pref-distance"
        label={"Distance"->trans}
        value={distance}
        onChange={Distance->handleChange}
        enum=enumRank
      />
      <UmaEnumSelector
        id="pref-strategy"
        label={"Strategy"->trans}
        value={strategy}
        onChange={Strategy->handleChange}
        enum=enumRank
      />
    </UmaFormContainer>
  }
}
module UmaStatusForm = {
  @react.component
  let make = (~status: statusTuple, ~dispatch: patch => unit) => {
    let (trans, _) = I18n.useSimpleTranslation()
    let (mood, strategy) = status

    let moodChange = val => {
      (val, strategy)->UmaStatus->dispatch
    }
    let strategyChange = val => {
      (mood, val)->UmaStatus->dispatch
    }

    <UmaFormContainer label={"Umamusume Status"->trans}>
      <UmaEnumSelector
        id="status-mood" label={"Mood"->trans} value=mood onChange=moodChange enum=enumMood
      />
      <UmaEnumSelector
        id="status-strategy"
        label={"Strategy"->trans}
        value=strategy
        onChange=strategyChange
        enum=enumStrategy
      />
    </UmaFormContainer>
  }
}
module RaceForm = {
  @react.component
  let make = (~settings: raceSettings, ~dispatch: patch => unit) => {
    let classes = Styles.useStyles()
    let (trans, _) = I18n.useSimpleTranslation()
    let (field, fstatus, length) = settings

    let fieldChange = val => {
      (val, fstatus, length)->RaceSetting->dispatch
    }
    let fstatusChange = val => {
      (field, val, length)->RaceSetting->dispatch
    }
    let distanceChange = (evt: ReactEvent.Form.t) => {
      let newVal = ReactEvent.Form.target(evt)["value"]->Belt.Int.fromString
      switch newVal {
      | Some(v) => (field, fstatus, v)->RaceSetting->dispatch
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
module CorrectionAttributes = {
  @react.component
  let make = (~attrs: umaAttrs) => {
    let (trans, _) = I18n.useSimpleTranslation()
    let (speed, stamina, power, guts, knowledge) = attrs

    <UmaFormContainer label={"Correction Attributes"->trans}>
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
  let make = () => {
    let (trans, _) = I18n.useSimpleTranslation()

    <UmaFormContainer label={"Base Ability"->trans}>
      <ValueDisplayer label={"Base Speed"->trans} value={40.00->React.float} />
      <ValueDisplayer label={"Hp"->trans} value={1550.00->React.float} />
      <ValueDisplayer label={"Hp"->trans} value={2000.00->React.float} />
      <ValueDisplayer label={"Hp Coef"->trans} value={46.50->React.float} />
      <ValueDisplayer label={"Spurt Coef"->trans} value={89.60->React.float} />
    </UmaFormContainer>
  }
}
module RaceSummary = {
  @react.component
  let make = () => {
    let (trans, _) = I18n.useSimpleTranslation()

    <UmaFormContainer label={"Summary"->trans}>
      <ValueDisplayer label={"Spurt Distance"->trans} value={333.33->React.float} />
      <ValueDisplayer label={"Time"->trans} value={"1:13.44"->React.string} />
      <ValueDisplayer label={"Display Time"->trans} value={"2:27.41"->React.string} />
      <ValueDisplayer label={"Hp Remained"->trans} value={450.5->React.float} />
      <ValueDisplayer
        label={"Exhaustion"->trans} sub={"Time"->trans} color=Danger value={"1.56s"->React.string}
      />
      <ValueDisplayer
        label={"Exhaustion"->trans}
        sub={"Distance"->trans}
        color=Warning
        value={"45.00m"->React.string}
      />
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
  hpDecrease: float,
}
module RaceDetail = {
  @react.component
  let make = () => {
    let (trans, _) = I18n.useSimpleTranslation()
    let stages: array<raceStage> = [
      {
        stage: "Starting",
        spStart: 3.00,
        spTarget: 17.850,
        acceleration: 24.21,
        time: 0.666,
        distance: 6.888,
        hpDecrease: 14.567,
      },
      {
        stage: "First Acceleration",
        spStart: 3.00,
        spTarget: 17.850,
        acceleration: 24.21,
        time: 0.666,
        distance: 6.888,
        hpDecrease: 14.567,
      },
      {
        stage: "First Cruise",
        spStart: 3.00,
        spTarget: 17.850,
        acceleration: 24.21,
        time: 0.666,
        distance: 6.888,
        hpDecrease: 14.567,
      },
      {
        stage: "Middle Speed Regulate",
        spStart: 3.00,
        spTarget: 17.850,
        acceleration: 24.21,
        time: 0.666,
        distance: 6.888,
        hpDecrease: 14.567,
      },
      {
        stage: "Middle Cruise",
        spStart: 3.00,
        spTarget: 17.850,
        acceleration: 24.21,
        time: 0.666,
        distance: 6.888,
        hpDecrease: 14.567,
      },
      {
        stage: "Last Acceleration",
        spStart: 3.00,
        spTarget: 17.850,
        acceleration: 24.21,
        time: 0.666,
        distance: 6.888,
        hpDecrease: 14.567,
      },
      {
        stage: "Last Cruise",
        spStart: 3.00,
        spTarget: 17.850,
        acceleration: 24.21,
        time: 0.666,
        distance: 6.888,
        hpDecrease: 14.567,
      },
      {
        stage: "Spurt Acceleration",
        spStart: 3.00,
        spTarget: 17.850,
        acceleration: 24.21,
        time: 0.666,
        distance: 6.888,
        hpDecrease: 14.567,
      },
      {
        stage: "Spurt Cruise",
        spStart: 3.00,
        spTarget: 17.850,
        acceleration: 24.21,
        time: 0.666,
        distance: 6.888,
        hpDecrease: 14.567,
      },
      {
        stage: "Exhaustion",
        spStart: 3.00,
        spTarget: 17.850,
        acceleration: 24.21,
        time: 0.666,
        distance: 6.888,
        hpDecrease: 14.567,
      },
    ]

    <Accordion>
      <AccordionSummary expandIcon={<Icons.ExpandMore />}>
        <Typography> {"Race Details"->trans} </Typography>
      </AccordionSummary>
      <AccordionDetails>
        <Table>
          <TableHead>
            <TableRow>
              <TableCell> {"Stage"->trans} </TableCell>
              <TableCell> {"Start Speed"->trans} </TableCell>
              <TableCell> {"Target Speed"->trans} </TableCell>
              <TableCell> {"Acceleration"->trans} </TableCell>
              <TableCell> {"Time"->trans} </TableCell>
              <TableCell> {"Distance"->trans} </TableCell>
              <TableCell> {"Hp Decrease"->trans} </TableCell>
            </TableRow>
          </TableHead>
          <TableBody>
            {stages->Belt.Array.map(s =>
              <TableRow key={s.stage}>
                <TableCell> {s.stage->trans} </TableCell>
                <TableCell> {s.spStart->React.float} </TableCell>
                <TableCell> {s.spTarget->React.float} </TableCell>
                <TableCell> {s.acceleration->React.float} </TableCell>
                <TableCell> {s.time->React.float} </TableCell>
                <TableCell> {s.distance->React.float} </TableCell>
                <TableCell> {s.hpDecrease->React.float} </TableCell>
              </TableRow>
            )}
          </TableBody>
        </Table>
      </AccordionDetails>
    </Accordion>
  }
}

type raceProps = {
  attrs: umaAttrs,
  preferences: umaPreferences,
  status: statusTuple,
  race: raceSettings,
}
@react.component
let make = () => {
  let (trans, _) = I18n.useSimpleTranslation()
  let classes = Styles.useStyles()

  let (raceState, dispatch) = React.useReducer(
    (prev, patch) => {
      switch patch {
      | UmaAttr(attrs) => {...prev, attrs: attrs}
      | Preferences(preferences) => {...prev, preferences: preferences}
      | UmaStatus(status) => {...prev, status: status}
      | RaceSetting(race) => {...prev, race: race}
      }
    },
    {
      attrs: (600, 600, 600, 600, 600),
      preferences: (Rank.A, Rank.A, Rank.A),
      status: (Mood.Good, Strategy.Leading),
      race: (Field.Turf, FieldStatus.Good, 1600),
    },
  )

  <>
    <h2 className=classes.header> {"Input"->trans} </h2>
    <UmaAttributeForm attrs={raceState.attrs} dispatch />
    <UmaPreferenceForm pref={raceState.preferences} dispatch />
    <UmaStatusForm status={raceState.status} dispatch />
    <RaceForm settings={raceState.race} dispatch />
    <Button />
    <Divider />
    <h2 className=classes.header> {"Result"->trans} </h2>
    <CorrectionAttributes attrs={raceState.attrs} />
    <BaseAbilities />
    <RaceSummary />
    <Divider />
    <h2 className=classes.header> {"Details"->trans} </h2>
    <RaceDetail />
  </>
}
