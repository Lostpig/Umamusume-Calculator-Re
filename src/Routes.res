@react.component
let make = () => {
  let url = RescriptReactRouter.useUrl()

  <>
    {
      switch url.path {
      | list{"race"} => <RacePage />
      | list{"stats"} => <StatsPage />
      | _ => <div>{React.string("Page Not Found")}</div>
      }
    }
  </>
}