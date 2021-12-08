open MaterialUi
module Rs = ReactDOMStyle

module Styles = %makeStyles({
  container: Rs.make(~display="flex", ~flexDirection="column", ~height="100vh", ()),
  toolbar: Rs.make(~paddingRight="80px", ()),
  spacer: Rs.make(~flex="1", ()),
  slideNavWrap: Rs.make(~flex="1", ~width="100%", ~position="relative", ()),
  slideNavOpen: Rs.make(~transform="translateX(0)", ()),
  slideNav: Rs.make(
    ~width="200px",
    ~height="100%",
    ~position="absolute",
    ~left="0",
    ~top="0",
    ~transition="200ms linear",
    ~transform="translateX(-210px)",
    ~boxShadow="1px 0 5px rgba(0, 0, 0, .2)",
    (),
  ),
  contentOnNav: Rs.make(~marginLeft="200px", ()),
  content: Rs.make(~padding="24px 40px", ~marginLeft="0", ~transition="200ms linear", ()),
})

module Header = {
  @react.component
  let make = (~setDrawOpen) => {
    let classes = Styles.useStyles()
    let (trans, setLng) = I18n.useSimpleTranslation()
    let (menuOpen, setMenuOpen) = React.useState(_ => false)
    let anchorEl = React.useRef(Js.Nullable.null)

    let changeLng = lng => {
      setLng(lng)
      setMenuOpen(_ => false)
    }
    let handleClick = _ => {
      let el = anchorEl.current->Js.Nullable.toOption
      switch el {
      | Some(_) => setMenuOpen(prev => !prev)
      | None => ignore()
      }
    }
    let handleClose = (_, _) => {
      setMenuOpen(_ => false)
    }

    <AppBar position={#Static}>
      <Toolbar className={classes.toolbar}>
        <IconButton
          edge={IconButton.Edge.start} color={#Inherit} onClick={_ => setDrawOpen(prev => !prev)}>
          <Icons.Menu />
        </IconButton>
        <span> {"Umamusume Calculator"->trans} </span>
        <span className={classes.spacer} />
        <IconButton
          edge={IconButton.Edge.start}
          color={#Inherit}
          onClick={handleClick}
          ref={ReactDOM.Ref.domRef(anchorEl)}>
          <Icons.Translate />
        </IconButton>
        <Menu anchorEl={Any(anchorEl.current)} _open={menuOpen} onClose={handleClose}>
          {I18n.languages->Belt.Array.map(lng =>
            <MenuItem onClick={_ => lng->changeLng}> {lng->React.string} </MenuItem>
          )}
        </Menu>
      </Toolbar>
    </AppBar>
  }
}

@react.component
let make = () => {
  let classes = Styles.useStyles()
  let (trans, _) = I18n.useSimpleTranslation()
  let (drawOpen, setDrawOpen) = React.useState(_ => true)
  let nav = path => RescriptReactRouter.push(path)

  <div className={classes.container}>
    <Header setDrawOpen />
    <div className={classes.slideNavWrap}>
      <div className={Utils.clsx(list{(classes.slideNav, true), (classes.slideNavOpen, drawOpen)})}>
        <List>
          <ListItem button={true} onClick={_ => "race"->nav}>
            <ListItemText primary={"Race Calculator"->trans} />
          </ListItem>
          <ListItem button={true} onClick={_ => "stats"->nav}>
            <ListItemText primary={"Stats Calculator"->trans} />
          </ListItem>
        </List>
      </div>
      <main className={Utils.clsx(list{(classes.content, true), (classes.contentOnNav, drawOpen)})}> <Routes /> </main>
    </div>
  </div>
}
