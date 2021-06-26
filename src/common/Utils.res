type cls = (string, bool)
let clsx = (s: list<cls>) => {
  s
  ->Belt.List.keepMap(((name, enable)) => enable ? Some(name) : None)
  ->Belt.List.reduce("", (p, c) => p ++ " " ++ c)
}
