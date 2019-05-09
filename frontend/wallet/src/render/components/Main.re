module Styles = {
  open Css;

  let main =
    style([
      display(`flex),
      flexDirection(`row),
      paddingTop(StyleGuide.Spacing.headerHeight),
      paddingBottom(StyleGuide.Spacing.footerHeight),
      height(`vh(100.)),
      width(`vw(100.)),
    ]);
};

[@react.component]
let make = (~children) => {
  <main className=Styles.main> children </main>;
};
