module Styles = {
  open Css;
  let currentItemTitle =
    style([
      display(`inlineFlex),
      alignItems(`center),
      marginTop(`px(4)),
      marginLeft(`rem(0.5)),
    ]);

  let container = {
    merge([
      Theme.Body.medium,
      style([
        position(`relative),
        width(`percent(100.)),
        letterSpacing(`rem(-0.0125)),
        fontWeight(`num(500)),
        border(`px(1), `solid, Theme.Colors.hyperlinkAlpha(0.3)),
        borderRadius(`px(4)),
        padding(`px(5)),
        cursor(`pointer),
        after([
          // Render triangle icon at the end of the dropdown
          unsafe("content", ""),
          position(`absolute),
          width(`zero),
          height(`zero),
          borderLeft(`px(7), `solid, transparent),
          borderRight(`px(7), `solid, transparent),
          borderTop(`px(7), `solid, Theme.Colors.teal),
          borderRadius(`px(4)),
          right(`px(15)),
          top(`percent(40.)),
        ]),
      ]),
    ]);
  };

  let collapsedDropdown = {
    style([
      position(`absolute),
      left(`zero),
      right(`zero),
      fontWeight(`num(500)),
      backgroundColor(Theme.Colors.white),
      pointerEvents(`none),
      border(`px(1), `solid, Theme.Colors.hyperlinkAlpha(0.3)),
      opacity(0.),
    ]);
  };

  let expandedDropdown = {
    merge([
      collapsedDropdown,
      style([
        top(`rem(2.3)),
        borderRadius(`px(4)),
        pointerEvents(`auto),
        opacity(1.),
        zIndex(100),
        selector(
          "li",
          [
            display(`block),
            textDecoration(`none),
            padding(`px(10)),
            hover([background(Theme.Colors.gandalf)]),
          ],
        ),
      ]),
    ]);
  };
};

[@react.component]
let make = (~items, ~currentItem, ~onItemPress) => {
  let (menuOpen, toggleMenu) = React.useState(() => false);

  let onDropdownItemPress = item => {
    onItemPress(item);
    toggleMenu(_ => !menuOpen);
  };

  <div className=Styles.container onClick={_ => {toggleMenu(_ => !menuOpen)}}>
    <span className=Styles.currentItemTitle>
      {React.string(currentItem)}
    </span>
    <ul
      className={menuOpen ? Styles.expandedDropdown : Styles.collapsedDropdown}>
      {items
       |> Array.map(item => {
            <li key=item onClick={_ => {onDropdownItemPress(item)}}>
              {React.string(item)}
            </li>
          })
       |> React.array}
    </ul>
  </div>;
};
