type mode =
  | Blue
  | Gray
  | Green
  | Red;

module Styles = {
  open Css;

  let base =
    merge([
      Theme.Text.body,
      style([
        display(`inlineFlex),
        alignItems(`center),
        justifyContent(`center),
        height(`rem(2.5)),
        minWidth(`rem(12.5)),
        padding2(~v=`zero,~h=`rem(1.)),
        background(white),
        border(`px(0), `solid, white),
        borderRadius(`rem(0.25)),
        cursor(`pointer),
        active([outlineStyle(`none)]),
        focus([outlineStyle(`none)]),
      ]),
    ]);
  
  let blue =
    merge([
      base,
      style([
        backgroundColor(Theme.Colors.marineLight),
        color(Theme.Colors.marine),
        hover([backgroundColor(Theme.Colors.marine), color(white)]),
      ]),
    ]);

  let green =
    merge([
      base,
      style([
        backgroundColor(Theme.Colors.serpentine),
        color(white),
        hover([backgroundColor(Theme.Colors.jungle)]),
      ]),
    ]);
  
  let red =
    merge([
      base,
      style([
        backgroundColor(Theme.Colors.roseBud),
        color(white),
        hover([backgroundColor(Theme.Colors.yeezy)]),
      ]),
    ]);

  let gray =
    merge([
      base,
      style([
        backgroundColor(Theme.Colors.slateAlpha(0.05)),
        color(Theme.Colors.midnight),
        hover([backgroundColor(Theme.Colors.slateAlpha(0.2))])
      ]),
    ]);
};

[@react.component]
let make = (~label, ~onClick=?, ~style=Blue) =>
  <button
    ?onClick
    className={
      switch (style) {
      | Blue => Styles.blue
      | Green => Styles.green
      | Red => Styles.red
      | Gray => Styles.gray
      }
    }>
    {React.string(label)}
  </button>;
