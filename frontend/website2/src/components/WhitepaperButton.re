open Css;

module Style = {
  let container =
    style([
      position(`relative),
      height(`rem(15.)),
      borderRadius(`px(6)),
      boxSizing(`borderBox),
      backgroundColor(`transparent),
      padding(`zero),
      border(`px(1), `solid, Theme.Colors.marine),
      width(`percent(100.)),
      media(Theme.MediaQuery.notMobile, [width(`rem(27.5))]),
      media(Theme.MediaQuery.veryLarge, [width(`rem(33.125))]),
      hover([backgroundColor(Theme.Colors.tan)]),
    ]);

  let ellipticBackground =
    style([
      position(`absolute),
      top(`zero),
      right(`zero),
      display(`inlineBlock),
    ]);

  let label =
    merge([
      Theme.H4.semiBold,
      style([
        position(`absolute),
        top(`rem(1.875)),
        left(`rem(1.875)),
        color(Theme.Colors.saville),
      ]),
    ]);

  let icon =
    style([
      position(`absolute),
      bottom(`rem(2.0625)),
      left(`rem(1.875)),
    ]);
};

// the page looking icon in the button
module Icon = {
  module Style = {
    let container = style([position(`relative), backgroundColor(white)]);

    let page = style([]);

    let sigil =
      style([
        position(`absolute),
        left(`percent(50.)),
        top(`percent(50.)),
        transform(translate(`percent(-50.), `percent(-50.))),
      ]);
  };

  [@react.component]
  let make = (~sigil) => {
    <div className=Style.container>
      <span className=Style.page> Icons.page </span>
      <span className=Style.sigil> sigil </span>
    </div>;
  };
};

[@react.component]
let make = (~label, ~sigil) => {
  <button className=Style.container ariaLabel=label>
    <span className=Style.ellipticBackground> EllipticBackground.svg </span>
    <label className=Style.label> {React.string(label)} </label>
    <span className=Style.icon> <Icon sigil /> </span>
  </button>;
};
