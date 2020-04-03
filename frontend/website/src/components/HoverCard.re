module Styles = {
  open Css;

  let ctaButton =
    style([
      background(Theme.Colors.hyperlinkAlpha(0.3 *. 0.2)),
      borderRadius(`px(6)),
      textDecoration(`none),
      padding(`rem(1.43)),
      paddingTop(`rem(1.625)),
      margin(`auto),
      height(`rem(14.25)),
      width(`rem(21.25)),
      media(
        "(min-width: 60rem)",
        [height(`rem(12.5)), width(`rem(23.8))],
      ),
      media(
        "(min-width: 105rem)",
        [height(`rem(11.5)), width(`rem(32.5))],
      ),
      hover([
        backgroundColor(`hex("FAFCFD")),
        cursor(`pointer),
        boxShadow(
          Shadow.box(
            ~x=px(0),
            ~y=px(4),
            ~blur=px(8),
            ~spread=px(0),
            rgba(0, 0, 0, 0.25),
          ),
        ),
        selector("svg", [SVG.fill(Theme.Colors.hyperlink)]),
      ]),
    ]);

  let ctaContent = style([display(`flex)]);

  let ctaText = style([width(`percent(100.))]);

  let ctaHeading =
    merge([
      Theme.H2.basic,
      style([
        fontWeight(`bold),
        color(Theme.Colors.marine),
        textAlign(`left),
        paddingBottom(`rem(0.3)),
      ]),
    ]);

  let ctaBody =
    merge([
      Theme.Body.basic,
      style([marginTop(`rem(1.)), color(Theme.Colors.midnight)]),
    ]);

  let ctaIcon =
    style([
      marginTop(`px(2)),
      minWidth(`px(36)),
      maxHeight(`px(48)),
      flexShrink(0.),
    ]);
  let arrow =
    style([
      marginTop(`rem(0.9)),
      marginLeft(`rem(1.0)),
      SVG.fill(Theme.Colors.hyperlinkAlpha(0.3)),
    ]);

  let headingRow =
    style([
      display(`flex),
      flexDirection(`row),
      justifyContent(`spaceBetween),
    ]);
};

[@react.component]
let make = (~heading, ~text, ~href) => {
  <a href className=Styles.ctaButton>
    <div className=Styles.ctaContent>
      <div className=Styles.ctaText>
        <div className=Styles.headingRow>
          <h2 className=Styles.ctaHeading> heading </h2>
          <svg
            className=Styles.arrow
            width="27"
            height="17"
            viewBox="0 0 27 17"
            xmlns="http://www.w3.org/2000/svg">
            <path
              d="M17.7329 16.716L15.1769 14.16L17.6249 11.712L19.2809 10.308L19.2449 10.164L16.1849 10.344H0.452881V6.52801H16.1849L19.2449 6.70801L19.2809 6.56401L17.6249 5.16001L15.1769 2.71201L17.7329 0.156006L26.0129 8.43601L17.7329 16.716Z"
            />
          </svg>
        </div>
        <p className=Styles.ctaBody> text </p>
      </div>
    </div>
  </a>;
};
