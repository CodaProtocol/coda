[@react.component]
let make =
    (
      ~src,
      ~width=2.,
      ~height=2.,
      ~marginLeft=0.5,
      ~marginRight=0.5,
      ~mobileMarginLeft=0.5,
      ~mobileMarginRight=0.5,
      ~title: string=?,
      ~alt: string=?,
    ) => {
  <img
    src
    title
    alt
    className={Css.merge([
      Css.style([
        Css.height(`rem(height)),
        Css.width(`rem(width)),
        Css.display(`flex),
        Css.justifyContent(`center),
        Css.alignItems(`center),
        Css.marginRight(`rem(mobileMarginRight)),
        Css.marginLeft(`rem(mobileMarginLeft)),
        Css.position(`relative),
        Css.media(
          Theme.MediaQuery.notMobile,
          [
            Css.marginRight(`rem(marginRight)),
            Css.marginLeft(`rem(marginLeft)),
          ],
        ),
      ]),
    ])}
  />;
};
