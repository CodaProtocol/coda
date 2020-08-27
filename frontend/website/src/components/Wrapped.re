[@react.component]
let make = (~overflowHidden=false, ~children) => {
  <div
    className=Css.(
      style(
        (overflowHidden ? [overflow(`hidden)] : [])
        @ [
          margin(`auto),
          media(
            Theme.MediaQuery.full,
            [
              maxWidth(`rem(89.0)),
              margin(`auto),
              ...Theme.paddingX(`rem(3.0)),
            ],
          ),
          ...Theme.paddingX(`rem(1.25)),
        ],
      )
    )>
    children
  </div>;
};
