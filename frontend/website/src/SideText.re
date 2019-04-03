module Cta = {
  type t = {
    copy: string,
    link: string,
  };
};
let component = ReasonReact.statelessComponent("SideText");
let make = (~className="", ~paragraphs, ~cta, _children) => {
  ...component,
  render: _self => {
    let {Cta.copy, link} = cta;
    let ps =
      Belt.Array.mapWithIndex(
        paragraphs,
        (i, entry) => {
          let content =
            switch (entry) {
            | `str(s) => [|ReasonReact.string(s)|]
            | `styled(xs) =>
              List.map(
                x =>
                  switch (x) {
                  | `emph(s) =>
                    <span className=Style.Body.basic_semibold>
                      {ReasonReact.string(s)}
                    </span>
                  | `str(s) => <span> {ReasonReact.string(s)} </span>
                  },
                xs,
              )
              |> Array.of_list
            };

          <p
            className=Css.(
              merge([
                Style.Body.basic,
                style(
                  if (i == 0) {
                    [marginTop(`zero)];
                  } else {
                    [];
                  },
                ),
              ])
            )
            key={Js.Int.toString(i)}>
            // should be fine to use i here since this is all static content
             ...content </p>;
        },
      );

    <div
      className=Css.(
        merge([
          className,
          style([
            media(Style.MediaQuery.notMobile, [width(`rem(20.625))]),
          ]),
        ])
      )>
      {ReasonReact.array(ps)}
      <a
        target="_blank"
        href=link
        className=Css.(
          merge([Style.Link.basic, style([marginTop(`rem(1.5))])])
        )>
        {ReasonReact.string(copy ++ {j|\u00A0→|j})}
      </a>
    </div>;
  },
};
