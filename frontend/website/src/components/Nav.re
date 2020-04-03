// Nav styles adapted from https://medium.com/creative-technology-concepts-code/responsive-mobile-dropdown-navigation-using-css-only-7218e4498a99

module Style = {
  open Css;
  open Theme;

  module MediaQuery = {
    let menu = "(min-width: 62rem)";
    let menuMax = "(max-width: 61.9375rem)";
  };
  let bottomNudge = Css.marginBottom(`rem(2.0));
  let bottomNudgeOffset = offset => Css.marginBottom(`rem(2.0 -. offset));

  let collapsedMenuItems =
    style([
      marginTop(`zero),
      marginBottom(`zero),
      display(`none),
      // when it's not hidden, make the dropdown appear
      position(`absolute),
      right(`rem(1.0)),
      top(`rem(0.0)),
      backgroundColor(Colors.white),
      // always visible and flexed on full
      media(
        MediaQuery.menu,
        [
          display(`flex),
          justifyContent(`spaceBetween),
          position(`static),
          alignItems(`center),
          width(`percent(100.0)),
        ],
      ),
    ]);

  let triangle = (c, offset) => [
    unsafe("content", ""),
    width(`zero),
    height(`zero),
    position(`absolute),
    borderLeft(`px(4 + offset), `solid, c),
    borderRight(`px(4 + offset), `solid, transparent),
    borderTop(`px(4 + offset), `solid, c),
    borderBottom(`px(4 + offset), `solid, transparent),
    right(`px(15 - offset)),
    top(`px((-3) - offset)),
    transforms([`rotate(`deg(45.))]),
    media(MediaQuery.menu, [display(`none)]),
  ];

  let expandedMenuBorderColor = Theme.Colors.hyperlinkLight;
  let expandedMenuItems =
    merge([
      collapsedMenuItems,
      style([
        media(
          // Make expanded menu not show up on a wide screen
          MediaQuery.menuMax,
          Theme.paddingY(`rem(0.3))
          @ [
            border(`px(1), `solid, expandedMenuBorderColor),
            boxShadow(
              Shadow.box(
                ~x=px(0),
                ~y=px(0),
                ~blur=px(12),
                ~spread=px(0),
                rgba(0, 0, 0, 0.12),
              ),
            ),
            borderRadius(`px(10)),
            marginTop(`zero),
            marginRight(`rem(-0.6)),
            display(`flex),
            maxWidth(`rem(10.)),
            flexDirection(`column),
            alignItems(`flexStart),
            position(`absolute),
            before(
              triangle(expandedMenuBorderColor, 1)
              @ [
                boxShadow(
                  Shadow.box(
                    ~x=px(0),
                    ~y=px(0),
                    ~blur=px(12),
                    ~spread=px(0),
                    rgba(0, 0, 0, 0.12),
                  ),
                ),
                zIndex(-1),
              ],
            ),
            after(triangle(white, 0) @ [zIndex(1)]),
          ],
        ),
      ]),
    ]);

  let menuToggleButton =
    merge([
      Theme.Link.basic,
      style(
        Theme.paddingY(`rem(0.5))
        @ [
          marginLeft(`rem(1.0)),
          border(`zero, `solid, `transparent),
          cursor(`pointer),
          display(`flex),
          justifyContent(`flexEnd),
          position(`relative),
          userSelect(`none),
          backgroundColor(`transparent),
          outline(`zero, `none, `transparent),
          focus([color(Theme.Colors.hyperlinkHover)]),
          // The menu is always shown on full-size
          media(MediaQuery.menu, [display(`none)]),
        ],
      ),
    ]);
};

module DropdownMenu = {
  [@react.component]
  let make = (~children) => {
    let (menuOpen, toggleMenu) = React.useState(() => false);
    <>
      <button
        className=Style.menuToggleButton
        id="nav-menu-btn"
        onClick={_ => toggleMenu(_ => !menuOpen)}>
        {React.string("Menu")}
      </button>
      <div className=Css.(style([zIndex(1), position(`relative)]))>
        <ul
          id="nav-menu"
          className={
            menuOpen ? Style.expandedMenuItems : Style.collapsedMenuItems
          }>
          children
        </ul>
      </div>
    </>;
  };
};

// TODO Please fix these styles so we don't need to mapi...
module NavWrapper = {
  [@react.component]
  let make = (~keepAnnouncementBar, ~children) => {
    let items =
      children
      |> Array.mapi((idx, elem) =>
           if (idx == Array.length(children) - 1) {
             <li
               className={Css.style([
                 Css.paddingLeft(`rem(0.75)), // we need to skip padding right here as it's on the edge
                 Css.listStyle(`none, `inside, `none),
                 Css.media(
                   Style.MediaQuery.menuMax,
                   [Css.width(`percent(100.)), Css.padding(`zero)],
                 ),
               ])}>
               elem
             </li>;
           } else {
             <>
               <li
                 className={Css.style(
                   Theme.paddingX(`rem(0.75))
                   @ Theme.paddingY(`rem(0.5))
                   @ [
                     Css.listStyle(`none, `inside, `none),
                     Css.media(
                       Style.MediaQuery.menuMax,
                       [Css.width(`percent(100.)), Css.padding(`zero)],
                     ),
                   ],
                 )}>
                 elem
               </li>
               <hr
                 ariaHidden=true
                 className=Css.(
                   style([
                     borderTop(
                       `rem(0.0625),
                       `solid,
                       Theme.Colors.hyperlinkAlpha(0.15),
                     ),
                     marginTop(`zero),
                     marginBottom(`zero),
                     borderBottomWidth(`zero),
                     borderLeftWidth(`zero),
                     borderRightWidth(`zero),
                     width(`percent(85.)),
                     media(Style.MediaQuery.menu, [display(`none)]),
                   ])
                 )
               />
             </>;
           }
         )
      |> ReactExt.staticArray;

    <nav
      className=Css.(
        style([
          display(`flex),
          justifyContent(`spaceBetween),
          alignItems(`flexEnd),
          flexWrap(`wrap),
          padding2(~v=`zero, ~h=`rem(1.25)),
          marginTop(`rem(2.0)),
          media(
            Theme.MediaQuery.notMobile,
            [padding2(~v=`zero, ~h=`rem(3.)), marginTop(`rem(2.0))],
          ),
          media(
            Theme.MediaQuery.full,
            [maxWidth(`rem(89.)), marginLeft(`auto), marginRight(`auto)],
          ),
          media(
            Theme.MediaQuery.statusLift(keepAnnouncementBar),
            [flexWrap(`nowrap), alignItems(`center)],
          ),
        ])
      )>
      <Next.Link href="/">
        <a
          className=Css.(
            style([
              display(`flex),
              Style.bottomNudge,
              width(`percent(50.0)),
              marginTop(`zero),
              media(
                Theme.MediaQuery.statusLift(keepAnnouncementBar),
                [
                  width(`auto),
                  marginRight(`rem(0.75)),
                  marginTop(`zero),
                  Style.bottomNudgeOffset(0.1875),
                ],
              ),
              media(Style.MediaQuery.menu, [marginTop(`zero)]),
            ])
          )>
          <Image className="" name="/static/img/coda-logo" alt="Coda Home" />
        </a>
      </Next.Link>
      <div
        className=Css.(
          style([
            order(3),
            width(`percent(100.0)),
            Style.bottomNudge,
            display(`none), // just hide when status lift happens
            media(
              Theme.MediaQuery.statusLift(keepAnnouncementBar),
              [
                order(2),
                width(`auto),
                marginLeft(`zero),
                ...keepAnnouncementBar
                     ? [display(`block)] : [display(`none)],
              ],
            ),
            media(Style.MediaQuery.menu, [width(`percent(40.0))]),
          ])
        )>
        <div
          className=Css.(
            style([
              width(`rem(22.25)),
              media(
                Theme.MediaQuery.statusLift(keepAnnouncementBar),
                [width(`rem(22.25)), margin(`auto)],
              ),
            ])
          )>
          <AnnouncementBar />
        </div>
      </div>
      <div
        className=Css.(
          style([
            position(`relative),
            width(`auto),
            maxWidth(px(500)),
            order(2),
            Style.bottomNudgeOffset(0.5),
            media(
              Theme.MediaQuery.statusLift(keepAnnouncementBar),
              [order(3), width(`auto), Style.bottomNudge],
            ),
            media(Style.MediaQuery.menu, [width(`percent(50.0))]),
          ])
        )>
        <DropdownMenu> items </DropdownMenu>
      </div>
    </nav>;
  };
};

let menuStyle =
  Theme.paddingX(`rem(1.8))
  @ Theme.paddingY(`rem(0.5))
  @ Css.[
      height(`auto),
      margin(`zero),
      borderWidth(`zero),
      Theme.Typeface.ibmplexsans,
      color(Theme.Colors.saville),
      fontSize(`rem(1.0)),
      lineHeight(`rem(1.5)),
      fontWeight(`medium),
      letterSpacing(`rem(0.)),
      fontStyle(`normal),
      display(`block),
      width(`percent(100.)),
      textTransform(`none),
      outline(`zero, `none, `transparent),
      focus([color(Theme.Colors.hyperlink)]),
      hover([backgroundColor(`transparent), color(Theme.Colors.hyperlink)]),
    ];

module SimpleButton = {
  open Theme;

  [@react.component]
  let make = (~name, ~link, ~target="_self") => {
    let router = Next.Router.useRouter();
    let currentSlug = Js.String.split("/", router.route);
    let isActive = {
      switch (currentSlug[1]) {
      | first => link == "/" ++ first
      | exception Not_found => false
      };
    };

    <Next.Link href=link>
      <a
        target
        className=Css.(
          merge([
            Body.basic,
            style(
              Theme.paddingY(`rem(0.75))
              @ [
                margin(`zero),
                textDecoration(`none),
                whiteSpace(`nowrap),
                hover([color(Theme.Colors.hyperlink)]),
                isActive ? color(Colors.hyperlink) : color(Colors.saville),
                media(Style.MediaQuery.menuMax, menuStyle),
              ],
            ),
          ])
        )>
        {React.string(name)}
      </a>
    </Next.Link>;
  };
};

[@react.component]
let make = () => {
  <NavWrapper keepAnnouncementBar=true>
    [|
      <SimpleButton name="Docs" link="/docs" />,
      <SimpleButton name="Blog" link="/blog" />,
      <SimpleButton name="Careers" link="/jobs" />,
      <SimpleButton name="Testnet" link="/testnet" />,
    |]
  </NavWrapper>;
};
