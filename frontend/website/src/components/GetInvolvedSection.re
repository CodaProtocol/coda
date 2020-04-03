module KnowledgeBase = {
  module SubSection = {
    [@react.component]
    let make = (~className="", ~title, ~content) => {
      let items =
        content
        |> Array.mapi((i, {title, url}: ContentType.KnowledgeBase.link) =>
             <li
               key={string_of_int(i)}
               className=Css.(
                 style([
                   marginBottom(`rem(0.5)),
                   color(Theme.Colors.hyperlink),
                   listStyle(`none, `inside, `none),
                   marginLeft(`rem(1.5)),
                   marginRight(`rem(1.)),
                   before([
                     contentRule(`text({js|*|js})),
                     color(Theme.Colors.hyperlink),
                     display(`inlineBlock),
                     marginLeft(`rem(-1.)),
                     marginRight(`rem(0.6)),
                     verticalAlign(`bottom),
                   ]),
                 ])
               )>
               <a
                 href=url
                 className=Css.(
                   merge([Theme.Link.basic, style([cursor(`pointer)])])
                 )>
                 {React.string(title)}
               </a>
             </li>
           )
        |> React.array;

      <div className>
        <h5
          className=Css.(
            merge([
              Theme.H5.basic,
              style([
                marginLeft(`zero),
                color(Theme.Colors.slate),
                marginRight(`zero),
                marginTop(`rem(1.)),
                marginBottom(`rem(0.75)),
                media(
                  Theme.MediaQuery.notMobile,
                  [marginTop(`rem(1.)), marginLeft(`rem(0.5))],
                ),
              ]),
            ])
          )>
          {React.string(title)}
        </h5>
        <ul
          className=Css.(
            style([
              marginRight(`zero),
              paddingBottom(`zero),
              paddingLeft(`zero),
              paddingRight(`zero),
              marginBottom(`zero),
              maxWidth(`rem(24.5)),
            ])
          )>
          items
        </ul>
      </div>;
    };
  };

  [@react.component]
  let make = (~links) => {
    let (baseOpen, setOpen) = React.useState(() => false);
    <fieldset
      className=Css.(
        style([
          textAlign(`center),
          Theme.Typeface.ibmplexserif,
          display(`block),
          border(`px(1), `solid, Theme.Colors.hyperlinkAlpha(0.3)),
          borderRadius(`px(18)),
          maxWidth(`rem(58.625)),
          marginLeft(`auto),
          marginRight(`auto),
          unsafe("minWidth", "min-content"),
          paddingBottom(`rem(1.)),
          media(Theme.MediaQuery.notMobile, [paddingBottom(`rem(2.))]),
        ])
      )>
      {ReactDOMRe.createElement(
         "legend",
         ~props=
           ReactDOMRe.objToDOMProps({
             "align": "center",
             "className":
               Css.(
                 style([
                   textAlign(`center),
                   marginTop(`zero),
                   marginBottom(`zero),
                 ])
               ),
           }),
         [|
           <h4
             className=Css.(
               style([
                 textAlign(`center),
                 letterSpacing(`rem(0.1875)),
                 border(`px(1), `solid, Theme.Colors.saville),
                 paddingLeft(`rem(1.25)),
                 paddingRight(`rem(1.25)),
                 paddingTop(`rem(0.25)),
                 paddingBottom(`rem(0.25)),
                 textTransform(`uppercase),
                 fontWeight(`medium),
                 color(Theme.Colors.midnight),
               ])
             )>
             {React.string("Knowledge base")}
           </h4>,
         |],
       )}
      <div
        className=Css.(
          style([
            position(`relative),
            display(`flex),
            justifyContent(`spaceAround),
            flexWrap(`wrap),
            textAlign(`left),
            paddingLeft(`rem(1.0)),
            paddingRight(`rem(1.0)),
            paddingTop(`rem(1.5)),
            paddingBottom(`rem(1.5)),
            height(baseOpen ? auto : `rem(15.)),
            overflow(`hidden),
            after([
              contentRule(`none),
              position(`absolute),
              bottom(`zero),
              left(`zero),
              height(`rem(2.)),
              width(`percent(100.)),
              pointerEvents(`none),
              backgroundImage(
                `linearGradient((
                  `deg(0.),
                  [
                    (`zero, Theme.Colors.white),
                    (`percent(100.), Theme.Colors.whiteAlpha(0.0)),
                  ],
                )),
              ),
            ]),
          ])
        )>
        <SubSection
          title="Articles"
          content={links.ContentType.KnowledgeBase.articles}
        />
        <SubSection
          title="Videos & Podcasts"
          content={links.ContentType.KnowledgeBase.videos}
        />
      </div>
      {baseOpen
         ? React.null
         : <label
             className=Css.(
               merge([
                 Theme.Link.basic,
                 style([
                   color(Theme.Colors.hyperlink),
                   marginTop(`rem(1.0)),
                   marginLeft(`auto),
                   marginRight(`auto),
                   marginBottom(`rem(-1.0)),
                   width(`rem(10.)),
                   height(`rem(2.5)),
                   display(`block),
                   cursor(`pointer),
                 ]),
               ])
             )
             onClick={_ => {setOpen(_ => !baseOpen)}}>
             {React.string({js|View all ↓|js})}
           </label>}
    </fieldset>;
  };
};
module SocialLink = {
  let colorVarName = "--svg-color-social";
  [@react.component]
  let make = (~link, ~name, ~svg) => {
    <a
      name={"getinvolved-" ++ name}
      href=link
      className=Css.(
        style([
          padding(`rem(1.)),
          cursor(`pointer),
          display(`flex),
          textDecoration(`none),
          justifyContent(`center),
          alignItems(`center),
          color(Theme.Colors.fadedBlue),
          // Original color of svg
          unsafe(colorVarName, Theme.Colors.(string(greyBlue))),
          hover([
            color(Theme.Colors.hyperlink),
            unsafe(colorVarName, Theme.Colors.(string(hyperlink))),
          ]),
        ])
      )>
      <div className=Css.(style([marginRight(`rem(1.))]))> svg </div>
      <h3 className=Theme.H3.wideNoColor> {React.string(name)} </h3>
    </a>;
  };
};

module Svg = {
  let className =
    Css.(style([unsafe("fill", "var(" ++ SocialLink.colorVarName ++ ")")]));
  let twitter =
    <svg
      width="34px"
      height="28px"
      viewBox="0 0 34 28"
      version="1.1"
      xmlns="http://www.w3.org/2000/svg"
      xmlnsXlink="http://www.w3.org/1999/xlink">
      <path
        fill=Theme.Colors.(string(greyBlue))
        className
        d="M30.51,6.98 C30.53,7.28 30.5,7.59 30.52,7.90 C30.53,17.24 23.52,28 10.70,28 C6.75,28 3.09,26.84 0,24.83 C0.56,24.89 1.10,24.92 1.68,24.92 C4.94,24.92 7.93,23.80 10.33,21.90 C7.27,21.83 4.70,19.80 3.82,17.00 C4.25,17.06 4.68,17.11 5.13,17.11 C5.76,17.11 6.39,17.02 6.97,16.87 C3.78,16.21 1.38,13.37 1.38,9.93 L1.38,9.84 C2.31,10.37 3.39,10.70 4.53,10.74 C2.65,9.47 1.42,7.31 1.42,4.86 C1.42,3.54 1.77,2.34 2.37,1.29 C5.80,5.58 10.96,8.38 16.74,8.68 C16.63,8.16 16.57,7.61 16.57,7.07 C16.57,3.17 19.68,0 23.54,0 C25.54,0 27.36,0.85 28.63,2.23 C30.20,1.93 31.71,1.33 33.05,0.53 C32.53,2.17 31.43,3.54 29.99,4.42 C31.39,4.27 32.75,3.87 34,3.33 C33.05,4.72 31.86,5.97 30.51,6.98 Z"
        id="IconTwitter"
      />
    </svg>;

  let discord =
    <svg
      xmlns="http://www.w3.org/2000/svg"
      xmlnsXlink="http://www.w3.org/1999/xlink"
      width="34"
      height="38">
      <defs> <path id="a" d="M0 0h34v38H0z" /> </defs>
      <g fill="none" fillRule="evenodd">
        <path
          fill=Theme.Colors.(string(greyBlue))
          className
          d="M19.944 16.424c-.912 0-1.632.8-1.632 1.776s.7359 1.776 1.632 1.776c.9119 0 1.6319-.8 1.6319-1.776s-.72-1.776-1.6318-1.776m-5.84 0c-.912 0-1.632.8-1.632 1.776s.7358 1.776 1.632 1.776c.912 0 1.632-.8 1.632-1.776.016-.976-.72-1.776-1.632-1.776"
        />
        <g>
          <mask id="b" fill="#fff"> <use xlinkHref="#a" /> </mask>
          <path
            fill=Theme.Colors.(string(greyBlue))
            className
            d="M22.5178 24.814s-.6996-.817-1.2825-1.539c2.5453-.703 3.5168-2.261 3.5168-2.261-.7968.513-1.5543.8741-2.2343 1.121-.9714.399-1.9042.665-2.8172.817-1.8653.342-3.5748.247-5.032-.0189-1.1076-.2092-2.0594-.5132-2.8561-.817-.4467-.1712-.9324-.38-1.4181-.6462-.0583-.038-.1166-.0568-.1749-.095-.0389-.0188-.0585-.038-.0777-.0568-.3497-.19-.544-.3232-.544-.3232s.9326 1.52 3.4 2.242c-.5829.722-1.3017 1.577-1.3017 1.577-4.2937-.1328-5.9257-2.8878-5.9257-2.8878 0-6.118 2.7977-11.0772 2.7977-11.0772 2.7977-2.052 5.4594-1.995 5.4594-1.995l.1943.228c-3.4972.988-5.1097 2.489-5.1097 2.489s.4274-.2278 1.146-.551c2.0791-.893 3.7305-1.14 4.4105-1.1968.1166-.0192.2138-.0382.3303-.0382a16.8172 16.8172 0 013.9246-.038c1.8457.209 3.8272.7412 5.848 1.824 0 0-1.535-1.425-4.8379-2.4128l.2722-.3042s2.6617-.057 5.4592 1.995c0 0 2.798 4.9592 2.798 11.0772 0 0-1.6515 2.755-5.9452 2.8878M30.0172 0H3.983C1.7875 0 .0001 1.7481.0001 3.914v25.688c0 2.1662 1.7874 3.9142 3.9828 3.9142h22.032l-1.0299-3.515 2.487 2.2608 2.351 2.128L34 38V3.914C34 1.7481 32.2126 0 30.0173 0"
            mask="url(#b)"
          />
        </g>
      </g>
    </svg>;

  let telegram =
    <svg
      width="36px"
      height="30px"
      viewBox="0 0 36 30"
      version="1.1"
      xmlns="http://www.w3.org/2000/svg"
      xmlnsXlink="http://www.w3.org/1999/xlink">
      <g
        id="coda_website"
        stroke="none"
        strokeWidth="1"
        fill="none"
        fillRule="evenodd">
        <g
          id="coda_homepage"
          transform="translate(-1074.000000, -3292.000000)"
          fill=Theme.Colors.(string(greyBlue))
          className>
          <g id="Community" transform="translate(418.000000, 3032.000000)">
            <g id="Telegram" transform="translate(656.000000, 260.000000)">
              <path
                d="M35.8974224,2.73110855 L30.4647954,28.1890674 C30.0549375,29.985818 28.9860922,30.4330092 27.4672069,29.5865401 L19.1896835,23.5255016 L15.1955776,27.3425983 C14.7535739,27.781804 14.3838981,28.1491396 13.5320365,28.1491396 L14.1267323,19.7722893 L29.4682781,5.99720177 C30.1353018,5.40627048 29.3236223,5.0788626 28.4315785,5.66979389 L9.46560267,17.5363331 L1.30058933,14.9969256 C-0.475461794,14.4459221 -0.507607516,13.2321173 1.67026514,12.3856482 L33.6070397,0.159758876 C35.0857429,-0.391244625 36.3796082,0.487166754 35.8974224,2.73110855 Z"
                id="IconTelegram"
              />
            </g>
          </g>
        </g>
      </g>
    </svg>;
};

[@react.component]
let make = (~links) => {
  <div>
    <h1
      className=Css.(
        merge([
          Theme.H1.hero,
          style([
            color(Theme.Colors.denimTwo),
            marginTop(`rem(6.)),
            marginBottom(`rem(1.5)),
            media(Theme.MediaQuery.notMobile, [textAlign(`center)]),
          ]),
        ])
      )>
      {React.string("Get involved")}
    </h1>
    <div
      className=Css.(
        style([
          display(`flex),
          justifyContent(`center),
          flexWrap(`wrap),
          alignItems(`flexStart),
          maxWidth(`rem(46.0)),
          media(
            Theme.MediaQuery.notMobile,
            [
              justifyContent(`center),
              margin3(~top=`zero, ~h=`auto, ~bottom=`rem(2.)),
            ],
          ),
        ])
      )>
      <NewsletterWidget center=true />
    </div>
    <div
      className=Css.(
        style([
          media(Theme.MediaQuery.notMobile, [marginBottom(`rem(2.4))]),
          display(`flex),
          flexWrap(`wrap),
          justifyContent(`spaceAround),
          alignItems(`center),
          marginTop(`rem(1.0)),
          marginBottom(`rem(1.25)),
          maxWidth(`rem(63.)),
          marginLeft(`auto),
          marginRight(`auto),
        ])
      )>
      <SocialLink
        link="https://twitter.com/codaprotocol"
        name="Twitter"
        svg=Svg.twitter
      />
      <SocialLink
        link="https://bit.ly/CodaDiscord"
        name="Discord"
        svg=Svg.discord
      />
      <SocialLink
        link="https://t.me/codaprotocol"
        name="Telegram"
        svg=Svg.telegram
      />
    </div>
    <KnowledgeBase links />
  </div>;
};
