let extraHeaders = () =>
  <>
    <script src="https://apis.google.com/js/api.js" />
    <script src="public/static/js/leaderboard.js" />
    <script
      src="https://cdnjs.cloudflare.com/ajax/libs/marked/0.7.0/marked.min.js"
      integrity="sha256-0Ed5s/n37LIeAWApZmZUhY9icm932KvYkTVdJzUBiI4="
      // crossOrigin="anonymous"
    />
    <script src="public/static/js/termynal.js" />
    <link rel="stylesheet" href="public/static/css/termynal.css" />
  </>;

module Styles = {
  open Css;

  let markdownStyles =
    style([
      selector(
        "a",
        [
          hover([color(Theme.Colors.hyperlinkHover)]),
          cursor(`pointer),
          ...Theme.Link.basicStyles,
        ],
      ),
      selector(
        "h4",
        Theme.H4.wideStyles
        @ [textAlign(`left), fontSize(`rem(1.)), fontWeight(`light)],
      ),
      selector(
        "code",
        [Theme.Typeface.pragmataPro, color(Theme.Colors.midnight)],
      ),
      selector(
        "p > code, li > code",
        [
          boxSizing(`borderBox),
          padding2(~v=`px(2), ~h=`px(6)),
          backgroundColor(Theme.Colors.slateAlpha(0.05)),
          borderRadius(`px(4)),
        ],
      ),
    ]);

  let page =
    style([
      selector(
        "hr",
        [
          height(px(4)),
          borderTop(px(1), `dashed, Theme.Colors.marine),
          borderLeft(`zero, solid, transparent),
          borderBottom(px(1), `dashed, Theme.Colors.marine),
        ],
      ),
    ]);

  let header =
    style([
      display(`flex),
      flexDirection(`column),
      width(`percent(100.)),
      color(Theme.Colors.slate),
      textAlign(`center),
      margin2(~v=rem(3.5), ~h=`zero),
    ]);

  let content =
    style([
      display(`flex),
      flexDirection(`columnReverse),
      justifyContent(`center),
      width(`percent(100.)),
      marginBottom(`rem(1.5)),
      media(Theme.MediaQuery.somewhatLarge, [flexDirection(`row)]),
    ]);

  let rowStyles = [
    display(`grid),
    gridColumnGap(rem(1.5)),
    gridTemplateColumns([rem(1.), rem(5.5), rem(5.5), rem(2.5)]),
    media(
      Theme.MediaQuery.notMobile,
      [
        width(`percent(100.)),
        gridTemplateColumns([rem(2.5), `auto, rem(6.), rem(2.5)]),
      ],
    ),
  ];

  let row = style(rowStyles);

  let leaderboardContainer =
    style([
      width(`percent(100.)),
      maxWidth(rem(41.)),
      margin2(~v=`zero, ~h=`auto),
    ]);

  let leaderboard =
    style([
      background(Theme.Colors.hyperlinkAlpha(0.15)),
      width(`percent(100.)),
      borderRadius(px(3)),
      paddingTop(`rem(1.)),
      Theme.Typeface.pragmataPro,
      lineHeight(rem(1.5)),
      color(Theme.Colors.midnight),
      selector(".leaderboard-row", rowStyles),
      selector(
        ".leaderboard-row > span",
        [textOverflow(`ellipsis), whiteSpace(`nowrap), overflow(`hidden)],
      ),
      selector("div span:last-child", [opacity(0.5)]),
      selector("div span:nth-child(odd)", [justifySelf(`flexEnd)]),
      selector(
        "#leaderboard-loading",
        [
          textAlign(`center),
          padding2(~v=`rem(2.), ~h=`zero),
          color(Theme.Colors.slateAlpha(0.7)),
        ],
      ),
      selector("div", [padding2(~v=`zero, ~h=`rem(1.))]),
      selector(
        "div:nth-child(even)",
        [backgroundColor(`rgba((71, 130, 130, 0.1)))],
      ),
    ]);

  let headerRow =
    merge([
      row,
      Theme.Body.basic_semibold,
      style([color(Theme.Colors.midnight)]),
    ]);

  let copy =
    style([
      maxWidth(rem(28.)),
      margin3(~top=`zero, ~h=`auto, ~bottom=rem(2.)),
      media(Theme.MediaQuery.somewhatLarge, [marginLeft(rem(5.))]),
      media(Theme.MediaQuery.notMobile, [width(rem(28.))]),
      ...Theme.Body.basicStyles,
    ]);

  let headerLink =
    merge([
      Theme.Link.basic,
      Theme.H3.basic,
      style([
        fontWeight(`semiBold),
        marginTop(rem(0.75)),
        marginLeft(rem(1.75)),
      ]),
    ]);

  let sidebarHeader =
    merge([
      Theme.H4.wide,
      style([textAlign(`left), fontSize(`rem(1.)), fontWeight(`light)]),
    ]);

  let weekHeader =
    merge([Theme.H2.basic, style([padding2(~v=`rem(1.), ~h=`zero)])]);

  let dashboardHeader =
    merge([
      header,
      style([marginTop(rem(1.5)), marginBottom(rem(2.25))]),
    ]);

  let dashboard =
    style([
      width(`percent(100.)),
      height(`rem(30.)),
      border(`px(0), `solid, white),
      borderRadius(px(3)),
    ]);

  let expandButton =
    merge([
      Theme.Link.basic,
      style([
        backgroundColor(Theme.Colors.hyperlink),
        color(white),
        marginLeft(`auto),
        marginRight(`auto),
        marginBottom(`rem(1.5)),
        width(`rem(10.)),
        height(`rem(2.5)),
        display(`block),
        cursor(`pointer),
        borderRadius(`px(4)),
        padding2(~v=`rem(0.25), ~h=`rem(3.)),
        fontWeight(`semiBold),
        lineHeight(`rem(2.5)),
        hover([backgroundColor(Theme.Colors.hyperlinkHover), color(white)]),
      ]),
    ]);

  let gradientSection =
    style([
      width(`percent(100.)),
      position(`relative),
      height(`rem(45.)),
      overflow(`hidden),
      display(`flex),
      flexWrap(`wrap),
      marginLeft(`auto),
      marginRight(`auto),
      justifyContent(`center),
      after([
        contentRule(""),
        position(`absolute),
        bottom(`px(-1)),
        left(`zero),
        height(`rem(8.)),
        width(`percent(100.)),
        pointerEvents(`none),
        backgroundImage(
          `linearGradient((
            `deg(0.),
            [
              (`zero, Theme.Colors.white),
              (`percent(100.), Theme.Colors.whiteAlpha(0.)),
            ],
          )),
        ),
      ]),
    ]);

  let buttonRow =
    style([
      display(`grid),
      gridTemplateColumns([`fr(1.0)]),
      gridRowGap(rem(1.5)),
      gridTemplateRows([`repeat((`num(4), `rem(6.0)))]),
      justifyContent(`center),
      marginLeft(`auto),
      marginRight(`auto),
      marginTop(rem(3.)),
      marginBottom(rem(3.)),
      media(
        "(min-width: 45rem)",
        [
          gridTemplateColumns([`repeat((`num(2), `fr(1.0)))]),
          gridTemplateRows([`repeat((`num(2), `rem(6.0)))]),
          gridColumnGap(rem(1.5)),
        ],
      ),
      media(
        "(min-width: 66rem)",
        [
          gridTemplateColumns([`repeat((`num(2), `fr(1.0)))]),
          gridTemplateRows([`repeat((`num(2), `rem(5.4)))]),
        ],
      ),
      media(
        "(min-width: 70rem)",
        [
          gridTemplateColumns([`repeat((`num(4), `fr(1.0)))]),
          gridTemplateRows([`repeat((`num(1), `rem(7.5)))]),
          gridColumnGap(rem(1.0)),
        ],
      ),
    ]);

  let discordIcon = style([marginTop(`px(-4))]);
  let formIcon = style([marginTop(`px(3))]);
  let heroRow =
    style([
      display(`flex),
      flexDirection(`column),
      justifyContent(`spaceBetween),
      alignItems(`center),
      media("(min-width: 70rem)", [flexDirection(`row)]),
    ]);

  let heroText =
    merge([header, style([maxWidth(`px(500)), textAlign(`left)])]);

  let termynal =
    style([
      height(`rem(16.875)),
      margin2(~v=`zero, ~h=`rem(1.875)),
      fontSize(`rem(0.625)),
      media(
        Theme.MediaQuery.notMobile,
        [
          padding2(~v=`rem(4.6875), ~h=`rem(2.1875)),
          height(`rem(25.)),
          fontSize(`rem(1.)),
        ],
      ),
    ]);
};

module Section = {
  [@react.component]
  let make = (~name, ~children) => {
    let checkboxName = name ++ "-checkbox";
    let labelName = name ++ "-label";
    <div className=Css.(style([display(`flex), flexDirection(`column)]))>
      <input
        type_="checkbox"
        id=checkboxName
        className=Css.(
          style([
            display(`none),
            selector(
              ":checked + div",
              [height(`auto), after([display(`none)])],
            ),
            selector(":checked ~ #" ++ labelName, [display(`none)]),
          ])
        )
      />
      <div className=Styles.gradientSection> children </div>
      <label id=labelName className=Styles.expandButton htmlFor=checkboxName>
        {React.string("Expand " ++ name)}
        <div
          className=Css.(
            style([
              position(`relative),
              bottom(`rem(2.6)),
              left(`rem(9.6)),
            ])
          )>
          {React.string({js| ↓|js})}
        </div>
      </label>
      <RunScript>
        {Printf.sprintf(
           {|document.getElementById("%s").checked = false;|},
           checkboxName,
         )}
      </RunScript>
    </div>;
  };
};

[@react.component]
let make = () => {
  <Page>
    <Next.Head>
      <script src="https://apis.google.com/js/api.js" />
      <script src="/static/js/leaderboard.js" />
      <script
        src="https://cdnjs.cloudflare.com/ajax/libs/marked/0.7.0/marked.min.js"
        integrity="sha256-0Ed5s/n37LIeAWApZmZUhY9icm932KvYkTVdJzUBiI4="
        // crossOrigin="anonymous"
      />
      <script src="/static/js/termynal.js" />
      <link rel="stylesheet" href="/static/css/termynal.css" />
    </Next.Head>
    <Wrapped>
      <div className=Styles.page>
        <div className=Styles.heroRow>
          <div className=Styles.heroText>
            <h1 className=Theme.H1.hero>
              {React.string("Coda Public Testnet")}
            </h1>
            <p className=Theme.Body.basic>
              {React.string(
                 "Coda's public testnet is live! There are weekly challenges for the community \
                  to interact with the testnet and contribute to Coda's development. Each week \
                  features a new competition to recognize and reward top contributors with testnet \
                  points.",
               )}
            </p>
            <br />
            <p className=Theme.Body.basic>
              {React.string(
                 "By participating in the testnet, you'll be helping advance the first cryptocurrency that utilizes recursive zk-SNARKs and production-scale Ouroboros proof of stake consensus.",
               )}
            </p>
          </div>
          <div id="termynal" className=Styles.termynal>
            <RunScript>
              {|var termynal = new Termynal('#termynal', {
            typeDelay: 40,
            lineDelay: 700,
            lineData: [
              { type: 'input', prompt: '>', value: 'coda daemon -peer ...' },
              { type: 'progress' },
              { value:  'Daemon ready. Clients can now connect!'},
              { type: 'input', prompt: '>', value: 'coda client status' },
              { delay: '0', value:  'Max observed block length: 120'},
              { delay: '0', value:  'Peers: 23'},
              { delay: '0', value:  'Consensus time now: epoch=1, slot=13'},
              { delay: '0', value:  'Sync status: Synced'},
            ]
          });|}
            </RunScript>
          </div>
        </div>
        <div>
          <div className=Styles.buttonRow>
            <ActionButton
              icon={React.string({js| 🚥 |js})}
              heading={React.string({js| Get Started |js})}
              text={React.string(
                "Get started by installing Coda and running a node",
              )}
              href="/docs/getting-started/"
            />
            <ActionButton
              icon={
                <img
                  className=Styles.discordIcon
                  src="/static/img/discord.svg"
                />
              }
              heading={React.string({js| Discord |js})}
              text={React.string(
                "Connect with the community and participate in weekly challenges",
              )}
              href="https://bit.ly/CodaDiscord"
            />
            <ActionButton
              icon={React.string({js|💬|js})}
              heading={React.string({js| Forum |js})}
              text={React.string(
                "Find longer discussions and in-depth content",
              )}
              href="https://forums.codaprotocol.com/"
            />
            <ActionButton
              icon={React.string({js| 📬 |js})}
              heading={React.string({js| Newsletter |js})}
              text={React.string(
                "Sign up for the Testnet newsletter to get weekly updates",
              )}
              href="https://docs.google.com/forms/d/e/1FAIpQLScQRGW0-xGattPmr5oT-yRb9aCkPE6yIKXSfw1LRmNx1oh6AA/viewform"
            />
          </div>
        </div>
        <hr />
        <Section name="Leaderboard">
          <div className=Styles.dashboardHeader>
            <h1 className=Theme.H1.hero>
              {React.string("Testnet Leaderboard")}
            </h1>
            <a
              href="https://docs.google.com/spreadsheets/d/1CLX9DF7oFDWb1UiimQXgh_J6jO4fVLJEcEnPVAOfq24/edit#gid=0"
              target="_blank"
              className=Styles.headerLink>
              {React.string({j|View Full Leaderboard\u00A0→|j})}
            </a>
          </div>
          <div className=Styles.content>
            <div className=Styles.leaderboardContainer>
              <div id="testnet-leaderboard" className=Styles.leaderboard>
                <div className=Styles.headerRow>
                  <span> {React.string("#")} </span>
                  <span> {React.string("Username")} </span>
                  <span id="leaderboard-current-week" />
                  <span> {React.string("Total")} </span>
                </div>
                <hr />
                <div id="leaderboard-loading">
                  {React.string("Loading...")}
                </div>
              </div>
            </div>
            <div className=Styles.copy>
              <p>
                <h4 className=Styles.sidebarHeader>
                  {React.string("Testnet Points")}
                </h4>
              </p>
              <p className=Styles.markdownStyles>
                {React.string("The goal of Testnet Points")}
                <a href="#disclaimer"> {React.string("*")} </a>
                {React.string(
                   " is to recognize Coda community members who are actively involved in the network. There will be regular challenges to make it fun, interesting, and foster some friendly competition! Points can be won in several ways like being first to complete a challenge, contributing code to Coda, or being an excellent community member and helping others out.",
                 )}
              </p>
              <p>
                <h2 id="challenges-current-week" className=Styles.weekHeader />
              </p>
              <p>
                <div id="challenges-list" className=Styles.markdownStyles />
              </p>
              <p id="disclaimer" className=Css.(style([fontStyle(`italic)]))>
                {React.string(
                   "* Testnet Points are designed solely to track contributions to the Testnet and Testnet Points have no cash or other monetary value. Testnet Points and are not transferable and are not redeemable or exchangeable for any cryptocurrency or digital assets. We may at any time amend or eliminate Testnet Points.",
                 )}
              </p>
            </div>
          </div>
        </Section>
        <hr />
        <div>
          <div className=Styles.dashboardHeader>
            <h1 className=Theme.H1.hero>
              {React.string("Network Dashboard")}
            </h1>
            <a
              href="https://o1testnet.grafana.net/d/Rgo87HhWz/block-producer-dashboard?orgId=1"
              target="_blank"
              className=Styles.headerLink>
              {React.string({j|View Full Dashboard\u00A0→|j})}
            </a>
          </div>
          <iframe
            src="https://o1testnet.grafana.net/d-solo/PeI0mtKWk/live-dashboard-for-website?orgId=1&panelId=2"
            className=Styles.dashboard
          />
        </div>
      </div>
    </Wrapped>
  </Page>;
};
