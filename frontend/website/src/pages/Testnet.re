module Styles = {
  open Css;

  let markdownStyles =
    style([
      selector("a", [cursor(`pointer), ...Theme.Link.basicStyles]),
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
    gridTemplateColumns([rem(1.), rem(5.5), rem(5.5), rem(3.5)]),
    media(
      Theme.MediaQuery.notMobile,
      [
        width(`percent(100.)),
        gridTemplateColumns([rem(2.5), `auto, rem(6.), rem(3.5)]),
      ],
    ),
  ];

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

  let dashboardHeader =
    merge([
      header,
      style([marginTop(rem(1.5)), marginBottom(rem(2.25))]),
    ]);

  let dashboard =
    style([
      width(`percent(100.)),
      height(`rem(70.)),
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

  let gradientSectionExpanded =
    style([
      height(`auto),
      width(`percent(100.)),
      position(`relative),
      overflow(`hidden),
      display(`flex),
      flexWrap(`wrap),
      marginLeft(`auto),
      marginRight(`auto),
      justifyContent(`center),
    ]);

  let gradientSection =
    merge([
      gradientSectionExpanded,
      style([
        height(`rem(45.)),
        after([
          contentRule(`none),
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
};

module Section = {
  [@react.component]
  let make = (~name, ~expanded, ~setExpanded, ~children) => {
    <div className=Css.(style([display(`flex), flexDirection(`column)]))>
      {if (expanded) {
         <div className=Styles.gradientSectionExpanded> children </div>;
       } else {
         <>
           <div className=Styles.gradientSection> children </div>
           <span
             className=Styles.expandButton
             onClick={_ => setExpanded(_ => true)}>
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
           </span>
         </>;
       }}
    </div>;
  };
};

[@react.component]
let make = (~challenges as _, ~testnetName as _) => {
  let (expanded, setExpanded) = React.useState(() => false);
  <Page title="Coda Testnet">
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
            <p className=Theme.Body.basic>
              {React.string("Testnet Status: ")}
              <StatusBadge service=`Network />
            </p>
          </div>
          <Terminal.Wrapper lineDelay=2000>
            <Terminal.Line prompt=">" value="coda daemon -peer ..." />
            <Terminal.Progress />
            <Terminal.MultiLine
              values=[|"Daemon ready. Clients can now connect!"|]
            />
            <Terminal.Line prompt=">" value="coda client status" />
            <Terminal.MultiLine
              values=[|
                "Max observed block length: 120",
                "Peers: 23",
                "Consensus time now: epoch=1, slot=13",
                "Sync status: Synced",
              |]
            />
          </Terminal.Wrapper>
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
              icon={React.string({js| 🌟 |js})}
              heading={React.string({js| Token Grant |js})}
              text={React.string(
                "Apply to be one of the early members to receive a Genesis token grant",
              )}
              href="/genesis"
            />
          </div>
        </div>
        <hr />
        <Section name="Leaderboard" expanded setExpanded>
          <div className=Styles.dashboardHeader>
            <h1 className=Theme.H1.hero>
              {React.string("Testnet Leaderboard")}
            </h1>
            // href="https://testnet-points-frontend-dot-o1labs-192920.appspot.com/"
            <a
              href="http://bit.ly/TestnetBetaLeaderboard"
              target="_blank"
              className=Styles.headerLink>
              {React.string({j|View Full Leaderboard\u00A0→|j})}
            </a>
          </div>
          <div className=Styles.content>
            <Leaderboard />
            <div className=Styles.copy>
              <h4 className=Styles.sidebarHeader>
                {React.string("Testnet Points")}
              </h4>
              <p className=Styles.markdownStyles>
                {React.string("The goal of Testnet Points")}
                <a href="#disclaimer" onClick={_ => setExpanded(_ => true)}>
                  {React.string("*")}
                </a>
                {React.string(
                   " is to recognize Coda community members who are actively involved in the network. There will be regular challenges to make it fun, interesting, and foster some friendly competition! Points can be won in several ways like being first to complete a challenge, contributing code to Coda, or being an excellent community member and helping others out.",
                 )}
              </p>
              // <Challenges challenges testnetName />
              // Temporarily hardcode the following message instead of the "Challenge" component
              <h4> {React.string("Genesis Token Program")} </h4>
              <p className=Styles.markdownStyles>
                {React.string(
                   "By completing challenges on testnet, you're preparing to become the first block producers upon mainnet launch. You're demonstrating that you have the skills and know-how to operate the Coda Protocol, the main purpose of ",
                 )}
                <a href="http://codaprotocol.com/genesis">
                  {React.string("Genesis")}
                </a>
                {React.string(".")}
              </p>
              <h4> {React.string("Testnet Challenges")} </h4>
              <p className=Styles.markdownStyles>
                {React.string(
                   "Learn how to operate the protocol, while contributing to Coda's network resilience. There are different ways for everyone to be involved. There are three categories of testnet challenges to earn testnet points",
                 )}
                <a href="#disclaimer" onClick={_ => setExpanded(_ => true)}>
                  {React.string("*")}
                </a>
                {React.string(".")}
              </p>
              <ul className=Styles.markdownStyles>
                <li>
                  {React.string("Entry level challenges (up to 1000 pts")}
                  <a href="#disclaimer" onClick={_ => setExpanded(_ => true)}>
                    {React.string("*")}
                  </a>
                  {React.string(") per challenge.")}
                </li>
                <li>
                  {React.string(
                     "Challenges for people who want to try out more features of the succinct blockchain (up to 4000 pts",
                   )}
                  <a href="#disclaimer" onClick={_ => setExpanded(_ => true)}>
                    {React.string("*")}
                  </a>
                  {React.string(") per challenge.")}
                </li>
                <li>
                  {React.string(
                     "Community challenges which require no technical skills (win Community MVP and up to 4000 pts",
                   )}
                  <a href="#disclaimer" onClick={_ => setExpanded(_ => true)}>
                    {React.string("*")}
                  </a>
                  {React.string(") per challenge.")}
                </li>
              </ul>
              <p className=Styles.markdownStyles>
                {React.string("Check out all challenges ")}
                <a
                  href="https://forums.codaprotocol.com/t/testnet-beta-release-3-1-challenges/271">
                  {React.string(" here ")}
                </a>
                {React.string("and join ")}
                <a href="http://bit.ly/CodaDiscord">
                  {React.string("http://bit.ly/CodaDiscord")}
                </a>
                {React.string(" for the latest updates!")}
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
            src="https://o1testnet.grafana.net/d/qx4y6dfWz/network-overview?orgId=1&refresh=1m"
            className=Styles.dashboard
          />
        </div>
      </div>
    </Wrapped>
  </Page>;
};

Next.injectGetInitialProps(make, _ =>
  Challenges.fetchAllChallenges()
  |> Promise.map(((testnetName, ranking, continuous, threshold)) =>
       {
         "challenges": (ranking, continuous, threshold),
         "testnetName": testnetName,
       }
     )
);
