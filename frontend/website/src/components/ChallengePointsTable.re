module Styles = {
  open Css;
  let text = {
    style([
      Theme.Typeface.ibmplexsans,
      color(`hex("344B65")),
      fontWeight(`num(600)),
      lineHeight(`rem(2.)),
      fontStyle(`normal),
      fontSize(`rem(1.125)),
      letterSpacing(`rem(-0.03)),
    ]);
  };

  let container = {
    merge([
      text,
      style([
        display(`flex),
        flexDirection(`column),
        flexWrap(`wrap),
        media(
          Theme.MediaQuery.notMobile,
          [
            width(`percent(100.)),
            flexDirection(`row),
            justifyContent(`spaceEvenly),
          ],
        ),
      ]),
    ]);
  };

  let releaseTitle = {
    style([
      fontWeight(`num(600)),
      fontSize(`rem(2.)),
      letterSpacing(`rem(-0.03)),
      display(`flex),
      width(`percent(100.)),
      justifyContent(`flexStart),
      paddingTop(`rem(2.)),
      media(
        Theme.MediaQuery.notMobile,
        [width(`percent(30.)), flexDirection(`row)],
      ),
      media(Theme.MediaQuery.tablet, [justifyContent(`center)]),
    ]);
  };

  let tableContainer = {
    style([
      width(`percent(100.)),
      media(
        Theme.MediaQuery.notMobile,
        [maxWidth(`rem(40.)), width(`percent(70.)), flexDirection(`row)],
      ),
    ]);
  };

  let gridContainer = {
    style([
      marginTop(`rem(1.)),
      background(`hex("F5F5F5")),
      borderTop(`px(1), `solid, Css_Colors.black),
      borderBottom(`px(1), `solid, Css_Colors.black),
      selector(
        "div:nth-child(even)",
        [background(`rgba((172, 151, 96, 0.06)))],
      ),
      media(Theme.MediaQuery.notMobile, [marginTop(`zero)]),
    ]);
  };

  let tableRow = {
    style([
      padding2(~v=`zero, ~h=`rem(1.)),
      padding(`px(8)),
      display(`grid),
      gridTemplateColumns([
        `percent(6.),
        `percent(6.),
        `auto,
        `percent(15.),
      ]),
      media(
        Theme.MediaQuery.notMobile,
        [
          gridTemplateColumns([
            `percent(12.5),
            `percent(5.),
            `auto,
            `percent(20.),
          ]),
        ],
      ),
    ]);
  };

  let topRow = {
    merge([
      tableRow,
      style([
        display(`none),
        letterSpacing(`px(2)),
        fontSize(`rem(0.875)),
        textTransform(`uppercase),
        padding(`zero),
        media(
          Theme.MediaQuery.notMobile,
          [
            display(`grid),
            gridTemplateColumns([
              `percent(12.5),
              `percent(5.),
              `auto,
              `percent(23.5),
            ]),
          ],
        ),
      ]),
    ]);
  };

  let bottomRow = {
    merge([
      tableRow,
      style([marginTop(`rem(0.5)), textTransform(`uppercase)]),
    ]);
  };

  let challengeLabel = {
    style([gridColumn(3, 4)]);
  };

  let pointsLabel = {
    style([gridColumn(4, 5)]);
  };

  let totalPointsTextLabel = {
    style([gridColumn(3, 4)]);
  };

  let totalPointsLabel = {
    style([gridColumn(4, 5)]);
  };

  let star = {
    style([justifySelf(`flexEnd)]);
  };

  let rank = {
    style([fontWeight(`normal), justifySelf(`center)]);
  };

  let disclaimer = {
    style([
      display(`flex),
      justifyContent(`flexStart),
      fontSize(`rem(1.)),
      marginTop(`rem(1.)),
      media(Theme.MediaQuery.notMobile, [justifyContent(`flexEnd)]),
    ]);
  };
};

module Row = {
  [@react.component]
  let make = (~star, ~rank, ~name, ~points) => {
    <div className=Styles.tableRow>
      <span className=Styles.star>
        {switch (star) {
         | Some(star) => React.string(star)
         | None => React.null
         }}
      </span>
      <span className=Styles.rank>
        {React.string(string_of_int(rank))}
      </span>
      <span> {React.string(name)} </span>
      <span>
        {switch (points) {
         | Some(points) => React.string(string_of_int(points))
         | None => React.null
         }}
      </span>
    </div>;
  };
};

[@react.component]
let make = (~releaseInfo, ~challengeInfo) => {
  let renderChallengePointsTable = () => {
    challengeInfo
    |> Array.mapi((index, challenge) => {
         let (star, name, points) = challenge;
         <Row star rank={index + 1} name points />;
       })
    |> React.array;
  };

  <div className=Styles.container>
    <div className=Styles.releaseTitle> {React.string(releaseInfo)} </div>
    <div className=Styles.tableContainer>
      <div className=Styles.topRow>
        <span className=Styles.challengeLabel>
          {React.string("Challenge Name")}
        </span>
        <span className=Styles.pointsLabel> {React.string("* Points")} </span>
      </div>
      <div className=Styles.gridContainer>
        {renderChallengePointsTable()}
      </div>
      <div className=Styles.bottomRow>
        <span className=Styles.totalPointsTextLabel>
          {React.string("Total Points *")}
        </span>
        <span className=Styles.totalPointsLabel>
          {React.string("5000")}
        </span>
      </div>
      <div>
        <span className=Styles.disclaimer>
          {React.string("** Scores are updated manually every few days")}
        </span>
      </div>
    </div>
  </div>;
};
