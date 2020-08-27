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
      gridTemplateColumns([`percent(12.), `auto, `percent(30.)]),
      gridColumnGap(`rem(1.5)),
      media(
        Theme.MediaQuery.notMobile,
        [gridTemplateColumns([`percent(10.), `auto, `percent(30.)])],
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
            gridTemplateColumns([`percent(11.), `auto, `percent(38.)]),
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

  let rank = {
    style([fontWeight(`normal), justifySelf(`flexEnd)]);
  };

  let points = {
    style([
      textAlign(`right),
      paddingRight(`rem(1.5)),
      media(Theme.MediaQuery.notMobile, [paddingRight(`rem(3.))]),
    ]);
  };

  let disclaimer = {
    style([
      display(`flex),
      justifyContent(`flexStart),
      fontSize(`rem(1.)),
      marginTop(`rem(1.)),
      marginBottom(`rem(5.)),
      media(Theme.MediaQuery.notMobile, [justifyContent(`flexEnd)]),
    ]);
  };
};

module Row = {
  [@react.component]
  let make = (~rank, ~name, ~points) => {
    <div className=Styles.tableRow>
      <span className=Styles.rank>
        {React.string(string_of_int(rank))}
      </span>
      <span> {React.string(name)} </span>
      <span className=Styles.points>
        {switch (points) {
         | Some(points) => React.string(string_of_int(points))
         | None => React.null
         }}
      </span>
    </div>;
  };
};

type challenge = {
  name: string,
  points: option(int),
};
type release = {
  name: string,
  challenges: array(challenge),
};

[@react.component]
let make = (~releaseTitle, ~challenges) => {
  let calculateTotalPoints = () => {
    challenges
    |> Array.fold_left(
         (totalPoints, challenge) => {
           switch (challenge.points) {
           | Some(points) => totalPoints + points
           | None => totalPoints
           }
         },
         0,
       )
    |> string_of_int;
  };
  let renderChallengePointsTable = () => {
    challenges
    |> Array.mapi((index, challenge) => {
         let {name, points} = challenge;
         <Row key={string_of_int(index)} rank={index + 1} name points />;
       })
    |> React.array;
  };

  <div className=Styles.container>
    <div className=Styles.releaseTitle> {React.string(releaseTitle)} </div>
    <div className=Styles.tableContainer>
      <div className=Styles.topRow>
        <span className=Css.(style([gridColumn(2, 3)]))>
          {React.string("Challenge Name")}
        </span>
        <span
          className=Css.(
            style([
              textAlign(`right),
              gridColumn(3, 4),
              paddingRight(`rem(3.5)),
            ])
          )>
          {React.string("Points *")}
        </span>
      </div>
      <div className=Styles.gridContainer>
        {renderChallengePointsTable()}
      </div>
      <div className=Styles.bottomRow>
        <span className=Css.(style([gridColumn(2, 3)]))>
          {React.string("Total Points *")}
        </span>
        <span className=Styles.points>
          {React.string(calculateTotalPoints())}
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
