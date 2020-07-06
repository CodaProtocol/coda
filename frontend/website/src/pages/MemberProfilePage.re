module Styles = {
  open Css;
  let page =
    style([
      maxWidth(`rem(58.0)),
      margin(`auto),
      media(Theme.MediaQuery.tablet, [maxWidth(`rem(89.))]),
    ]);

  let border =
    selector(
      "> :not(:last-child)",
      [
        after([
          unsafe("content", ""),
          display(`flex),
          justifyContent(`center),
          marginLeft(`zero),
          marginRight(`zero),
          borderBottom(`px(1), `dashed, `rgb((200, 200, 200))),
          media(
            Theme.MediaQuery.desktop,
            [marginLeft(`percent(16.)), marginRight(`percent(7.))],
          ),
        ]),
      ],
    );

  let table =
    style([
      selector("> div", [marginTop(`rem(5.))]),
      media(Theme.MediaQuery.notMobile, [border]),
    ]);

  let loading =
    style([
      Theme.Typeface.ibmplexsans,
      padding(`rem(5.)),
      color(Theme.Colors.leaderboardMidnight),
      textAlign(`center),
    ]);
};

/* Adds the remaining length to the array parameter.
   This is done because the Google Sheets API truncates trailing empty cells.
   */
let normalizeGoogleSheets = (length, a) => {
  let rowLength = Array.length(a);
  if (rowLength < length) {
    Array.append(a, ArrayLabels.make(length - rowLength, ""));
  } else {
    a;
  };
};

let fetchRelease = (username, release) => {
  let (releaseName, range, challengeColumnOffset) = release;
  Sheets.fetchRange(
    ~sheet="1Nq_Y76ALzSVJRhSFZZm4pfuGbPkZs2vTtCnVQ1ehujE",
    ~range,
  )
  |> Promise.map(res => {
       let rows = Array.map(Leaderboard.parseEntry, res);

       let numberOfChallenges =
         rows->Belt.Array.slice(~offset=0, ~len=1)->Array.get(0)
         |> Array.length;

       let challengeTitles =
         rows
         ->Belt.Array.slice(~offset=0, ~len=1)
         ->Array.get(0)
         ->Belt.Array.slice(
             ~offset=challengeColumnOffset,
             ~len=numberOfChallenges,
           );

       let userInfo =
         rows
         ->Belt.Array.keep(entry =>
             String.lowercase_ascii(entry[0])
             == String.lowercase_ascii(username)
           )
         ->Array.get(0)
         ->Belt.Array.slice(
             ~offset=challengeColumnOffset,
             ~len=numberOfChallenges,
           )
         |> normalizeGoogleSheets(Array.length(challengeTitles)); /* This is done so we have an equal number of point entries and challenges */

       let challengeInfo =
         userInfo
         |> Belt.Array.zip(challengeTitles)
         |> Array.map(user => {
              let (challengeTitle, challengePoints) = user;
              switch (challengePoints) {
              | "" => {
                  ChallengePointsTable.name: challengeTitle,
                  points: None,
                }
              | points => {
                  ChallengePointsTable.name: challengeTitle,
                  points: Some(int_of_string(points)),
                }
              };
            });

       Some({
         ChallengePointsTable.name: releaseName,
         challenges: challengeInfo,
       });
     })
  |> Js.Promise.catch(_ => Promise.return(None));
};

let fetchReleases = name => {
  [|
    ("Release 3.1", "3.1!B3:Z", 4), /* offset for challenge titles in 3.1 starts on the 4th column */
    ("Release 3.2a", "3.2a!B3:Z", 2), /* offset for challenge titles in 3.2a starts on the 2nd column */
    ("Release 3.2b", "3.2b!B3:Z", 2) /* offset for challenge titles in 3.2b starts on the 2nd column */
  |]
  |> Array.map(release => fetchRelease(name, release));
};

let parseMember = map => {
  let memberProperties = [|
    Js.Dict.get(map, "genesisMember"),
    Js.Dict.get(map, "phasePoints"),
    Js.Dict.get(map, "releasePoints"),
    Js.Dict.get(map, "allTimePoints"),
    Js.Dict.get(map, "allTimeRank"),
    Js.Dict.get(map, "phaseRank"),
    Js.Dict.get(map, "releaseRank"),
  |];

  /* Return None if a property is not present in the URL */
  if (Belt.Array.keep(memberProperties, property => {
        Belt.Option.isNone(property)
      })
      |> Array.length > 0) {
    None;
  } else {
    {
      Leaderboard.name: Js.Dict.get(map, "name")->Belt.Option.getExn,
      genesisMember:
        Js.Dict.get(map, "genesisMember")->Belt.Option.getExn
        |> bool_of_string,
      phasePoints:
        Js.Dict.get(map, "phasePoints")->Belt.Option.getExn |> int_of_string,
      releasePoints:
        Js.Dict.get(map, "releasePoints")->Belt.Option.getExn |> int_of_string,
      allTimePoints:
        Js.Dict.get(map, "allTimePoints")->Belt.Option.getExn |> int_of_string,
      allTimeRank:
        Js.Dict.get(map, "allTimeRank")->Belt.Option.getExn |> int_of_string,
      phaseRank:
        Js.Dict.get(map, "phaseRank")->Belt.Option.getExn |> int_of_string,
      releaseRank:
        Js.Dict.get(map, "releaseRank")->Belt.Option.getExn |> int_of_string,
    }
    ->Some;
  };
};

type state = {
  loading: bool,
  releases: array(ChallengePointsTable.release),
  currentMember: option(Leaderboard.member),
};

let initialState = {loading: true, releases: [||], currentMember: None};

type actions =
  | UpdateReleaseInfo(array(ChallengePointsTable.release))
  | UpdateCurrentUser(Leaderboard.member);

let reducer = (prevState, action) => {
  switch (action) {
  | UpdateReleaseInfo(releases) => {
      ...prevState,
      loading: false,
      releases: Belt.Array.concat(prevState.releases, releases),
    }
  | UpdateCurrentUser(member) => {...prevState, currentMember: Some(member)}
  };
};

[@react.component]
let make = () => {
  let (state, dispatch) = React.useReducer(reducer, initialState);
  let router = Next.Router.useRouter();

  React.useEffect1(
    () => {
      switch (parseMember(router.query)) {
      | Some(member) =>
        dispatch(UpdateCurrentUser(member));
        fetchReleases(member.name)
        |> Array.iter(e => {
             e
             |> Promise.iter(releaseInfo => {
                  switch (releaseInfo) {
                  | Some(releaseInfo) =>
                    dispatch(UpdateReleaseInfo([|releaseInfo|]))
                  | None => ()
                  }
                })
           });
      | None => ()
      };
      None;
    },
    [|router.query|],
  );

  <Page title="Member Profile">
    <Wrapped>
      <div className=Styles.page>
        {switch (state.currentMember) {
         | Some(member) => <div> <ProfileHero member /> </div>
         | None => React.null
         }}
        <div className=Styles.table>
          {state.releases
           |> Array.map((release: ChallengePointsTable.release) => {
                <div key={release.name}>
                  <ChallengePointsTable
                    releaseTitle={release.name}
                    challenges={release.challenges}
                  />
                </div>
              })
           |> React.array}
        </div>
        {state.loading
           ? <div className=Styles.loading>
               {React.string("Loading...")}
             </div>
           : React.null}
      </div>
    </Wrapped>
  </Page>;
};
