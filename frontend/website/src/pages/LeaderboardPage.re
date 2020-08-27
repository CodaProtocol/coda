module Styles = {
  open Css;
  let page =
    style([
      maxWidth(`rem(58.0)),
      margin(`auto),
      media(Theme.MediaQuery.tablet, [maxWidth(`rem(89.))]),
    ]);
  let filters =
    style([
      display(`flex),
      flexDirection(`column),
      media(Theme.MediaQuery.notMobile, [flexDirection(`row)]),
      justifyContent(`spaceBetween),
      width(`percent(100.)),
      marginTop(`rem(2.)),
    ]);
  let searchBar =
    style([
      display(`flex),
      flexDirection(`column),
      marginTop(`rem(3.)),
      media(
        Theme.MediaQuery.notMobile,
        [marginTop(`zero), marginRight(`rem(1.)), width(`percent(48.))],
      ),
    ]);
  let textField =
    style([
      display(`inlineFlex),
      alignItems(`center),
      height(px(40)),
      borderRadius(px(4)),
      width(`percent(100.)),
      fontSize(rem(1.)),
      color(Theme.Colors.teal),
      padding(px(12)),
      marginTop(`rem(0.5)),
      border(px(1), `solid, Theme.Colors.hyperlinkAlpha(0.3)),
      active([
        outline(px(0), `solid, `transparent),
        padding(px(11)),
        borderWidth(px(2)),
        borderColor(Theme.Colors.hyperlinkAlpha(1.)),
      ]),
      focus([
        outline(px(0), `solid, `transparent),
        padding(px(11)),
        borderWidth(px(2)),
        borderColor(Theme.Colors.hyperlinkAlpha(1.)),
      ]),
      hover([borderColor(Theme.Colors.hyperlinkAlpha(1.))]),
      selector(
        "::placeholder",
        [
          fontSize(`px(12)),
          fontWeight(normal),
          color(Theme.Colors.slateAlpha(0.7)),
        ],
      ),
      media(
        Theme.MediaQuery.tablet,
        [
          width(`percent(100.)),
          maxWidth(`rem(38.5)),
          marginRight(`rem(1.)),
        ],
      ),
      media(Theme.MediaQuery.veryVeryLarge, [width(`percent(100.))]),
    ]);
};

module SearchBar = {
  [@react.component]
  let make = (~onUsernameEntered, ~username) => {
    <div className=Styles.searchBar>
      <span className=Theme.H5.semiBold>
        {React.string("Find Participant")}
      </span>
      <input
        type_="text"
        value=username
        placeholder="NAME"
        onChange={e => {
          let value = ReactEvent.Form.target(e)##value;
          onUsernameEntered(value);
        }}
        className=Styles.textField
      />
    </div>;
  };
};

module ToggleButtons = {
  module Styles = {
    open Css;

    let flexColumn =
      style([
        display(`none),
        media(
          Theme.MediaQuery.tablet,
          [
            display(`flex),
            flexDirection(`column),
            justifyContent(`center),
            height(`rem(4.5)),
          ],
        ),
      ]);

    let buttonRow =
      merge([
        style([
          display(`flex),
          width(`rem(40.5)),
          borderRadius(`px(4)),
          overflow(`hidden),
          selector(
            "*:not(:last-child)",
            [borderRight(`px(1), `solid, Theme.Colors.grey)],
          ),
        ]),
      ]);
  };

  [@react.component]
  let make = (~currentToggle, ~onTogglePress, ~toggleLabels) => {
    let renderToggleButtons = () => {
      toggleLabels
      |> Array.map(label => {
           <ToggleButton currentToggle onTogglePress label key=label />
         })
      |> React.array;
    };

    <div className=Styles.flexColumn>
      <h3 className=Theme.H5.semiBold> {React.string("View")} </h3>
      <Spacer height=0.5 />
      <div className=Styles.buttonRow> {renderToggleButtons()} </div>
    </div>;
  };
};

module FilterDropdown = {
  module Styles = {
    open Css;
    let flexColumn =
      style([
        display(`flex),
        flexDirection(`column),
        justifyContent(`center),
        height(`rem(4.5)),
        width(`percent(100.)),
        marginTop(`rem(2.0)),
        media(Theme.MediaQuery.tablet, [display(`none)]),
        media(
          Theme.MediaQuery.notMobile,
          [width(`percent(48.)), marginTop(`zero)],
        ),
      ]);
  };

  [@react.component]
  let make = (~currentFilter, ~onFilterPress, ~filterLabels) => {
    <div className=Styles.flexColumn>
      <h3 className=Theme.H5.semiBold> {React.string("View")} </h3>
      <Spacer height=0.5 />
      <Dropdown
        items=filterLabels
        currentItem=currentFilter
        onItemPress=onFilterPress
      />
    </div>;
  };
};

type state = {
  currentToggle: Leaderboard.Toggle.t,
  currentFilter: Leaderboard.Filter.t,
  username: string,
};

let initialState = {currentToggle: All, currentFilter: Release, username: ""};

type action =
  | Toggled(Leaderboard.Toggle.t)
  | Filtered(Leaderboard.Filter.t)
  | UsernameEntered(string);

let reducer = (prevState, action) => {
  switch (action) {
  | Toggled(toggle) => {...prevState, currentToggle: toggle}
  | Filtered(filter) => {...prevState, currentFilter: filter}
  | UsernameEntered(input) => {...prevState, username: input}
  };
};

[@react.component]
let make = () => {
  open Leaderboard.Toggle;
  open Leaderboard.Filter;
  let (state, dispatch) = React.useReducer(reducer, initialState);
  let onTogglePress = toggle => {
    toggle->toggle_of_string->Toggled->dispatch;
  };

  let onFilterPress = filter => {
    filter->filter_of_string->Filtered->dispatch;
  };

  let onUsernameEntered = username => {
    dispatch(UsernameEntered(username));
  };

  <Page title="Testnet Leaderboard">
    <Wrapped>
      <div className=Styles.page> <Summary /> </div>
      <div className=Styles.filters>
        <SearchBar onUsernameEntered username={state.username} />
        <ToggleButtons
          currentToggle={string_of_toggle(state.currentToggle)}
          onTogglePress
          toggleLabels={Array.map(
            toggle => {string_of_toggle(toggle)},
            toggles,
          )}
        />
        <FilterDropdown
          currentFilter={string_of_filter(state.currentFilter)}
          onFilterPress
          filterLabels={Array.map(
            filter => {string_of_filter(filter)},
            filters,
          )}
        />
      </div>
      <Spacer height=1.5 />
      <Leaderboard
        search={state.username}
        filter={state.currentFilter}
        toggle={state.currentToggle}
        onFilterPress
      />
    </Wrapped>
  </Page>;
};
