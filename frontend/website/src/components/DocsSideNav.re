module Style = {
  open Css;
  let sideNav =
    style([
      minWidth(rem(15.)),
      listStyleType(`none),
      firstChild([marginLeft(`zero)]),
      media(
        Theme.MediaQuery.somewhatLarge,
        [
          marginRight(rem(2.)),
          marginTop(rem(2.)),
          position(`sticky),
          top(rem(2.5)),
        ],
      ),
    ]);

  let page =
    merge([
      Theme.Link.basic,
      style([
        display(`inlineBlock),
        marginBottom(`rem(0.5)),
        height(`rem(1.5)),
      ]),
    ]);

  let currentPage =
    merge([
      page,
      style([
        fontWeight(`bolder),
        position(`relative),
        before([
          position(`absolute),
          left(rem(-0.75)),
          contentRule(`text("\\2022 ")),
        ]),
      ]),
    ]);

  let navFolder =
    style([
      display(`block),
      flexDirection(`row),
      justifyContent(`spaceBetween),
      selector(
        "a",
        [
          marginBottom(`rem(0.5)),
          height(`rem(1.5)),
          cursor(`pointer),
          textDecoration(`none),
          Theme.Typeface.ibmplexsans,
          hover([color(Theme.Colors.hyperlinkHover)]),
        ],
      ),
    ]);
  let folderLabel =
    style([
      display(`flex),
      justifyContent(`spaceBetween),
      color(Theme.Colors.marine),
    ]);
  let childPage = style([marginLeft(`rem(1.)), listStyleType(`none)]);
  let flip = style([transform(rotate(`deg(180.)))]);
};

module CurrentSlugProvider = {
  let (context, make, makeProps) = ReactExt.createContext("");
};

module FolderSlugProvider = {
  let (context, make, makeProps) = ReactExt.createContext(None);
};

let slugConcat = (n1, n2) => {
  String.length(n2) > 0 ? n1 ++ "/" ++ n2 : n1;
};

module Page = {
  [@react.component]
  let make = (~title, ~slug) => {
    let currentSlug = React.useContext(CurrentSlugProvider.context);
    let folderSlug = React.useContext(FolderSlugProvider.context);
    let fullSlug =
      switch (folderSlug) {
      | Some(fs) => slugConcat(fs, slug)
      | None => slug
      };
    let isCurrentPage = currentSlug == fullSlug;
    let href = slugConcat("/docs", fullSlug);
    <li>
      <Next.Link href>
        <a className={isCurrentPage ? Style.currentPage : Style.page}>
          {React.string(title)}
        </a>
      </Next.Link>
    </li>;
  };
};

module Folder = {
  [@react.component]
  let make = (~title, ~slug, ~children) => {
    let currentSlug = React.useContext(CurrentSlugProvider.context);
    let hasCurrentSlug = ref(false);

    // Check if the children's props contain the current slug
    ReactExt.Children.forEach(children, (. child) => {
      switch (ReactExt.props(child)##slug) {
      | Some(childSlug) when slugConcat(slug, childSlug) == currentSlug =>
        hasCurrentSlug := true
      | _ => ()
      }
    });

    let (expanded, setExpanded) = React.useState(() => hasCurrentSlug^);

    let toggleExpanded =
      React.useCallback(e => {
        ReactEvent.Mouse.preventDefault(e);
        setExpanded(expanded => !expanded);
      });

    <li key=title className=Style.navFolder>
      <div>
        <a
          href="#"
          onClick=toggleExpanded
          ariaExpanded=expanded
          className=Style.folderLabel>
          {React.string(title)}
          <Spacer width=1.0 />
          <img
            src="/static/img/chevron-down.svg"
            width="16"
            height="16"
            className={expanded ? Style.flip : ""}
          />
        </a>
      </div>
      {!expanded
         ? React.null
         : <FolderSlugProvider value={Some(slug)}>
             <ul className=Style.childPage> children </ul>
           </FolderSlugProvider>}
    </li>;
  };
};

[@react.component]
let make = (~currentSlug) => {
  <aside>
    <CurrentSlugProvider value=currentSlug>
      <ul className=Style.sideNav>
        <Page title="Overview" slug="" />
        <Page title="Getting Started" slug="getting-started" />
        <Page title="My First Transaction" slug="my-first-transaction" />
        <Page title="Become a Node Operator" slug="node-operator" />
        <Page title="Contributing to Coda" slug="contributing" />
        <Folder title="Developers" slug="developers">
          <Page title="Developers Overview" slug="" />
          <Page title="Codebase Overview" slug="codebase-overview" />
          <Page title="Repository Structure" slug="directory-structure" />
          <Page title="Code Reviews" slug="code-reviews" />
          <Page title="Style Guide" slug="style-guide" />
          <Page title="GraphQL API" slug="graphql-api" />
        </Folder>
        <Folder title="Coda Protocol Architecture" slug="architecture">
          <Page title="Coda Overview" slug="" />
          <Page title="Lifecycle of a Payment" slug="lifecycle-payment" />
          <Page title="Consensus" slug="consensus" />
          <Page title="Proof of Stake" slug="proof-of-stake" />
          <Page title="Snark Workers" slug="snark-workers" />
        </Folder>
        <Folder title="SNARKs" slug="snarks">
          <Page title="SNARKs Overview" slug="" />
          <Page title="Getting started using SNARKs" slug="snarky" />
          <Page title="Which SNARK is right for me?" slug="constructions" />
          <Page title="The snarkyjs-crypto library" slug="snarkyjs-crypto" />
          <Page title="The snarky-universe library" slug="snarky-universe" />
        </Folder>
        <Page title="GUI Wallet" slug="gui-wallet" />
        <Page title="CLI Reference" slug="cli-reference" />
        <Page title="Troubleshooting" slug="troubleshooting" />
        <Page title="FAQ" slug="faq" />
        <Page title="Glossary" slug="glossary" />
      </ul>
    </CurrentSlugProvider>
  </aside>;
};
