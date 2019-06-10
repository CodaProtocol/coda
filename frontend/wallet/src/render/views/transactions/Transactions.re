open Tc;

module Styles = {
  open Css;

  let container = 
    style([
      height(`percent(100.)),
      borderLeft(`px(1), `solid, white),
    ]);

  let headerRow =
    merge([
      Theme.Text.Header.h6,
      style([
        display(`grid),
        gridTemplateColumns([`rem(16.), `fr(1.), `px(200)]),
        gridGap(Theme.Spacing.defaultSpacing),
        padding2(~v=`px(0), ~h=`rem(1.)),
        borderBottom(`px(1), `solid, Theme.Colors.savilleAlpha(0.1)),
        borderTop(`px(1), `solid, white),
        textTransform(`uppercase),
        height(`rem(2.)),
        alignItems(`center),
        color(Theme.Colors.slate),
        userSelect(`none),
      ]),
    ]);

  let alertContainer =
    style([
      display(`flex),
      height(`percent(100.)),
      alignItems(`center),
      justifyContent(`center),
    ]);

  let noTransactionsAlert = style([width(`px(348)), height(`px(80))]);

  let icon = style([opacity(0.5), height(`rem(1.5))]);
};

module TransactionsQueryString = [%graphql
  {|
    query transactions($after: String, $publicKey: String!) {
      blocks(first: 5, after: $after, filter: { relatedTo: $publicKey }) {
        nodes {
          creator @bsDecoder(fn: "Apollo.Decoders.publicKey")
          protocolState {
            blockchainState {
              date @bsDecoder(fn: "Apollo.Decoders.date")
            }
          }
          transactions {
            userCommands {
              to_: to @bsDecoder(fn: "Apollo.Decoders.publicKey")
              from @bsDecoder(fn: "Apollo.Decoders.publicKey")
              amount @bsDecoder(fn: "Apollo.Decoders.int64")
              fee @bsDecoder(fn: "Apollo.Decoders.int64")
              memo
              isDelegation
              date @bsDecoder(fn: "Apollo.Decoders.date")
            }
            feeTransfer {
              recipient @bsDecoder(fn: "Apollo.Decoders.publicKey")
              amount @bsDecoder(fn: "Apollo.Decoders.int64")
            }
            coinbase @bsDecoder(fn: "Apollo.Decoders.int64")
          }
        }
        pageInfo {
          hasNextPage
          lastCursor
        }
      }
    }
  |}
];
module TransactionsQuery = ReasonApollo.CreateQuery(TransactionsQueryString);

let extractTransactions: Js.t('a) => array(TransactionCell.Transaction.t) =
  data => {
    data##blocks##nodes
    |> Array.map(~f=block => {
         open TransactionCell.Transaction;
         let userCommands =
           block##transactions##userCommands
           |> Array.map(~f=userCommand => UserCommand(userCommand));
         let blockReward =
           BlockReward({
             date: block##protocolState##blockchainState##date,
             creator: block##creator,
             coinbase: block##transactions##coinbase,
             feeTransfers: block##transactions##feeTransfer,
           });
         Array.append(userCommands, [|blockReward|]);
       })
    |> Array.concatenate;
  };

[@react.component]
let make = () => {
  let activeWallet = Hooks.useActiveWallet();

  let updateQuery: ReasonApolloQuery.updateQueryT = [%bs.raw
    {| function (prevResult, { fetchMoreResult }) {
      const newBlocks = fetchMoreResult.blocks.nodes;
      const pageInfo = fetchMoreResult.blocks.pageInfo;
      return newBlocks.length > 0 ?
        {
          blocks: {
            __typename: "BlockConnection",
            nodes: [...prevResult.blocks.nodes, ...newBlocks],
            pageInfo,
          },
        } : prevResult
    }
    |}
  ];

  <div className=Styles.container>
    <div className=Styles.headerRow>
      <span className=Css.(style([display(`flex), alignItems(`center)]))>
        {React.string("Sender")}
        <span className=Styles.icon> <Icon kind=Icon.BentArrow /> </span>
        {React.string("recipient")}
      </span>
      <span> {ReasonReact.string("Memo")} </span>
      <span className=Css.(style([textAlign(`right)]))>
        {ReasonReact.string("Date / Amount")}
      </span>
    </div>
    {switch (activeWallet) {
     | Some(pubkey) =>
       let transactionQuery =
         TransactionsQueryString.make(
           ~publicKey=PublicKey.toString(pubkey),
           ~after="",
           (),
         );
       <TransactionsQuery variables=transactionQuery##variables>
         (
           response =>
             switch (response.result) {
             | Loading => <Loader.Page> <Loader /> </Loader.Page>
             | Error(err) => React.string(err##message) /* TODO format this error message */
             | Data(data) =>
               let transactions = extractTransactions(data);
               switch (Array.length(transactions)) {
               | 0 =>
                 <div className=Styles.alertContainer>
                   <Alert
                     kind=`Info
                     message="You don't have any coda in this wallet."
                   />
                 </div>
               | _ =>
                 <TransactionsList
                   transactions
                   onLoadMore={() => {
                     let moreTransactions =
                       TransactionsQueryString.make(
                         ~publicKey=PublicKey.toString(pubkey),
                         (),
                       );

                     response.fetchMore(
                       ~variables=moreTransactions##variables,
                       ~updateQuery,
                       (),
                     );
                   }}
                 />
               };
             }
         )
       </TransactionsQuery>;
     | None =>
       <div className=Styles.alertContainer>
         <Alert
           message="Select a wallet from the side bar to view related transactions."
           kind=`Info
         />
       </div>
     }}
  </div>;
};
