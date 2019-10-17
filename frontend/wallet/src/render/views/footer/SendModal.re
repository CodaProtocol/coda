open Tc;

module Styles = {
  open Css;

  let contentContainer =
    style([
      display(`flex),
      width(`percent(100.)),
      flexDirection(`column),
      alignItems(`center),
      justifyContent(`center),
    ]);
};

module SendPayment = [%graphql
  {|
    mutation (
      $from: PublicKey!,
      $to_: PublicKey!,
      $amount: UInt64!,
      $fee: UInt64!,
      $memo: String) {
      sendPayment(input:
                    {from: $from, to: $to_, amount: $amount, fee: $fee, memo: $memo}) {
        payment {
          nonce
        }
      }
    }
  |}
];

module SendPaymentMutation = ReasonApollo.CreateMutation(SendPayment);

module ModalState = {
  module Validated = {
    type t = {
      from: PublicKey.t,
      to_: PublicKey.t,
      amount: string,
      fee: string,
      memoOpt: option(string),
    };
  };
  module Unvalidated = {
    type t = {
      fromStr: option(string),
      toStr: string,
      amountStr: string,
      feeStr: string,
      memoOpt: option(string),
      errorOpt: option(string),
    };
  };
};

let emptyModal: option(PublicKey.t) => ModalState.Unvalidated.t =
  activeWallet => {
    fromStr: Option.map(~f=PublicKey.toString, activeWallet),
    toStr: "",
    amountStr: "",
    feeStr: "",
    memoOpt: None,
    errorOpt: None,
  };

let validateInt64 = s =>
  switch (Int64.of_string(s)) {
  | i => i > Int64.zero
  | exception (Failure(_)) => false
  };

let validatePubkey = s =>
  switch (PublicKey.ofStringExn(s)) {
  | k => Some(k)
  | exception _ => None
  };

let validate:
  ModalState.Unvalidated.t => Belt.Result.t(ModalState.Validated.t, string) =
  state =>
    switch (state, validatePubkey(state.toStr)) {
    | ({fromStr: None}, _) => Error("Please specify a wallet to send from.")
    | ({toStr: ""}, _) => Error("Please specify a destination address.")
    | (_, None) => Error("Destination is invalid public key.")
    | ({amountStr}, _) when !validateInt64(amountStr) =>
      Error("Please specify a non-zero amount.")
    | ({feeStr}, _) when !validateInt64(feeStr) =>
      Error("Please specify a non-zero fee.")
    | ({fromStr: Some(fromPk), amountStr, feeStr, memoOpt}, Some(toPk)) =>
      Ok({
        from: PublicKey.ofStringExn(fromPk),
        to_: toPk,
        amount: amountStr,
        fee: feeStr,
        memoOpt,
      })
    };

module SendForm = {
  open ModalState.Unvalidated;

  [@react.component]
  let make = (~onSubmit, ~onClose) => {
    let activeWallet = Hooks.useActiveWallet();
    let (addressBook, _) = React.useContext(AddressBookProvider.context);
    let (sendState, setModalState) =
      React.useState(_ => emptyModal(activeWallet));
    let {fromStr, toStr, amountStr, feeStr, memoOpt, errorOpt} = sendState;
    let spacer = <Spacer height=0.5 />;
    let setError = e =>
      setModalState(s => {...s, ModalState.Unvalidated.errorOpt: Some(e)});
    <form
      className=Styles.contentContainer
      onSubmit={event => {
        ReactEvent.Form.preventDefault(event);
        switch (validate(sendState)) {
        | Error(e) => setError(e)
        | Ok(validated) =>
          onSubmit(
            validated,
            fun
            | Belt.Result.Error(e) => setError(e)
            | Ok() => onClose(),
          )
        };
      }}>
      {switch (errorOpt) {
       | None => React.null
       | Some(err) => <Alert kind=`Danger message=err />
       }}
      spacer
      // Disable dropdown, only show active Wallet
      <TextField
        label="From"
        value={WalletName.getName(
          Option.getExn(fromStr) |> PublicKey.ofStringExn,
          addressBook,
        )}
        disabled=true
        onChange={value => setModalState(s => {...s, fromStr: Some(value)})}
      />
      spacer
      <TextField
        label="To"
        mono=true
        onChange={value => setModalState(s => {...s, toStr: value})}
        value=toStr
        placeholder="Recipient Public Key"
      />
      spacer
      <TextField.Currency
        label="QTY"
        onChange={value => setModalState(s => {...s, amountStr: value})}
        value=amountStr
        placeholder="0"
      />
      spacer
      <TextField.Currency
        label="Fee"
        onChange={value => setModalState(s => {...s, feeStr: value})}
        value=feeStr
        placeholder="0"
      />
      spacer
      {switch (memoOpt) {
       | None =>
         <div className=Css.(style([alignSelf(`flexEnd)]))>
           <Link
             kind=Link.Blue
             onClick={_ => setModalState(s => {...s, memoOpt: Some("")})}>
             {React.string("+ Add memo")}
           </Link>
         </div>
       | Some(memoStr) =>
         <TextField
           label="Memo"
           onChange={value =>
             setModalState(s => {...s, memoOpt: Some(value)})
           }
           value=memoStr
           placeholder="Thanks!"
         />
       }}
      <Spacer height=1.0 />
      //Disable Modal button if no active wallet
      <div className=Css.(style([display(`flex)]))>
        <Button label="Cancel" style=Button.Gray onClick={_ => onClose()} />
        <Spacer width=1. />
        <Button label="Send" style=Button.Green type_="submit" />
      </div>
    </form>;
  };
};

[@react.component]
let make = (~onClose) => {
  <Modal title="Send Coda" onRequestClose={_ => onClose()}>
    <SendPaymentMutation>
      {(mutation, _) =>
         <SendForm
           onClose
           onSubmit={(
             {from, to_, amount, fee, memoOpt}: ModalState.Validated.t,
             afterSubmit,
           ) => {
             let variables =
               SendPayment.make(
                 ~from=Apollo.Encoders.publicKey(from),
                 ~to_=Apollo.Encoders.publicKey(to_),
                 ~amount=Js.Json.string(amount),
                 ~fee=Js.Json.string(fee),
                 ~memo=?memoOpt,
                 (),
               )##variables;
             let performMutation =
               Task.liftPromise(() =>
                 mutation(~variables, ~refetchQueries=[|"transactions"|], ())
               );
             Task.perform(
               performMutation,
               ~f=
                 fun
                 | Data(_)
                 | EmptyResponse => afterSubmit(Belt.Result.Ok())
                 | Errors(err) => {
                     /* TODO: Display more than first error? */
                     let message =
                       err
                       |> Array.get(~index=0)
                       |> Option.map(~f=e => e##message)
                       |> Option.withDefault(~default="Server error");
                     afterSubmit(Error(message));
                   },
             );
           }}
         />}
    </SendPaymentMutation>
  </Modal>;
};
