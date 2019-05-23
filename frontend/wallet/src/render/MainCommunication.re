open BsElectron;

include IpcRenderer.MakeIpcRenderer(Messages);

module CallTable = Messages.CallTable;

let callTable = CallTable.make();

let controlCodaDaemon = maybeArgs => {
  let pending =
    CallTable.nextPending(
      callTable,
      Messages.Typ.ControlCodaResponse,
      ~loc=__LOC__,
    );
  send(
    `Control_coda_daemon((
      maybeArgs,
      CallTable.Ident.Encode.t(pending.ident),
    )),
  );
  pending.task;
};

module ListenToken = {
  type t = messageCallback(Messages.mainToRendererMessages);
};

let listen = () => {
  let cb =
    (. _event, message: Messages.mainToRendererMessages) =>
      switch (message) {
      | `Respond_control_coda(ident, maybeErr) =>
        CallTable.resolve(
          callTable,
          CallTable.Ident.Decode.t(ident, Messages.Typ.ControlCodaResponse),
          maybeErr,
        )
      | `Deep_link(routeString) => ReasonReact.Router.push(routeString)
      };
  on(cb);
  cb;
};

let stopListening: ListenToken.t => unit = removeListener;
