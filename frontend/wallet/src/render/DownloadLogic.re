module Filestream = {
  type t;

  module Chunk = {
    type t = {. "length": int};
  };

  [@bs.module "fs"]
  external create: (string, {. "encoding": string}) => t =
    "createWriteStream";

  [@bs.send]
  external onError: (t, [@bs.as "error"] _, string => unit) => unit = "on";
  [@bs.send]
  external onFinish: (t, [@bs.as "finish"] _, unit => unit) => unit = "on";

  [@bs.send] external write: (t, Chunk.t) => unit = "";

  [@bs.send] external endStream: t => unit = "end";
};

module Https = {
  module Request = {
    type t;

    [@bs.send]
    external onError: (t, [@bs.as "error"] _, string => unit) => unit = "on";
  };

  module Response = {
    type t;

    module Headers = {
      [@bs.deriving abstract]
      type t =
        pri {
          location: string,
          [@bs.as "content-length"]
          contentLength: string,
        };
    };

    [@bs.get] external getStatusCode: t => int = "statusCode";

    [@bs.get] external getHeaders: t => Headers.t = "headers";

    [@bs.send] external setEncoding: (t, string) => unit = "";

    [@bs.send]
    external onData: (t, [@bs.as "data"] _, Filestream.Chunk.t => unit) => unit =
      "on";

    [@bs.send]
    external onEnd: (t, [@bs.as "end"] _, unit => unit) => unit = "on";
  };

  [@bs.module "https"]
  external get: (string, Response.t => unit) => Request.t = "";
};

let handleResponse = (response, filename, dataEncoding, chunkCb, doneCb) => {
  Https.Response.setEncoding(response, dataEncoding);
  let total =
    response
    |> Https.Response.getHeaders
    |> Https.Response.Headers.contentLengthGet
    |> int_of_string;
  let filestream = Filestream.create(filename, {"encoding": dataEncoding});

  Filestream.onFinish(filestream, () => doneCb(Belt.Result.Ok()));
  Filestream.onError(filestream, e => doneCb(Error(e)));

  Https.Response.onData(
    response,
    chunk => {
      Filestream.write(filestream, chunk);
      chunkCb(chunk##length, total);
    },
  );

  Https.Response.onEnd(response, () => Filestream.endStream(filestream));
};

let rec download = (filename, url, encoding, maxRedirects, chunkCb, doneCb) => {
  let request =
    Https.get(url, response =>
      switch (Https.Response.getStatusCode(response)) {
      | 301
      | 306
      | 307 =>
        if (maxRedirects <= 0) {
          let redirectUrl =
            response
            |> Https.Response.getHeaders
            |> Https.Response.Headers.locationGet;
          download(
            filename,
            redirectUrl,
            encoding,
            maxRedirects - 1,
            chunkCb,
            doneCb,
          );
        } else {
          doneCb(Belt.Result.Error("Too many redirects."));
        }
      | _ => handleResponse(response, filename, encoding, chunkCb, doneCb)
      }
    );
  Https.Request.onError(request, e => doneCb(Error(e)));
};
