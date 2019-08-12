let getEnvOrFail = name =>
  switch (Js_dict.get(Node.Process.process##env, name)) {
  | Some(value) => value
  | None => failwith({j|Couldn't find env var: `$name`"|j})
  };

let echoKey = getEnvOrFail("ECHO_PUBLICKEY");
let faucetKey = getEnvOrFail("FAUCET_PUBLICKEY");
let discordApiKey = getEnvOrFail("DISCORD_API_KEY");

let listeningChannels = ["faucet"];
let faucetApproveRole = "faucet-approvers";
let feeAmount = Int64.of_int(5);
let faucetAmount = Int64.of_int(100);
let cooldownTimeMs = 1000. *. 60. *. 60. *. 3.; // 3 hrs
