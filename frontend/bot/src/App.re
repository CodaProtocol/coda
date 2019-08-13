open Discord;

let client = Client.createClient();

let handleMessageHelper = msg =>
  switch (Array.to_list(Js.String.split(",", Message.content(msg)))) {
  | ["$tiny"] => Message.reply(msg, Messages.greeting)
  | ["$help"] => Message.reply(msg, Messages.help)
  | ["$request", pk] => Faucet.handleMessage(msg, pk)
  | ["$request", ..._] => Message.reply(msg, Messages.requestError)
  | _ => ()
  };

let handleMessage = msg =>
  switch (TextChannel.fromChannel(Message.channel(msg))) {
  | None => ()
  | Some(channel) =>
    if (!(Message.author(msg) |> User.bot)
        && List.mem(TextChannel.name(channel), Constants.listeningChannels)) {
      handleMessageHelper(msg);
    }
  };

// Start echo service
Echo.start(Constants.echoKey);

Client.onReady(client, _ => print_endline("Bot is ready"));

Client.onMessage(client, handleMessage);

Client.login(client, Constants.discordApiKey);
