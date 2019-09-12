# GraphQL API

!!! warning
    - Coda APIs are still under construction, so these endpoints may change
    - By default, the GraphQL port is bound to localhost, but if you for some reason decide to expose it to the internet, be careful! Someone with access to this api can send coda from your wallets.

### Basic setup:

1. Start your daemon with the `-rest-port <port>` flag. For example: `coda daemon -rest-port 8000`
2. Connect your GraphQL client to `[localhost:<port>/graphql](http://localhost:8000/graphql)` (with `<port>` replaced with the port you used above.
3. [Optional] If you want to use an existing wallet with GraphQL, "import" it using the following: `coda daemon -propose-key keys/my-wallet -unsafe-track-propose-key`. This is a temporary workaround that removes password protection from the private key file. You can always create a new wallet using the `addWallet` mutation. 
4. Open your web browser to `http://localhost:<port>/graphql` to play with the api using [GraphiQL](https://github.com/graphql/graphiql)
5. Look at the <a href="/docs/graphql/" target="_blank">GraphQL schema docs</a>
