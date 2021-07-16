[%%import "/src/config.mlh"]

type t = Testnet | Mainnet

[%%if mainnet]

let t = Mainnet

[%%else]

let t = Testnet

[%%endif]
