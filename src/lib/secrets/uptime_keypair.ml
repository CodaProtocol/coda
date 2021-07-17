(* uptime_keypair.ml -- keypair for uptime service *)

module T = Keypair_read_write.Make (struct
  let env = "UPTIME_PRIVKEY_PASS"

  let which = "Uptime service keypair"
end)

include T
module Terminal_stdin = Keypair_common.Make_terminal_stdin (T)
