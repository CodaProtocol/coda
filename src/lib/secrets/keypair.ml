module T = Keypair_read_write.Make (struct
  let env = "CODA_PRIVKEY_PASS"

  let which = "Coda keypair"
end)

include T
module Terminal_stdin = Keypair_common.Make_terminal_stdin (T)
