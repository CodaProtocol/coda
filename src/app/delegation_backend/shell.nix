with import <nixpkgs> {};
let
  minaSigner = import ../../external/c-reference-signer;
in
{
  devEnv = stdenv.mkDerivation {
    name = "dev";
    buildInputs = [ stdenv go_1_16 glibc minaSigner ];
    shellHook = ''
      export LIB_MINA_SIGNER=${minaSigner}/lib/libmina_signer.so
      return
      '';
  };
}
