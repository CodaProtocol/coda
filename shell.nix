with import <nixpkgs> {};
mkShell {
  nativeBuildInputs = 
    [ opam pkgconfig
      m4 openssl gmp jemalloc libffi
      libsodium postgresql
      cargo zlib bzip2
    ];
  shellHook = ''
    eval $(opam env 2>/dev/null)
    '';
}
