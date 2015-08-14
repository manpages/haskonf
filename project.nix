{ mkDerivation, base, directory, extensible-exceptions, filepath
, process, stdenv, time, unix
}:
mkDerivation {
  pname = "haskonf";
  version = "0.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    base directory extensible-exceptions filepath process time unix
  ];
  description = "Reflective configuration library";
  license = stdenv.lib.licenses.bsd2;
}
