{ mkDerivation, base, directory, stdenv, time }:
mkDerivation {
  pname = "haskonf";
  version = "0.0.1";
  src = ./.;
  buildDepends = [ base directory time ];
  description = "Reflective configuration library";
  license = stdenv.lib.licenses.bsd2;
}
