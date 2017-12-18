{ mkDerivation, archive, base, bytestring, io-streams, stdenv, unix
}:
mkDerivation {
  pname = "libarchive-streams";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base bytestring io-streams unix ];
  librarySystemDepends = [ archive ];
  homepage = "https://github.com/ttuegel/libarchive-streams#readme";
  description = "Read any archive format with libarchive and io-streams";
  license = stdenv.lib.licenses.asl20;
}
