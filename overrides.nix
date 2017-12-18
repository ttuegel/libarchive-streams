pkgs: lib: self: super:

{
  progress = lib.dontHaddock super.progress;
}
