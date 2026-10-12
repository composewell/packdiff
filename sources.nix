{nixpack}:
with nixpack.mkSources;
{
layers = [
{
  packdiff = local ./.;
  # parserOptionGroup, for help sections, is new in 0.19.
  optparse-applicative =
    hackage "0.19.0.0" "0cs8fqipakad38lvm75nz98hmvf881mgjhnc7icblxfzh92ay6kn"
      // { profiling = true; };
}
];
}
