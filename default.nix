{ cabal, HTTP, aeson, amqp, blazeBuilder, errors, heist, mimeMail
, networkMetrics, optparseApplicative, postgresqlSimple, fynder
, doCheck ? false
}:
cabal.mkDerivation (self: {
  pname = "fynder-email";
  version = "1.0.0";
  src = ./.;
  buildDepends = [
    HTTP aeson amqp blazeBuilder errors heist mimeMail networkMetrics
    optparseApplicative postgresqlSimple fynder
  ];
  doCheck = false;
})