{ writeShellApplication
, curl
, dev-md-format
, jq
, ...
}:

writeShellApplication {
  name = "dev-cross-post-hashnode";
  runtimeInputs = [ curl dev-md-format jq ];
  text = builtins.readFile ./script.sh;
}
