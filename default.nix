{ emacsPackages, webkit }:
emacsPackages.trivialBuild {
  pname = "mu4e-webkit-preview";
  version = "0.1.0";
  src = ./mu4e-webkit-preview.el;
  packageRequires = [ emacsPackages.mu4e webkit ];
}
