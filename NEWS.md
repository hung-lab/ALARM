# ALARM 0.0.3

* Support parallel processing of ALARM predictions using the `mirai` package.

* Memoise the two functions that load the ALARM never-smoker and ever-smoker models. The package no longer loads the models when the package is loaded, which we no longer consider a good idea. The models will be loaded on the first calls (internally) to either function and will be cached in-memory for future calls.

# ALARM 0.0.2

* Enforce a hard dependency on `flexsurv` version 2.2.1 (#1).

# ALARM 0.0.1

* Initial public release.