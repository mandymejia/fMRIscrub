## Test environments

* Windows x86_64-w64-mingw32/x64, R 4.2.2
* Mac x86_64-apple-darwin17.0, R 4.2.3

## R CMD check results

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

## Downstream dependencies

None.

## Tests

Passes all the tests in `tests/testthat.R`.

## Previous submission

0.14.3 Had issues where the `ICA_method` argument was not being sent to `pscrub_multi`; this is fixed. Some tests were not setting `ICA_method` to `"R"`, so this is fixed too.