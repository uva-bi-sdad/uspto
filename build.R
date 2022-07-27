# rebuild
styler::style_pkg(filetype = c("R", "Rmd"))
spelling::spell_check_package()
devtools::document()
pkgdown::build_site(lazy = TRUE)
covr::report(covr::package_coverage(quiet = FALSE), "docs/coverage.html")

# check
devtools::check()
