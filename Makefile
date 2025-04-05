SHELL := /bin/bash
PKGNAME=simTool

help:
	-@ echo "R-cmd-check: Builds and checks (--as-cran) the Rpkg"
	-@ echo "test: Executes all unit-tests"
	-@ echo "coverage: Calculates the test coverage of the unit-test"
	-@ echo "lint: Starts linting"
	-@ echo "README: Builds README.md"
	-@ echo "pkgdown: Builds pkgdown site"

NAMESPACE: R/*
	Rscript -e "roxygen2::roxygenize()"

R-cmd-check: NAMESPACE
	Rscript -e "devtools::check(document=FALSE, args=c('--as-cran'))"
	# R CMD build .
	# R CMD check --as-cran --no-manual $(PKGNAME)*.tar.gz
	# make clean-pkg-build-file
	# make clean-cmd-check-files

clean-pkg-build-file:
	rm $(PKGNAME)*tar.gz

clean-cmd-check-files:
	rm -rf $(PKGNAME).Rcheck

test: NAMESPACE
	Rscript -e "pkgload::load_all(); tinytest::test_all()"

coverage: NAMESPACE
	Rscript -e "pkgload::load_all(); covr::package_coverage(type = 'tests')"

lint:
	Rscript -e "library(tinytest); pkgload::load_all(); lintr::lint_package()"

pkgdown: NAMESPACE
	Rscript -e "library(pkgdown); pkgdown::build_site()"

README:
	Rscript -e "pkgload::load_all(); rmarkdown::render(input='README.Rmd', output_format='md_document')"
