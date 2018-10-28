document:
	Rscript -e "devtools::document()"

readme:
	Rscript -e "rmarkdown::render('README.Rmd')"

build:
	Rscript -e "devtools::build()"

check:
	Rscript -e "devtools::check()"

install:
	Rscript -e "devtools::install(build_vignettes = TRUE, dependencies = FALSE)"

winbuild:
	Rscript -e "devtools::check_win_devel(quiet = TRUE)"

pkgdown:
	Rscript -e "pkgdown::build_site()"

test:
	Rscript -e "devtools::test()"
