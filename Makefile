document:
	Rscript -e "devtools::document()"

build:
	Rscript -e "devtools::build()"

check:
	Rscript -e "devtools::check()"

install:
	Rscript -e "devtools::install(build_vignettes = TRUE, upgrade_dependencies = FALSE)"

winbuild:
	Rscript -e "devtools::build_win(version = 'R-devel', quiet = TRUE)"

release:
	Rscript -e "devtools::release()"

pkgdown:
	Rscript -e "pkgdown::clean_site(); pkgdown::build_site()"
