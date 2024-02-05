PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGSRC  := $(shell basename `pwd`)

all: docs check 

docs:
	Rscript -e 'roxygen2::roxygenise(".")'

rd:
	Rscript -e 'library(methods); devtools::document()'

build:
	Rscript -e 'devtools::build()'
	# cd ..;\
	# R CMD build $(PKGSRC)

build2:
	cd ..;\
	R CMD build --no-build-vignettes $(PKGSRC)

install:
	cd ..;\
	R CMD INSTALL $(PKGNAME)_$(PKGVERS).tar.gz

check: 
	Rscript -e 'devtools::check()'
	# cd ..;\
	# R CMD check --as-cran $(PKGNAME)_$(PKGVERS).tar.gz

clean:
	cd ..;\
	$(RM) -r $(PKGNAME).Rcheck/


