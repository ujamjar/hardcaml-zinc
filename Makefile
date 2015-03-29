########################################
# hardcaml-zinc - OCaml ZINC stack machine
#   implemented in HardCaml
#
#   (c) 2014 MicroJamJar Ltd
#
# Author(s): andy.ray@ujamjar.com
# Description: 
#
########################################

.PHONY: clean all install uninstall 

all: setup.data
	ocaml setup.ml -build

setup.ml: _oasis
	oasis setup

setup.data: setup.ml
	ocaml setup.ml -configure

####################################################

install: all
	ocaml setup.ml -install

uninstall: 
	ocamlfind remove hardcaml

clean:
	ocaml setup.ml -clean
	- find . -name "*~" | xargs rm

