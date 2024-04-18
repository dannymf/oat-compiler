SUBMIT := $(shell cat submit_zip_contents.txt)
HWNAME := hw5
TIMESTAMP := $(shell /bin/date "+%Y-%m-%d-%H:%M:%S")
ZIPNAME := $(HWNAME)-submit-$(TIMESTAMP).zip

.PHONY: all oatc test clean zip

all: oatc

oatc: 
	dune build bin/main.exe
	@cp bin/main.exe oatc

printanalysis: 
	dune build bin/printanalysis.exe
	@cp bin/printanalysis.exe printanalysis

test: oatc
	./oatc --test

utop:
	dune utop

zip: $(SUBMIT)
	zip '$(ZIPNAME)' $(SUBMIT)

clean:
	dune clean
	rm -rf oatc ocamlbin bin/main.exe printanalysis bin/printanalysis.exe

# 
