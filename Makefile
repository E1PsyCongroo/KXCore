.DEFAULT_GOAL := build

help:
	@./mill -i superscalar.run --help

debug:
	@mkdir -p build
	@./mill -i superscalar.run -td build

build:
	@mkdir -p build
	@./mill -i superscalar.run -td build --firtool-option -disable-all-randomization --firtool-option -strip-debug-info

clean:
	-@rm -rf build

.PHONY: help build debug
