.DEFAULT_GOAL := build

help:
	@./mill -i superscalar.run --help

debug:
	@./mill -i superscalar.run -td build

build:
	@./mill -i superscalar.run -td build --firtool-option -disable-all-randomization --firtool-option -strip-debug-info

clean:
	-@rm -rf build

.PHONY: help build debug
