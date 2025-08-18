.DEFAULT_GOAL := build

help:
	@./mill -i superscalar.run --help

debug:
	@mkdir -p build
	@./mill -i superscalar.run -td build
	@find build -type f -name "*.sv" -exec sed -i '1i `define ENABLE_INITIAL_MEM_' {} \;
	@cp -r build ../myCPU

build:
	@mkdir -p build
	@./mill -i superscalar.run -td build --firtool-option -disable-all-randomization --firtool-option -strip-debug-info
	@find build -type f -name "*.sv" -exec sed -i '1i `define ENABLE_INITIAL_MEM_' {} \;
	@cp -r build ../myCPU

clean:
	-@rm -rf build

.PHONY: help build debug
