# StockQ: test Makefile
#  - builds all files needed for testing, then runs tests

default: test clean

test: scanner parser compiler

scanner: build
	@./scanner_test.sh

parser: build
	@./parser_test.sh

compiler: build
	@./compiler_test.sh

build:
	@echo Building tests and necessary files.
	@cd ../compiler; make > /dev/null

.PHONY: clean
clean:
	@echo Cleaning up all directories.
	@cd ../compiler; make clean > /dev/null
