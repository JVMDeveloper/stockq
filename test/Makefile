# StockX: test Makefile
#  - builds all files needed for testing, then runs tests

default: test clean

test: build
	@./scanner_test.sh
	@./parser_test.sh
	@./compiler_test.sh

build:
	@echo Building tests and necessary files.
	@cd ../compiler; make > /dev/null
	@cd scanner; make > /dev/null
	@cd parser; make > /dev/null

.PHONY: clean
clean:
	@echo Cleaning up all directories.
	@cd ../compiler; make clean > /dev/null
	@cd scanner; make clean > /dev/null
	@cd parser; make clean > /dev/null
