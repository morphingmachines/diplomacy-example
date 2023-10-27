# Replace 'adder' with your %PROJECT-NAME%
project = adder

# Toolchains and tools
MILL = ./mill


# Targets
rtl:## Generates Verilog code from Chisel sources (output to ./generated_sv_dir)
	$(MILL) $(project).runMain adder.diplomacyExample

check: test
.PHONY: test
test:## Run Chisel tests
	$(MILL) $(project).test.testOnly adder.DiplomaticAdder
	@echo "If using WriteVcdAnnotation in your tests, the VCD files are generated in ./test_run_dir/testname directories."

.PHONY: lint
lint: ## Formats code using scalafmt and scalafix
	$(MILL) $(project).fix
	$(MILL) $(project).reformat

.PHONY: test-lint
test-lint: ## Formats test code using scalafmt and scalafix
	$(MILL) $(project).fix
	$(MILL) $(project).reformat



.PHONY: console
console: ## Start a scala console within this project
	$(MILL) -i $(project).console

.PHONY: clean
clean:   ## Clean all generated files
	$(MILL) clean
	@rm -rf test_run_dir generated_sv_dir
	@rm -rf out

.PHONY: cleanall
cleanall: clean  ## Clean all downloaded dependencies and cache
	@rm -rf project/.bloop
	@rm -rf project/project
	@rm -rf project/target
	@rm -rf .bloop .bsp .metals .vscode

.PHONY: help
help:
	@echo "Makefile targets:"
	@echo ""
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = "[:##]"}; {printf "\033[36m%-20s\033[0m %s\n", $$1, $$4}'
	@echo ""

.DEFAULT_GOAL := help
