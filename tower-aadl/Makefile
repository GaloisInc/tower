include ../stack.mk

test: default
	stack exec -- test --src-dir=test_codegen

.PHONY: test-echronos
test-echronos: default
	stack exec -- test-echronos --src-dir=test_echronos_codegen --lib-dir=""
