include ../stack.mk

test: default
	stack exec -- test --src-dir=test_codegen

test-external: default
	stack exec -- test-external --src-dir=test_external_codegen

.PHONY: test-echronos
test-echronos: default
	stack exec -- test-echronos --src-dir=test_echronos_codegen --lib-dir=""
