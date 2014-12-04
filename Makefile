
default: tower tower-statemachine tower-config tower-aadl

tower:
	make -C tower create-sandbox
	make -C tower

tower-statemachine:
	make -C tower-statemachine create-sandbox
	make -C tower-statemachine

tower-config:
	make -C tower-config create-sandbox
	make -C tower-config

tower-aadl:
	make -C tower-aadl install-deps
	make -C tower-aadl build
	make -C tower-aadl runtest

