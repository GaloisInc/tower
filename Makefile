
default:
	make -C tower create-sandbox
	make -C tower
	make -C tower-statemachine create-sandbox
	make -C tower-statemachine
	make -C tower-config create-sandbox
	make -C tower-config
	make -C tower-aadl install-deps
	make -C tower-aadl build
	make -C tower-aadl runtest

