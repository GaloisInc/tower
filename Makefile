
default:
	make -C tower create-sandbox
	make -C tower
	make -C tower-statemachine create-sandbox
	make -C tower-statemachine
	make -C tower-config create-sandbox
	make -C tower-config

