
default: tower tower-config tower-aadl

.PHONY: tower
tower:
	make -C tower create-sandbox
	make -C tower

.PHONY: tower-config
tower-config:
	make -C tower-config create-sandbox
	make -C tower-config

.PHONY: tower-aadl
tower-aadl:
	make -C tower-aadl install-deps
	make -C tower-aadl build
	make -C tower-aadl runtest

