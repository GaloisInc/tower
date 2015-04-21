
default: tower tower-config tower-aadl tower-hal

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

.PHONY: tower-hal
tower-hal:
	make -C tower-hal create-sandbox
	make -C tower-hal
