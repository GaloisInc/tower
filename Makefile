SUBDIRS = tower tower-config tower-aadl tower-hal

default: $(SUBDIRS)

$(SUBDIRS):
	make -C $@ create-sandbox
	make -C $@
	make -C $@ test

.PHONY: $(SUBDIRS)
