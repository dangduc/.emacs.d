.DEFAULT_GOAL := default

default: bootstrap-borg
	@ echo Calling git submodule update --init --recursive...
	@ git submodule update --init --recursive
	@ echo Calling git config --add diff.ignoreSubmodules dirty...
	@ git config --add diff.ignoreSubmodules dirty
	@ echo Call make bootstrap to finish bootstrap.

DRONES_DIR = $(shell git config "borg.drones-directory" || echo "lib")

-include $(DRONES_DIR)/borg/borg.mk

bootstrap-borg:
		@git submodule--helper clone --name borg --path $(DRONES_DIR)/borg \
		--url git@github.com:emacscollective/borg.git
		@cd $(DRONES_DIR)/borg; git symbolic-ref HEAD refs/heads/master
		@cd $(DRONES_DIR)/borg; git reset --hard HEAD

.PHONY: default
