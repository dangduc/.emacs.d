.DEFAULT_GOAL := default

ifeq ($(OS),Windows_NT)
	EMACS = /c/Program\ Files/Emacs/x86_64/bin/emacs
else
	ifeq ($(UNAME), Darwin)
	EMACS = /Applications/Emacs.app/Contents/MacOS/Emacs
	endif
endif

default: bootstrap-borg
	@ echo Calling git submodule update --init --recursive...
	@ git submodule update --init --recursive
	@ echo Calling git config --add diff.ignoreSubmodules dirty...
	@ git config --add diff.ignoreSubmodules dirty
	@ echo Calling make bootstrap to finish up.
	@ make bootstrap

DRONES_DIR = $(shell git config "borg.drones-directory" || echo "lib")

-include $(DRONES_DIR)/borg/borg.mk

# $ make bootstrap needs to be called after to set up autoloads and byte
# compilation.
bootstrap-borg:
		@ git submodule--helper clone --name borg --path $(DRONES_DIR)/borg \
		--url git@github.com:emacscollective/borg.git
		@ cd $(DRONES_DIR)/borg; git symbolic-ref HEAD refs/heads/master
		@ cd $(DRONES_DIR)/borg; git reset --hard HEAD

.PHONY: default
