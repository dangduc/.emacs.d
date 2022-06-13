.DEFAULT_GOAL := default

ifeq ($(OS),Windows_NT)
	EMACS = /c/Program\ Files/Emacs/x86_64/bin/emacs
else
	ifeq ($(UNAME), Darwin)
	EMACS = /Applications/Emacs.app/Contents/MacOS/Emacs
	endif
endif

default:
	@ echo Calling git submodule update --init --recursive...
	@ git submodule update --init --recursive
	@ echo Calling git config --add diff.ignoreSubmodules dirty...
	@ git config --add diff.ignoreSubmodules dirty

.PHONY: default
