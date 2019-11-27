#
# Makefile for testing Aria
#

CLEAR= clear
MKDIR= mkdir -p
CP= cp
RM= rm -f

BIN= bin/

TEST= lua tester.lua

SIMPLE= scanner parser ast sem
TESTS= all $(SIMPLE) backend

# Targets start here.
all: build $(SIMPLE) backend
	@- ./$(BIN)vectortest

$(SIMPLE): build
	@- $(TEST) $@ $(BIN)$@test

backend: build
	@- $(CP) ../bin/aria bin/backendtest
	@- $(TEST) $@ "$(BIN)$@test -r"
	@- sh test.sh backend -r

build: aria
	$(MKDIR) $(BIN)
	cd src && $(MAKE)
	$(CLEAR)

aria:
	cd .. && $(MAKE)

clean:
	$(RM) -r ./$(BIN)
	$(RM) *.bc
	$(RM) backend/*.bc
	cd src && $(MAKE) $@
