#
# Makefile for testing Aria
#

CLEAR= clear
MKDIR= mkdir -p
CP= cp
RM= rm -f

BIN= bin/

TEST= lua tester.lua

# Targets start here.
run: build
	$(CP) ../bin/aria bin/backendtest
	$(CLEAR)

	@- ./$(BIN)vectortest
	@- $(TEST) scanner $(BIN)scannertest
	@- $(TEST) parser $(BIN)parsertest
	@- $(TEST) ast $(BIN)asttest
	@- $(TEST) sem $(BIN)semtest
	@- $(TEST) backend "$(BIN)backendtest -r"
	@- sh test.sh backend -r

build: aria
	$(MKDIR) $(BIN)
	cd src && $(MAKE)

aria:
	cd .. && $(MAKE)

clean:
	$(RM) -r ./$(BIN)
	$(RM) *.bc
	$(RM) backend/*.bc
	cd src && $(MAKE) $@
