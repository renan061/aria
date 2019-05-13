#
# Makefile for Aria
#

# == CHANGE THE SETTINGS BELOW TO SUIT YOUR ENVIRONMENT =======================

# Install.
BIN= bin/

# Utilities.
MKDIR= mkdir -p
RM= rm -f

# == END OF USER SETTINGS -- NO NEED TO CHANGE ANYTHING BELOW THIS LINE =======

# Tests.
TESTS= all scanner parser ast sem backend

# Aria version.
V= 0.2

# Targets start here.
main: build

build:
	$(MKDIR) $(BIN)
	cd src && $(MAKE)

test:
	cd tests && $(MAKE)

$(TESTS):
	cd tests && $(MAKE) $@

clean:
	$(RM) -r ./$(BIN)
	$(RM) bison.output
	$(RM) *.bc
	$(RM) *.ll
	cd src && $(MAKE) $@
	cd tests && $(MAKE) $@
