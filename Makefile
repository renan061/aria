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

# Aria version.
V= 0.2

# Targets start here.
all: build

build:
	$(MKDIR) $(BIN)
	cd src && $(MAKE)

test:
	cd tests && $(MAKE)

clean:
	$(RM) -r ./$(BIN)
	$(RM) bison.output
	$(RM) *.bc
	$(RM) *.ll
	cd src && $(MAKE) $@
	cd tests && $(MAKE) $@
