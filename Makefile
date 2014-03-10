ALEX=alex
HAPPY=happy
GHC=ghc
MKDIR=mkdir

GHCFLAGS=-iobj -hidir=obj
PACKAGES=bytestring

PACKAGE_FLAGS=$(foreach p,$(PACKAGES),-package $p)

.PHONY: all
all: hshabit

.PHONY: clean
clean:
	rm -rf obj hshabit

hshabit: obj/Main.o obj/Syntax/Tokens.o obj/Syntax/Parser.o
	$(GHC) $(PACKAGE_FLAGS) -o $@ $^

obj/Main.o: obj/Syntax/Tokens.o obj/Syntax/Parser.o
obj/Syntax/Parser.o: obj/Syntax/Tokens.o

obj/%.hs: %.x
	$(MKDIR) -p $(dir $@)
	$(ALEX) -g -o $@ $^

obj/%.hs: %.y
	$(MKDIR) -p $(dir $@)
	$(HAPPY) -a -g -o $@ -iobj/habit.info $^

obj/%.o: %.hs
	$(MKDIR) -p $(dir $@)
	$(GHC) $(GHCFLAGS) -c -o $@ $<

obj/%.o: obj/%.hs
	$(MKDIR) -p $(dir $@)
	$(GHC) $(GHCFLAGS) -c -o $@ $<
