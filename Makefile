# TODO: Can this be moved to C99?
CFLAGS:=-Wall -Wextra -Wpedantic -std=c11 -glldb

SOURCE_FILES:=$(wildcard src/*.c)
HEADER_FILES:=$(wildcard src/*.h)

.PHONY: all clean
all: upsi

clean: 
	rm upsi

upsi: $(SOURCE_FILES) $(HEADER_FILES)
	cc $(CFLAGS) -Isrc/ -o $@ $(SOURCE_FILES)
