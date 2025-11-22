CC := gcc intcode.c
FILES := day01 day02 day03 day04 day05 day06 day07 day08 day09

PHONY: all clean time run

$(FILES):
	$(CC) $@.c -o $@

run: all
	for f in $(FILES); do printf "$$f\n"; "./$$f"; printf "\n"; done

all: $(FILES)

clean:
	rm $(FILES)
