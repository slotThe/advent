CC := gcc intcode.c

FILES := day01 day02 day03 day04 day05 day06

$(FILES):
	$(CC) $@.c -o $@

all: $(FILES)

time: all
	time for f in $(FILES); do "./$$f">/dev/null; done
