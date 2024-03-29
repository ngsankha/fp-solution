UNAME := $(shell uname)

ifeq ($(UNAME), Darwin)
  format=macho64
else ifeq ($(UNAME), Linux)
  format=elf64
else
  format=win64
endif

%.run: %.o main.o char.o
	gcc main.o char.o $< -o $@

main.o: main.c types.h
	gcc -c main.c -o main.o

char.o: char.c types.h
	gcc -c char.c -o char.o

%.o: %.s
	nasm -f $(format) -o $@ $<

%.s: %.rkt
	racket -t compile-file.rkt -m $< > $@

compiler:
	raco make compile-file.rkt

clean:
	rm *.o *.s *.run
