
CC=gcc

all:: \
f91_dats
f91_dats: \
f91_dats.c ; $(CC) -o $@ $<

test:: f91_dats ; ./f91_dats

clean:: ; rm -f *~
clean:: ; rm -f *_?ats.o
cleanall:: clean
cleanall:: ; rm -f f91_dats
