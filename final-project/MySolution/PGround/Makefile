
CC=gcc

all:: \
fact_dats
fact_dats: \
fact_dats.c ; $(CC) -o $@ $<
all:: \
fibo_dats
fibo_dats: \
fibo_dats.c ; $(CC) -o $@ $<

test:: fact_dats ; ./fact_dats
test:: fibo_dats ; ./fibo_dats

clean:: ; rm -f *~
clean:: ; rm -f *_?ats.o
cleanall:: clean
cleanall:: ; rm -f fact_dats fibo_dats
