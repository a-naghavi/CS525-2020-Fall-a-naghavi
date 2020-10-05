######
#
# target : dependency ; command
#
######

PATSCC=$(PATSHOME)/bin/patscc

######

OBJECTS :=
OBJECTS += \
lambda0_print_dats.o
OBJECTS += \
lambda0_interp_dats.o
OBJECTS += \
fcoin_change_dats.o

######

assign02_sol : \
assign02_sol.dats $(OBJECTS) ; \
patscc -o assign02_sol \
-DATS_MEMALLOC_LIBC assign02_sol.dats $(OBJECTS)

######

lambda0_print_dats.o : \
lambda0.sats lambda0_print.dats ; $(PATSCC) -o $@ -c lambda0_print.dats
lambda0_interp_dats.o : \
lambda0.sats lambda0_interp.dats ; $(PATSCC) -o $@ -c lambda0_interp.dats
fcoin_change_dats.o : \
fcoin_change.dats ; $(PATSCC) -o $@ -c fcoin_change.dats

######

clean:: ; rm -f *_?ats.c
clean:: ; rm -f *_?ats.o

######

cleanall:: clean
cleanall:: ; rm -f assign02_sol

###### end of [Makefile] ######
