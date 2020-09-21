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
lambda0_fvset_dats.o
OBJECTS += \
lambda0_interp_dats.o

######

assign01_sol : \
assign01_sol.dats $(OBJECTS) ; \
patscc -o assign01_sol \
-DATS_MEMALLOC_LIBC assign01_sol.dats $(OBJECTS)

######

lambda0_print_dats.o : lambda0_print.dats ; $(PATSCC) -o $@ -c $<
lambda0_fvset_dats.o : lambda0_fvset.dats ; $(PATSCC) -o $@ -c $<
lambda0_interp_dats.o : lambda0_interp.dats ; $(PATSCC) -o $@ -c $<

######

clean:: ; rm -f *_?ats.c
clean:: ; rm -f *_?ats.o

######

cleanall:: clean
cleanall:: ; rm -f assign01_sol

###### end of [Makefile] ######
