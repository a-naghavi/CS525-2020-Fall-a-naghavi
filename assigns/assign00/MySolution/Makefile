
assign00_sol: \
assign00_sol.dats ; \
patscc -o assign00_sol -DATS_MEMALLOC_LIBC assign00_sol.dats

datatype: \
datatype.dats ; \
patscc -o datatype -DATS_MEMALLOC_LIBC datatype.dats

clean:: ; rm -f *~
clean:: ; rm -f *_dats.c

cleanall:: clean
cleanall:: ; rm -f assign00_sol
