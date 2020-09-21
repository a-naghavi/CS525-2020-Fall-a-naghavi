/*
**
** The C code is generated by [ATS/Postiats-0-4-1]
** The starting compilation time is: 2020-9-21: 14h:24m
**
*/

/*
** include runtime header files
*/
#ifndef _ATS_CCOMP_HEADER_NONE_
#include "pats_ccomp_config.h"
#include "pats_ccomp_basics.h"
#include "pats_ccomp_typedefs.h"
#include "pats_ccomp_instrset.h"
#include "pats_ccomp_memalloc.h"
#ifndef _ATS_CCOMP_EXCEPTION_NONE_
#include "pats_ccomp_memalloca.h"
#include "pats_ccomp_exception.h"
#endif // end of [_ATS_CCOMP_EXCEPTION_NONE_]
#endif /* _ATS_CCOMP_HEADER_NONE_ */


/*
** include prelude cats files
*/
#ifndef _ATS_CCOMP_PRELUDE_NONE_
//
#include "prelude/CATS/basics.cats"
#include "prelude/CATS/integer.cats"
#include "prelude/CATS/pointer.cats"
#include "prelude/CATS/integer_long.cats"
#include "prelude/CATS/integer_size.cats"
#include "prelude/CATS/integer_short.cats"
#include "prelude/CATS/bool.cats"
#include "prelude/CATS/char.cats"
#include "prelude/CATS/float.cats"
#include "prelude/CATS/integer_ptr.cats"
#include "prelude/CATS/integer_fixed.cats"
#include "prelude/CATS/memory.cats"
#include "prelude/CATS/string.cats"
#include "prelude/CATS/strptr.cats"
//
#include "prelude/CATS/fprintf.cats"
//
#include "prelude/CATS/filebas.cats"
//
#include "prelude/CATS/list.cats"
#include "prelude/CATS/option.cats"
#include "prelude/CATS/array.cats"
#include "prelude/CATS/arrayptr.cats"
#include "prelude/CATS/arrayref.cats"
#include "prelude/CATS/matrix.cats"
#include "prelude/CATS/matrixptr.cats"
//
#endif /* _ATS_CCOMP_PRELUDE_NONE_ */
/*
** for user-supplied prelude
*/
#ifdef _ATS_CCOMP_PRELUDE_USER_
//
#include _ATS_CCOMP_PRELUDE_USER_
//
#endif /* _ATS_CCOMP_PRELUDE_USER_ */
/*
** for user2-supplied prelude
*/
#ifdef _ATS_CCOMP_PRELUDE_USER2_
//
#include _ATS_CCOMP_PRELUDE_USER2_
//
#endif /* _ATS_CCOMP_PRELUDE_USER2_ */

/*
staload-prologues(beg)
*/
/*
/home/amin/Apps/ATS2-Postiats-0.4.1/libats/libc/SATS/stdio.sats: 1390(line=36, offs=1) -- 1437(line=39, offs=3)
*/

#include \
"libats/libc/CATS/stdio.cats"
/*
/home/amin/Apps/ATS2-Postiats-0.4.1/libats/libc/SATS/sys/types.sats: 1390(line=36, offs=1) -- 1441(line=39, offs=3)
*/

#include \
"libats/libc/CATS/sys/types.cats"
/*
/home/amin/Apps/ATS2-Postiats-0.4.1/libats/libc/SATS/sys/stat.sats: 1390(line=36, offs=1) -- 1440(line=39, offs=3)
*/

#include \
"libats/libc/CATS/sys/stat.cats"
/*
/home/amin/Apps/ATS2-Postiats-0.4.1/libats/libc/SATS/sys/types.sats: 1390(line=36, offs=1) -- 1441(line=39, offs=3)
*/

#include \
"libats/libc/CATS/sys/types.cats"
/*
/home/amin/Apps/ATS2-Postiats-0.4.1/libats/libc/SATS/stdio.sats: 1390(line=36, offs=1) -- 1437(line=39, offs=3)
*/

#include \
"libats/libc/CATS/stdio.cats"
/*
/home/amin/Apps/ATS2-Postiats-0.4.1/libats/libc/SATS/sys/types.sats: 1390(line=36, offs=1) -- 1441(line=39, offs=3)
*/

#include \
"libats/libc/CATS/sys/types.cats"
/*
staload-prologues(end)
*/
/*
typedefs-for-tyrecs-and-tysums(beg)
*/
typedef
ATSstruct {
// #if(1)
int contag ;
// #endif
atstkind_type(atstype_ptrk) atslab__0 ;
} postiats_tysum_0 ;
typedef
ATSstruct {
// #if(1)
int contag ;
// #endif
atstkind_type(atstype_ptrk) atslab__0 ;
atstype_boxed atslab__1 ;
} postiats_tysum_1 ;
typedef
ATSstruct {
// #if(1)
int contag ;
// #endif
atstype_boxed atslab__0 ;
atstype_boxed atslab__1 ;
} postiats_tysum_2 ;
typedef
ATSstruct {
// #if(1)
int contag ;
// #endif
atstkind_type(atstype_ptrk) atslab__0 ;
atstype_boxed atslab__1 ;
atstype_boxed atslab__2 ;
} postiats_tysum_3 ;
/*
typedefs-for-tyrecs-and-tysums(end)
*/
/*
dynconlst-declaration(beg)
*/
/*
dynconlst-declaration(end)
*/
/*
dyncstlst-declaration(beg)
*/
ATSdyncst_mac(atspre_g0int_add_int)
ATSdyncst_extfun(_057_home_057_amin_057_Docs_057_Compiler_040_Design_057_CS525_055_2020_055_Fall_057_CS525_055_2020_055_Fall_055_a_055_naghavi_057_assigns_057_assign01_057_MySolution_057_lambda0_056_sats__t0erm_size, (atstype_boxed), atstkind_t0ype(atstype_int)) ;
ATSdyncst_mac(atspre_print_string)
ATSdyncst_mac(atspre_print_newline)
/*
dyncstlst-declaration(end)
*/
/*
dynvalist-implementation(beg)
*/
/*
dynvalist-implementation(end)
*/
/*
exnconlst-declaration(beg)
*/
#ifndef _ATS_CCOMP_EXCEPTION_NONE_
ATSextern()
atsvoid_t0ype
the_atsexncon_initize
(
  atstype_exnconptr d2c, atstype_string exnmsg
) ;
#endif // end of [_ATS_CCOMP_EXCEPTION_NONE_]
/*
exnconlst-declaration(end)
*/
/*
extypelst-declaration(beg)
*/
/*
extypelst-declaration(end)
*/
/*
assumelst-declaration(beg)
*/
#ifndef _ATS_CCOMP_ASSUME_CHECK_NONE_
#endif // #ifndef(_ATS_CCOMP_ASSUME_CHECK_NONE_)
/*
assumelst-declaration(end)
*/
ATSstatmpdec(statmp15, atstype_boxed) ;
ATSstatmpdec(statmp16, atstype_boxed) ;
ATSstatmpdec(statmp17, atstype_boxed) ;
ATSstatmpdec(statmp18, atstype_boxed) ;
ATSstatmpdec(statmp19, atstype_boxed) ;
ATSstatmpdec(statmp20, atstype_boxed) ;
ATSstatmpdec(statmp21, atstype_boxed) ;
ATSstatmpdec(statmp22, atstype_boxed) ;
ATSstatmpdec(statmp23, atstype_boxed) ;
ATSstatmpdec(statmp24, atstype_boxed) ;
ATSstatmpdec(statmp25, atstype_boxed) ;
ATSstatmpdec(statmp26, atstype_boxed) ;
ATSstatmpdec(statmp27, atstype_boxed) ;
#if(0)
ATSextern()
atstkind_t0ype(atstype_int)
_057_home_057_amin_057_Docs_057_Compiler_040_Design_057_CS525_055_2020_055_Fall_057_CS525_055_2020_055_Fall_055_a_055_naghavi_057_assigns_057_assign01_057_MySolution_057_lambda0_056_sats__t0erm_size(atstype_boxed) ;
#endif // end of [QUALIFIED]

#if(0)
ATSextern()
atsvoid_t0ype
mainats_0_void() ;
#endif // end of [QUALIFIED]

/*
/home/amin/Docs/Compiler Design/CS525-2020-Fall/CS525-2020-Fall-a-naghavi/assigns/assign01/MySolution/assign01_sol.dats: 363(line=21, offs=11) -- 617(line=33, offs=2)
*/
/*
local: 
global: t0erm_size$0$0(level=0)
local: 
global: 
*/
ATSextern()
atstkind_t0ype(atstype_int)
_057_home_057_amin_057_Docs_057_Compiler_040_Design_057_CS525_055_2020_055_Fall_057_CS525_055_2020_055_Fall_055_a_055_naghavi_057_assigns_057_assign01_057_MySolution_057_lambda0_056_sats__t0erm_size(atstype_boxed arg0)
{
/* tmpvardeclst(beg) */
ATStmpdec(tmpret0, atstkind_t0ype(atstype_int)) ;
ATStmpdec(tmp2, atstype_boxed) ;
ATStmpdec(tmp3, atstype_boxed) ;
ATStmpdec(tmp4, atstype_boxed) ;
ATStmpdec(tmp6, atstype_boxed) ;
ATStmpdec(tmp7, atstype_boxed) ;
ATStmpdec(tmp8, atstkind_t0ype(atstype_int)) ;
ATStmpdec(tmp9, atstkind_t0ype(atstype_int)) ;
ATStmpdec(tmp10, atstkind_t0ype(atstype_int)) ;
ATStmpdec(tmp11, atstkind_t0ype(atstype_int)) ;
ATStmpdec(tmp12, atstkind_t0ype(atstype_int)) ;
ATStmpdec(tmp13, atstkind_t0ype(atstype_int)) ;
ATStmpdec(tmp14, atstkind_t0ype(atstype_int)) ;
/* tmpvardeclst(end) */
ATSfunbody_beg()
/*
emit_instr: loc0 = /home/amin/Docs/Compiler Design/CS525-2020-Fall/CS525-2020-Fall-a-naghavi/assigns/assign01/MySolution/assign01_sol.dats: 353(line=21, offs=1) -- 617(line=33, offs=2)
*/
ATSINSflab(__patsflab_t0erm_size):
/*
emit_instr: loc0 = /home/amin/Docs/Compiler Design/CS525-2020-Fall/CS525-2020-Fall-a-naghavi/assigns/assign01/MySolution/assign01_sol.dats: 394(line=23, offs=1) -- 615(line=32, offs=38)
*/
ATScaseof_beg()
/*
** ibranchlst-beg
*/
ATSbranch_beg()
/*
emit_instr: loc0 = /home/amin/Docs/Compiler Design/CS525-2020-Fall/CS525-2020-Fall-a-naghavi/assigns/assign01/MySolution/assign01_sol.dats: 408(line=24, offs=3) -- 416(line=24, offs=11)
*/
ATSINSlab(__atstmplab0):
/*
emit_instr: loc0 = /home/amin/Docs/Compiler Design/CS525-2020-Fall/CS525-2020-Fall-a-naghavi/assigns/assign01/MySolution/assign01_sol.dats: 364(line=21, offs=12) -- 366(line=21, offs=14)
*/
ATSifnthen(ATSCKpat_con1(arg0, 0)) { ATSINSgoto(__atstmplab2) ; } ;
/*
emit_instr: loc0 = /home/amin/Docs/Compiler Design/CS525-2020-Fall/CS525-2020-Fall-a-naghavi/assigns/assign01/MySolution/assign01_sol.dats: 416(line=24, offs=11) -- 416(line=24, offs=11)
*/
ATSINSlab(__atstmplab1):
/*
emit_instr: loc0 = : 0(line=0, offs=0) -- 0(line=0, offs=0)
*/
/*
ibranch-mbody:
*/
/*
emit_instr: loc0 = /home/amin/Docs/Compiler Design/CS525-2020-Fall/CS525-2020-Fall-a-naghavi/assigns/assign01/MySolution/assign01_sol.dats: 420(line=24, offs=15) -- 421(line=24, offs=16)
*/
ATSINSmove(tmpret0, ATSPMVi0nt(1)) ;
ATSbranch_end()

ATSbranch_beg()
/*
emit_instr: loc0 = /home/amin/Docs/Compiler Design/CS525-2020-Fall/CS525-2020-Fall-a-naghavi/assigns/assign01/MySolution/assign01_sol.dats: 424(line=25, offs=3) -- 432(line=25, offs=11)
*/
ATSINSlab(__atstmplab2):
/*
emit_instr: loc0 = /home/amin/Docs/Compiler Design/CS525-2020-Fall/CS525-2020-Fall-a-naghavi/assigns/assign01/MySolution/assign01_sol.dats: 364(line=21, offs=12) -- 366(line=21, offs=14)
*/
ATSifnthen(ATSCKpat_con1(arg0, 1)) { ATSINSgoto(__atstmplab4) ; } ;
/*
emit_instr: loc0 = /home/amin/Docs/Compiler Design/CS525-2020-Fall/CS525-2020-Fall-a-naghavi/assigns/assign01/MySolution/assign01_sol.dats: 432(line=25, offs=11) -- 432(line=25, offs=11)
*/
ATSINSlab(__atstmplab3):
/*
emit_instr: loc0 = : 0(line=0, offs=0) -- 0(line=0, offs=0)
*/
/*
ibranch-mbody:
*/
/*
emit_instr: loc0 = /home/amin/Docs/Compiler Design/CS525-2020-Fall/CS525-2020-Fall-a-naghavi/assigns/assign01/MySolution/assign01_sol.dats: 436(line=25, offs=15) -- 437(line=25, offs=16)
*/
ATSINSmove(tmpret0, ATSPMVi0nt(1)) ;
ATSbranch_end()

ATSbranch_beg()
/*
emit_instr: loc0 = /home/amin/Docs/Compiler Design/CS525-2020-Fall/CS525-2020-Fall-a-naghavi/assigns/assign01/MySolution/assign01_sol.dats: 440(line=26, offs=3) -- 448(line=26, offs=11)
*/
ATSINSlab(__atstmplab4):
/*
emit_instr: loc0 = /home/amin/Docs/Compiler Design/CS525-2020-Fall/CS525-2020-Fall-a-naghavi/assigns/assign01/MySolution/assign01_sol.dats: 364(line=21, offs=12) -- 366(line=21, offs=14)
*/
ATSifnthen(ATSCKpat_con1(arg0, 4)) { ATSINSgoto(__atstmplab6) ; } ;
/*
emit_instr: loc0 = /home/amin/Docs/Compiler Design/CS525-2020-Fall/CS525-2020-Fall-a-naghavi/assigns/assign01/MySolution/assign01_sol.dats: 448(line=26, offs=11) -- 448(line=26, offs=11)
*/
ATSINSlab(__atstmplab5):
/*
emit_instr: loc0 = : 0(line=0, offs=0) -- 0(line=0, offs=0)
*/
/*
ibranch-mbody:
*/
/*
emit_instr: loc0 = /home/amin/Docs/Compiler Design/CS525-2020-Fall/CS525-2020-Fall-a-naghavi/assigns/assign01/MySolution/assign01_sol.dats: 452(line=26, offs=15) -- 453(line=26, offs=16)
*/
ATSINSmove(tmpret0, ATSPMVi0nt(1)) ;
ATSbranch_end()

ATSbranch_beg()
/*
emit_instr: loc0 = /home/amin/Docs/Compiler Design/CS525-2020-Fall/CS525-2020-Fall-a-naghavi/assigns/assign01/MySolution/assign01_sol.dats: 456(line=27, offs=3) -- 469(line=27, offs=16)
*/
ATSINSlab(__atstmplab6):
/*
emit_instr: loc0 = /home/amin/Docs/Compiler Design/CS525-2020-Fall/CS525-2020-Fall-a-naghavi/assigns/assign01/MySolution/assign01_sol.dats: 364(line=21, offs=12) -- 366(line=21, offs=14)
*/
ATSifnthen(ATSCKpat_con1(arg0, 2)) { ATSINSgoto(__atstmplab8) ; } ;
/*
emit_instr: loc0 = /home/amin/Docs/Compiler Design/CS525-2020-Fall/CS525-2020-Fall-a-naghavi/assigns/assign01/MySolution/assign01_sol.dats: 469(line=27, offs=16) -- 469(line=27, offs=16)
*/
ATSINSlab(__atstmplab7):
/*
emit_instr: loc0 = /home/amin/Docs/Compiler Design/CS525-2020-Fall/CS525-2020-Fall-a-naghavi/assigns/assign01/MySolution/assign01_sol.dats: 466(line=27, offs=13) -- 468(line=27, offs=15)
*/
ATSINSmove(tmp2, ATSSELcon(arg0, postiats_tysum_1, atslab__1)) ;
/*
emit_instr: loc0 = : 0(line=0, offs=0) -- 0(line=0, offs=0)
*/
/*
ibranch-mbody:
*/
/*
emit_instr: loc0 = /home/amin/Docs/Compiler Design/CS525-2020-Fall/CS525-2020-Fall-a-naghavi/assigns/assign01/MySolution/assign01_sol.dats: 479(line=28, offs=7) -- 492(line=28, offs=20)
*/
ATSINSmove(tmp8, _057_home_057_amin_057_Docs_057_Compiler_040_Design_057_CS525_055_2020_055_Fall_057_CS525_055_2020_055_Fall_055_a_055_naghavi_057_assigns_057_assign01_057_MySolution_057_lambda0_056_sats__t0erm_size(tmp2)) ;

/*
emit_instr: loc0 = /home/amin/Docs/Compiler Design/CS525-2020-Fall/CS525-2020-Fall-a-naghavi/assigns/assign01/MySolution/assign01_sol.dats: 475(line=28, offs=3) -- 492(line=28, offs=20)
*/
ATSINSmove(tmpret0, atspre_g0int_add_int(ATSPMVi0nt(1), tmp8)) ;

ATSbranch_end()

ATSbranch_beg()
/*
emit_instr: loc0 = /home/amin/Docs/Compiler Design/CS525-2020-Fall/CS525-2020-Fall-a-naghavi/assigns/assign01/MySolution/assign01_sol.dats: 496(line=29, offs=3) -- 510(line=29, offs=17)
*/
ATSINSlab(__atstmplab8):
/*
emit_instr: loc0 = /home/amin/Docs/Compiler Design/CS525-2020-Fall/CS525-2020-Fall-a-naghavi/assigns/assign01/MySolution/assign01_sol.dats: 364(line=21, offs=12) -- 366(line=21, offs=14)
*/
ATSifnthen(ATSCKpat_con1(arg0, 3)) { ATSINSgoto(__atstmplab10) ; } ;
/*
emit_instr: loc0 = /home/amin/Docs/Compiler Design/CS525-2020-Fall/CS525-2020-Fall-a-naghavi/assigns/assign01/MySolution/assign01_sol.dats: 510(line=29, offs=17) -- 510(line=29, offs=17)
*/
ATSINSlab(__atstmplab9):
/*
emit_instr: loc0 = /home/amin/Docs/Compiler Design/CS525-2020-Fall/CS525-2020-Fall-a-naghavi/assigns/assign01/MySolution/assign01_sol.dats: 503(line=29, offs=10) -- 505(line=29, offs=12)
*/
ATSINSmove(tmp3, ATSSELcon(arg0, postiats_tysum_2, atslab__0)) ;
/*
emit_instr: loc0 = /home/amin/Docs/Compiler Design/CS525-2020-Fall/CS525-2020-Fall-a-naghavi/assigns/assign01/MySolution/assign01_sol.dats: 507(line=29, offs=14) -- 509(line=29, offs=16)
*/
ATSINSmove(tmp4, ATSSELcon(arg0, postiats_tysum_2, atslab__1)) ;
/*
emit_instr: loc0 = : 0(line=0, offs=0) -- 0(line=0, offs=0)
*/
/*
ibranch-mbody:
*/
/*
emit_instr: loc0 = /home/amin/Docs/Compiler Design/CS525-2020-Fall/CS525-2020-Fall-a-naghavi/assigns/assign01/MySolution/assign01_sol.dats: 520(line=30, offs=7) -- 533(line=30, offs=20)
*/
ATSINSmove(tmp10, _057_home_057_amin_057_Docs_057_Compiler_040_Design_057_CS525_055_2020_055_Fall_057_CS525_055_2020_055_Fall_055_a_055_naghavi_057_assigns_057_assign01_057_MySolution_057_lambda0_056_sats__t0erm_size(tmp3)) ;

/*
emit_instr: loc0 = /home/amin/Docs/Compiler Design/CS525-2020-Fall/CS525-2020-Fall-a-naghavi/assigns/assign01/MySolution/assign01_sol.dats: 516(line=30, offs=3) -- 533(line=30, offs=20)
*/
ATSINSmove(tmp9, atspre_g0int_add_int(ATSPMVi0nt(1), tmp10)) ;

/*
emit_instr: loc0 = /home/amin/Docs/Compiler Design/CS525-2020-Fall/CS525-2020-Fall-a-naghavi/assigns/assign01/MySolution/assign01_sol.dats: 537(line=30, offs=24) -- 550(line=30, offs=37)
*/
ATSINSmove(tmp11, _057_home_057_amin_057_Docs_057_Compiler_040_Design_057_CS525_055_2020_055_Fall_057_CS525_055_2020_055_Fall_055_a_055_naghavi_057_assigns_057_assign01_057_MySolution_057_lambda0_056_sats__t0erm_size(tmp4)) ;

/*
emit_instr: loc0 = /home/amin/Docs/Compiler Design/CS525-2020-Fall/CS525-2020-Fall-a-naghavi/assigns/assign01/MySolution/assign01_sol.dats: 516(line=30, offs=3) -- 550(line=30, offs=37)
*/
ATSINSmove(tmpret0, atspre_g0int_add_int(tmp9, tmp11)) ;

ATSbranch_end()

ATSbranch_beg()
/*
emit_instr: loc0 = /home/amin/Docs/Compiler Design/CS525-2020-Fall/CS525-2020-Fall-a-naghavi/assigns/assign01/MySolution/assign01_sol.dats: 554(line=31, offs=3) -- 574(line=31, offs=23)
*/
ATSINSlab(__atstmplab10):
/*
emit_instr: loc0 = /home/amin/Docs/Compiler Design/CS525-2020-Fall/CS525-2020-Fall-a-naghavi/assigns/assign01/MySolution/assign01_sol.dats: 364(line=21, offs=12) -- 366(line=21, offs=14)
*/
#if(0)
ATSifnthen(ATSCKpat_con1(arg0, 5)) { ATSINSdeadcode_fail() ; } ;
#endif
/*
emit_instr: loc0 = /home/amin/Docs/Compiler Design/CS525-2020-Fall/CS525-2020-Fall-a-naghavi/assigns/assign01/MySolution/assign01_sol.dats: 574(line=31, offs=23) -- 574(line=31, offs=23)
*/
ATSINSlab(__atstmplab11):
/*
emit_instr: loc0 = /home/amin/Docs/Compiler Design/CS525-2020-Fall/CS525-2020-Fall-a-naghavi/assigns/assign01/MySolution/assign01_sol.dats: 567(line=31, offs=16) -- 569(line=31, offs=18)
*/
ATSINSmove(tmp6, ATSSELcon(arg0, postiats_tysum_3, atslab__1)) ;
/*
emit_instr: loc0 = /home/amin/Docs/Compiler Design/CS525-2020-Fall/CS525-2020-Fall-a-naghavi/assigns/assign01/MySolution/assign01_sol.dats: 571(line=31, offs=20) -- 573(line=31, offs=22)
*/
ATSINSmove(tmp7, ATSSELcon(arg0, postiats_tysum_3, atslab__2)) ;
/*
emit_instr: loc0 = : 0(line=0, offs=0) -- 0(line=0, offs=0)
*/
/*
ibranch-mbody:
*/
/*
emit_instr: loc0 = /home/amin/Docs/Compiler Design/CS525-2020-Fall/CS525-2020-Fall-a-naghavi/assigns/assign01/MySolution/assign01_sol.dats: 584(line=32, offs=7) -- 597(line=32, offs=20)
*/
ATSINSmove(tmp13, _057_home_057_amin_057_Docs_057_Compiler_040_Design_057_CS525_055_2020_055_Fall_057_CS525_055_2020_055_Fall_055_a_055_naghavi_057_assigns_057_assign01_057_MySolution_057_lambda0_056_sats__t0erm_size(tmp6)) ;

/*
emit_instr: loc0 = /home/amin/Docs/Compiler Design/CS525-2020-Fall/CS525-2020-Fall-a-naghavi/assigns/assign01/MySolution/assign01_sol.dats: 580(line=32, offs=3) -- 597(line=32, offs=20)
*/
ATSINSmove(tmp12, atspre_g0int_add_int(ATSPMVi0nt(1), tmp13)) ;

/*
emit_instr: loc0 = /home/amin/Docs/Compiler Design/CS525-2020-Fall/CS525-2020-Fall-a-naghavi/assigns/assign01/MySolution/assign01_sol.dats: 601(line=32, offs=24) -- 614(line=32, offs=37)
*/
ATSINSmove(tmp14, _057_home_057_amin_057_Docs_057_Compiler_040_Design_057_CS525_055_2020_055_Fall_057_CS525_055_2020_055_Fall_055_a_055_naghavi_057_assigns_057_assign01_057_MySolution_057_lambda0_056_sats__t0erm_size(tmp7)) ;

/*
emit_instr: loc0 = /home/amin/Docs/Compiler Design/CS525-2020-Fall/CS525-2020-Fall-a-naghavi/assigns/assign01/MySolution/assign01_sol.dats: 580(line=32, offs=3) -- 614(line=32, offs=37)
*/
ATSINSmove(tmpret0, atspre_g0int_add_int(tmp12, tmp14)) ;

ATSbranch_end()

/*
** ibranchlst-end
*/
ATScaseof_end()

ATSfunbody_end()
ATSreturn(tmpret0) ;
} /* end of [_057_home_057_amin_057_Docs_057_Compiler_040_Design_057_CS525_055_2020_055_Fall_057_CS525_055_2020_055_Fall_055_a_055_naghavi_057_assigns_057_assign01_057_MySolution_057_lambda0_056_sats__t0erm_size] */

/*
/home/amin/Docs/Compiler Design/CS525-2020-Fall/CS525-2020-Fall-a-naghavi/assigns/assign01/MySolution/assign01_sol.dats: 997(line=71, offs=6) -- 1053(line=75, offs=2)
*/
/*
local: 
global: mainats_0_void$2$0(level=0)
local: 
global: 
*/
ATSextern()
atsvoid_t0ype
mainats_0_void()
{
/* tmpvardeclst(beg) */
// ATStmpdec_void(tmpret28) ;
// ATStmpdec_void(tmp29) ;
/* tmpvardeclst(end) */
ATSfunbody_beg()
/*
emit_instr: loc0 = /home/amin/Docs/Compiler Design/CS525-2020-Fall/CS525-2020-Fall-a-naghavi/assigns/assign01/MySolution/assign01_sol.dats: 992(line=71, offs=1) -- 1053(line=75, offs=2)
*/
ATSINSflab(__patsflab_main_0_void):
/*
emit_instr: loc0 = /home/amin/Docs/Compiler Design/CS525-2020-Fall/CS525-2020-Fall-a-naghavi/assigns/assign01/MySolution/assign01_sol.dats: 1002(line=72, offs=1) -- 1053(line=75, offs=2)
*/
/*
letpush(beg)
*/
/*
letpush(end)
*/

/*
emit_instr: loc0 = /home/amin/Docs/Compiler Design/CS525-2020-Fall/CS525-2020-Fall-a-naghavi/assigns/assign01/MySolution/assign01_sol.dats: 1012(line=73, offs=9) -- 1050(line=73, offs=47)
*/
ATSINSmove_void(tmp29, atspre_print_string(ATSPMVstring("\n\n*** end of testing ***"))) ;

/*
emit_instr: loc0 = /home/amin/Docs/Compiler Design/CS525-2020-Fall/CS525-2020-Fall-a-naghavi/assigns/assign01/MySolution/assign01_sol.dats: 1012(line=73, offs=9) -- 1050(line=73, offs=47)
*/
ATSINSmove_void(tmpret28, atspre_print_newline()) ;

/*
emit_instr: loc0 = /home/amin/Docs/Compiler Design/CS525-2020-Fall/CS525-2020-Fall-a-naghavi/assigns/assign01/MySolution/assign01_sol.dats: 1002(line=72, offs=1) -- 1053(line=75, offs=2)
*/
/*
INSletpop()
*/
ATSfunbody_end()
ATSreturn_void(tmpret28) ;
} /* end of [mainats_0_void] */

ATSdynloadflag_init(_057_home_057_amin_057_Docs_057_Compiler_040_Design_057_CS525_055_2020_055_Fall_057_CS525_055_2020_055_Fall_055_a_055_naghavi_057_assigns_057_assign01_057_MySolution_057_lambda0_print_056_dats__dynloadflag) ;
ATSextern()
atsvoid_t0ype
_057_home_057_amin_057_Docs_057_Compiler_040_Design_057_CS525_055_2020_055_Fall_057_CS525_055_2020_055_Fall_055_a_055_naghavi_057_assigns_057_assign01_057_MySolution_057_lambda0_print_056_dats__dynload(/*void*/) ;
ATSdynloadflag_init(_057_home_057_amin_057_Docs_057_Compiler_040_Design_057_CS525_055_2020_055_Fall_057_CS525_055_2020_055_Fall_055_a_055_naghavi_057_assigns_057_assign01_057_MySolution_057_lambda0_fvset_056_dats__dynloadflag) ;
ATSextern()
atsvoid_t0ype
_057_home_057_amin_057_Docs_057_Compiler_040_Design_057_CS525_055_2020_055_Fall_057_CS525_055_2020_055_Fall_055_a_055_naghavi_057_assigns_057_assign01_057_MySolution_057_lambda0_fvset_056_dats__dynload(/*void*/) ;
ATSdynloadflag_init(_057_home_057_amin_057_Docs_057_Compiler_040_Design_057_CS525_055_2020_055_Fall_057_CS525_055_2020_055_Fall_055_a_055_naghavi_057_assigns_057_assign01_057_MySolution_057_lambda0_interp_056_dats__dynloadflag) ;
ATSextern()
atsvoid_t0ype
_057_home_057_amin_057_Docs_057_Compiler_040_Design_057_CS525_055_2020_055_Fall_057_CS525_055_2020_055_Fall_055_a_055_naghavi_057_assigns_057_assign01_057_MySolution_057_lambda0_interp_056_dats__dynload(/*void*/) ;
/*
** for initialization(dynloading)
*/
ATSdynloadflag_minit(_057_home_057_amin_057_Docs_057_Compiler_040_Design_057_CS525_055_2020_055_Fall_057_CS525_055_2020_055_Fall_055_a_055_naghavi_057_assigns_057_assign01_057_MySolution_057_assign01_sol_056_dats__dynloadflag) ;
ATSextern()
atsvoid_t0ype
_057_home_057_amin_057_Docs_057_Compiler_040_Design_057_CS525_055_2020_055_Fall_057_CS525_055_2020_055_Fall_055_a_055_naghavi_057_assigns_057_assign01_057_MySolution_057_assign01_sol_056_dats__dynload()
{
ATSfunbody_beg()
ATSdynload(/*void*/)
ATSdynloadflag_sta(
_057_home_057_amin_057_Docs_057_Compiler_040_Design_057_CS525_055_2020_055_Fall_057_CS525_055_2020_055_Fall_055_a_055_naghavi_057_assigns_057_assign01_057_MySolution_057_assign01_sol_056_dats__dynloadflag
) ;
ATSif(
ATSCKiseqz(
_057_home_057_amin_057_Docs_057_Compiler_040_Design_057_CS525_055_2020_055_Fall_057_CS525_055_2020_055_Fall_055_a_055_naghavi_057_assigns_057_assign01_057_MySolution_057_assign01_sol_056_dats__dynloadflag
)
) ATSthen() {
ATSdynloadset(_057_home_057_amin_057_Docs_057_Compiler_040_Design_057_CS525_055_2020_055_Fall_057_CS525_055_2020_055_Fall_055_a_055_naghavi_057_assigns_057_assign01_057_MySolution_057_assign01_sol_056_dats__dynloadflag) ;
/*
dynexnlst-initize(beg)
*/
/*
dynexnlst-initize(end)
*/
ATSdynloadfcall(_057_home_057_amin_057_Docs_057_Compiler_040_Design_057_CS525_055_2020_055_Fall_057_CS525_055_2020_055_Fall_055_a_055_naghavi_057_assigns_057_assign01_057_MySolution_057_lambda0_print_056_dats__dynload) ;
ATSdynloadfcall(_057_home_057_amin_057_Docs_057_Compiler_040_Design_057_CS525_055_2020_055_Fall_057_CS525_055_2020_055_Fall_055_a_055_naghavi_057_assigns_057_assign01_057_MySolution_057_lambda0_fvset_056_dats__dynload) ;
ATSdynloadfcall(_057_home_057_amin_057_Docs_057_Compiler_040_Design_057_CS525_055_2020_055_Fall_057_CS525_055_2020_055_Fall_055_a_055_naghavi_057_assigns_057_assign01_057_MySolution_057_lambda0_interp_056_dats__dynload) ;
/*
emit_instr: loc0 = /home/amin/Docs/Compiler Design/CS525-2020-Fall/CS525-2020-Fall-a-naghavi/assigns/assign01/MySolution/assign01_sol.dats: 722(line=42, offs=25) -- 733(line=42, offs=36)
*/

/*
#LINCONSTATUS==1
*/
ATSINSmove_con1_beg()
ATSINSmove_con1_new(statmp17, postiats_tysum_0) ;
// #if(1)
ATSINSstore_con1_tag(statmp17, 1) ;
// #endif
ATSINSstore_con1_ofs(statmp17, postiats_tysum_0, atslab__0, ATSPMVstring("x")) ;
ATSINSmove_con1_end()
/*
emit_instr: loc0 = /home/amin/Docs/Compiler Design/CS525-2020-Fall/CS525-2020-Fall-a-naghavi/assigns/assign01/MySolution/assign01_sol.dats: 710(line=42, offs=13) -- 734(line=42, offs=37)
*/

/*
#LINCONSTATUS==1
*/
ATSINSmove_con1_beg()
ATSINSmove_con1_new(statmp16, postiats_tysum_1) ;
// #if(1)
ATSINSstore_con1_tag(statmp16, 2) ;
// #endif
ATSINSstore_con1_ofs(statmp16, postiats_tysum_1, atslab__0, ATSPMVstring("y")) ;
ATSINSstore_con1_ofs(statmp16, postiats_tysum_1, atslab__1, statmp17) ;
ATSINSmove_con1_end()
/*
emit_instr: loc0 = /home/amin/Docs/Compiler Design/CS525-2020-Fall/CS525-2020-Fall-a-naghavi/assigns/assign01/MySolution/assign01_sol.dats: 698(line=42, offs=1) -- 735(line=42, offs=38)
*/

/*
#LINCONSTATUS==1
*/
ATSINSmove_con1_beg()
ATSINSmove_con1_new(statmp15, postiats_tysum_1) ;
// #if(1)
ATSINSstore_con1_tag(statmp15, 2) ;
// #endif
ATSINSstore_con1_ofs(statmp15, postiats_tysum_1, atslab__0, ATSPMVstring("x")) ;
ATSINSstore_con1_ofs(statmp15, postiats_tysum_1, atslab__1, statmp16) ;
ATSINSmove_con1_end()
/*
emit_instr: loc0 = /home/amin/Docs/Compiler Design/CS525-2020-Fall/CS525-2020-Fall-a-naghavi/assigns/assign01/MySolution/assign01_sol.dats: 888(line=57, offs=8) -- 899(line=57, offs=19)
*/

/*
#LINCONSTATUS==1
*/
ATSINSmove_con1_beg()
ATSINSmove_con1_new(statmp23, postiats_tysum_0) ;
// #if(1)
ATSINSstore_con1_tag(statmp23, 1) ;
// #endif
ATSINSstore_con1_ofs(statmp23, postiats_tysum_0, atslab__0, ATSPMVstring("x")) ;
ATSINSmove_con1_end()
/*
emit_instr: loc0 = /home/amin/Docs/Compiler Design/CS525-2020-Fall/CS525-2020-Fall-a-naghavi/assigns/assign01/MySolution/assign01_sol.dats: 901(line=57, offs=21) -- 912(line=57, offs=32)
*/

/*
#LINCONSTATUS==1
*/
ATSINSmove_con1_beg()
ATSINSmove_con1_new(statmp24, postiats_tysum_0) ;
// #if(1)
ATSINSstore_con1_tag(statmp24, 1) ;
// #endif
ATSINSstore_con1_ofs(statmp24, postiats_tysum_0, atslab__0, ATSPMVstring("z")) ;
ATSINSmove_con1_end()
/*
emit_instr: loc0 = /home/amin/Docs/Compiler Design/CS525-2020-Fall/CS525-2020-Fall-a-naghavi/assigns/assign01/MySolution/assign01_sol.dats: 881(line=57, offs=1) -- 913(line=57, offs=33)
*/

/*
#LINCONSTATUS==1
*/
ATSINSmove_con1_beg()
ATSINSmove_con1_new(statmp22, postiats_tysum_2) ;
// #if(1)
ATSINSstore_con1_tag(statmp22, 3) ;
// #endif
ATSINSstore_con1_ofs(statmp22, postiats_tysum_2, atslab__0, statmp23) ;
ATSINSstore_con1_ofs(statmp22, postiats_tysum_2, atslab__1, statmp24) ;
ATSINSmove_con1_end()
/*
emit_instr: loc0 = /home/amin/Docs/Compiler Design/CS525-2020-Fall/CS525-2020-Fall-a-naghavi/assigns/assign01/MySolution/assign01_sol.dats: 923(line=59, offs=8) -- 934(line=59, offs=19)
*/

/*
#LINCONSTATUS==1
*/
ATSINSmove_con1_beg()
ATSINSmove_con1_new(statmp26, postiats_tysum_0) ;
// #if(1)
ATSINSstore_con1_tag(statmp26, 1) ;
// #endif
ATSINSstore_con1_ofs(statmp26, postiats_tysum_0, atslab__0, ATSPMVstring("y")) ;
ATSINSmove_con1_end()
/*
emit_instr: loc0 = /home/amin/Docs/Compiler Design/CS525-2020-Fall/CS525-2020-Fall-a-naghavi/assigns/assign01/MySolution/assign01_sol.dats: 936(line=59, offs=21) -- 947(line=59, offs=32)
*/

/*
#LINCONSTATUS==1
*/
ATSINSmove_con1_beg()
ATSINSmove_con1_new(statmp27, postiats_tysum_0) ;
// #if(1)
ATSINSstore_con1_tag(statmp27, 1) ;
// #endif
ATSINSstore_con1_ofs(statmp27, postiats_tysum_0, atslab__0, ATSPMVstring("z")) ;
ATSINSmove_con1_end()
/*
emit_instr: loc0 = /home/amin/Docs/Compiler Design/CS525-2020-Fall/CS525-2020-Fall-a-naghavi/assigns/assign01/MySolution/assign01_sol.dats: 916(line=59, offs=1) -- 948(line=59, offs=33)
*/

/*
#LINCONSTATUS==1
*/
ATSINSmove_con1_beg()
ATSINSmove_con1_new(statmp25, postiats_tysum_2) ;
// #if(1)
ATSINSstore_con1_tag(statmp25, 3) ;
// #endif
ATSINSstore_con1_ofs(statmp25, postiats_tysum_2, atslab__0, statmp26) ;
ATSINSstore_con1_ofs(statmp25, postiats_tysum_2, atslab__1, statmp27) ;
ATSINSmove_con1_end()
/*
emit_instr: loc0 = /home/amin/Docs/Compiler Design/CS525-2020-Fall/CS525-2020-Fall-a-naghavi/assigns/assign01/MySolution/assign01_sol.dats: 873(line=56, offs=1) -- 950(line=60, offs=2)
*/

/*
#LINCONSTATUS==1
*/
ATSINSmove_con1_beg()
ATSINSmove_con1_new(statmp21, postiats_tysum_2) ;
// #if(1)
ATSINSstore_con1_tag(statmp21, 3) ;
// #endif
ATSINSstore_con1_ofs(statmp21, postiats_tysum_2, atslab__0, statmp22) ;
ATSINSstore_con1_ofs(statmp21, postiats_tysum_2, atslab__1, statmp25) ;
ATSINSmove_con1_end()
/*
emit_instr: loc0 = /home/amin/Docs/Compiler Design/CS525-2020-Fall/CS525-2020-Fall-a-naghavi/assigns/assign01/MySolution/assign01_sol.dats: 861(line=55, offs=1) -- 952(line=61, offs=2)
*/

/*
#LINCONSTATUS==1
*/
ATSINSmove_con1_beg()
ATSINSmove_con1_new(statmp20, postiats_tysum_1) ;
// #if(1)
ATSINSstore_con1_tag(statmp20, 2) ;
// #endif
ATSINSstore_con1_ofs(statmp20, postiats_tysum_1, atslab__0, ATSPMVstring("z")) ;
ATSINSstore_con1_ofs(statmp20, postiats_tysum_1, atslab__1, statmp21) ;
ATSINSmove_con1_end()
/*
emit_instr: loc0 = /home/amin/Docs/Compiler Design/CS525-2020-Fall/CS525-2020-Fall-a-naghavi/assigns/assign01/MySolution/assign01_sol.dats: 848(line=54, offs=1) -- 954(line=62, offs=2)
*/

/*
#LINCONSTATUS==1
*/
ATSINSmove_con1_beg()
ATSINSmove_con1_new(statmp19, postiats_tysum_1) ;
// #if(1)
ATSINSstore_con1_tag(statmp19, 2) ;
// #endif
ATSINSstore_con1_ofs(statmp19, postiats_tysum_1, atslab__0, ATSPMVstring("y")) ;
ATSINSstore_con1_ofs(statmp19, postiats_tysum_1, atslab__1, statmp20) ;
ATSINSmove_con1_end()
/*
emit_instr: loc0 = /home/amin/Docs/Compiler Design/CS525-2020-Fall/CS525-2020-Fall-a-naghavi/assigns/assign01/MySolution/assign01_sol.dats: 836(line=53, offs=1) -- 956(line=63, offs=2)
*/

/*
#LINCONSTATUS==1
*/
ATSINSmove_con1_beg()
ATSINSmove_con1_new(statmp18, postiats_tysum_1) ;
// #if(1)
ATSINSstore_con1_tag(statmp18, 2) ;
// #endif
ATSINSstore_con1_ofs(statmp18, postiats_tysum_1, atslab__0, ATSPMVstring("x")) ;
ATSINSstore_con1_ofs(statmp18, postiats_tysum_1, atslab__1, statmp19) ;
ATSINSmove_con1_end()
} /* ATSendif */
ATSfunbody_end()
ATSreturn_void(tmpret_void) ;
} /* end of [*_dynload] */

/*
** the ATS runtime
*/
#ifndef _ATS_CCOMP_RUNTIME_NONE_
#include "pats_ccomp_runtime.c"
#include "pats_ccomp_runtime_memalloc.c"
#ifndef _ATS_CCOMP_EXCEPTION_NONE_
#include "pats_ccomp_runtime2_dats.c"
#ifndef _ATS_CCOMP_RUNTIME_TRYWITH_NONE_
#include "pats_ccomp_runtime_trywith.c"
#endif /* _ATS_CCOMP_RUNTIME_TRYWITH_NONE_ */
#endif // end of [_ATS_CCOMP_EXCEPTION_NONE_]
#endif /* _ATS_CCOMP_RUNTIME_NONE_ */

/*
** the [main] implementation
*/
int
main
(
int argc, char **argv, char **envp
) {
int err = 0 ;
_057_home_057_amin_057_Docs_057_Compiler_040_Design_057_CS525_055_2020_055_Fall_057_CS525_055_2020_055_Fall_055_a_055_naghavi_057_assigns_057_assign01_057_MySolution_057_assign01_sol_056_dats__dynload() ;
ATSmainats_0_void(err) ;
return (err) ;
} /* end of [main] */

/* ****** ****** */

/* end-of-compilation-unit */
