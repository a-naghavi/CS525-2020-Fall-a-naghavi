The functions 'trans01_tpgm', 't1pgm_interp', and 't1pgm_tinfer' are implemented and tested. trans01_tpgm and t1pgm_interp works for all test cases. Also tipgm_tinfer works for all cases except for test03.lam. I will be glad if you can help me to fix that. To build the project and test it please run the following codes:
```
make
./project ./TEST/fibo.lam
./project ./TEST/fact.lam
./project ./TEST/coin_change.lam
./midterm ./TEST/test03.lam
```
In PGround folder the Fibonacci function is written in C in the file 'fibo_dats.c'. To test that please run the following commands:
```
cd PGround
make
./fibo_dats
```

