Current status:

Trans01 is completed and tested

What is done in the last week:

The transition from level 0 syntax to level 1 syntax is completed. In the previous version of the code I used tinfer for type inference and the t1erm was a data type. Now it this version, every t1erm is a box with both term and type and the type inference is done during the transition from level0 to level1 syntax. Also after a week of strugling, all of the bugs are fixed and now the code can manage the execution of test03.lam. 
To test the code just run following commands:
```
make
./project ./TEST/fibo.lam
./project ./TEST/fact.lam
./project ./TEST/coin_change.lam
./project ./TEST/test03.lam
```

