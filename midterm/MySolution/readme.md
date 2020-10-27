A closure-based interpreter and a type checker (based on type inference) are totally implemented. Furthermore the occurs check is implemented in type checker. Please first make xanadu then make the project and test it using following commands:
```
make
./midterm ./TEST/test01.lam
./midterm ./TEST/test02.lam
./midterm ./TEST/coin_change.lam
```
To test the occurs_check just call the function in file occurs_check.lam using following:
```
make
./midterm ./TEST/occurs_check.lam
```
This function should gives a type check error due to the occurs_check.

The function isPrime is implemented in lambda in is_prime.lam and the function 8 queens is implemented in eight_queens.lam. By calling these functions you can see the result of isprime on 113 and the result of solving eight queen problem.
```
make
./midterm ./TEST/is_prime.lam
./midterm ./TEST/eight_queens.lam
```
