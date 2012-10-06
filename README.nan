This branch implements NaN integers in Clang compiler. Programmers can declare an integer to be an NaN integer by adding a type qualifier "_nan" to an integer declaration. The type conversion rules for NaN integers are as follows.
	- An integer of type T will be automatically promoted to _nan T when used with an integer of type _nan T.
	- The resulting type of an arithmetic or comparison operation with operands of type _nan T is also _nan T.

Up to now, this branch has implements integer overflow checks for the following operations:
	- Binary operations: x + y, x - y,x * y,x / y,x % y and bit operations;
	- Unary operations: -x, x++, x--, ++x, --x;
	- Assignment operations: x += y, x -= y, x *= y, x /= y;
	- Integer comparisions;
	_ Integer conversions

For NaN integers, if integer overflows occurr during the above operations or any source operand is the NaN state, compiler will automatically set the result to the NaN state, otherwise the operation follows standard C rules. We choose the maximum value as the NaN state for unsigned integers, and the minimum for signed integers.

This branch also implements two built-in functions to handle NaN integers.
	- bool isnan(_nan T x)
	returns true if and only if x is NaN state
	- T unnan(_nan T x, T v) 
	returns v if x is NaN state, or (T)x otherwise.

Several examples are available in nan_testcases folder.

Contact us:
If you find a bug in NaN compiler, please send email to jiazhih@gmail.com. Thanks in advance!
