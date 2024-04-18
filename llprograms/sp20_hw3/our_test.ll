%arr = type [2 x i64]
%var1 = type { i64, %arr* }
%var2 = type { i64, %arr*, %var1* }

@tmp = global %arr [ i64 0, i64 0 ]
@v1 = global %var1 {i64 2, %arr* @tmp}
@v2 = global %var2 {i64 1, %arr* null, %var1* @v1}

define i64 @main(i64 %argc, i8** %arcv) {
	%a = getelementptr %arr, %arr* @tmp, i32 0, i32 0
	%b = getelementptr %arr, %arr* @tmp, i32 0, i32 1
	store i64 10, i64* %a
	store i64 11, i64* %b
	%1 = getelementptr %var2, %var2* @v2, i32 0, i32 2
	%2 = load %var1*, %var1** %1
	%3 = getelementptr %var1, %var1* %2, i32 0, i32 1
	%4 = load %arr*, %arr** %3
	%5 = getelementptr %arr, %arr* %4, i32 0, i32 0
	store i64 11, i64* %5
	%6 = getelementptr %arr, %arr* %4, i32 0, i32 1
	store i64 10, i64* %5
	%9 = load i64, i64* %a
	%10 = load i64, i64* %b
	%result = sub i64 %9, %10
	ret i64 %result
}