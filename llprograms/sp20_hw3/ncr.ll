define i64 @main(i64 %argc, i8** %arcv) {
    %1 = call i64 @fac(i64 9)
    %2 = call i64 @fac(i64 3)
    %3 = sub i64 9, 3
    %4 = call i64 @fac(i64 %3)
    %5 = mul i64 %2, %4
    %6 = call i64 @div(i64 %1, i64 %5)
    ret i64 %6
}

define i64 @div(i64 %num, i64 %div) {
    %1 = icmp slt i64 %num, %div
    br i1 %1, label %rett, label %recc
rett:
    ret i64 0
recc:
    %2 = sub i64 %num, %div
    %3 = call i64 @div(i64 %2, i64 %div)
    %4 = add i64 %3, 1
    ret i64 %4
}

define i64 @fac(i64 %n) {
    %1 = icmp sle i64 %n, 0
    br i1 %1, label %rett, label %rec
rett:
    ret i64 1
rec:
    %2 = sub i64 %n, 1
    %3 = call i64 @fac(i64 %2)
    %4 = mul i64 %n, %3
    ret i64 %4
}
