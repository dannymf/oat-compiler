define i64 @main(i64 %argc, i8** %arcv) {
  %1 = alloca i1
  store i1 1, i1* %1
  %2 = load i1, i1* %1
  ret i64 %2
}
