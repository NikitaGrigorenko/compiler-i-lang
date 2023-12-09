routine ifStatement(lastDigit : int): void is
  if lastDigit % 2 = 0 xor lastDigit % 3 = 0 then
    print(39393939);
  end
end

routine main(): int is
  var number : int is 23679;
  var lastDigit : int;
  var numberOfDigits : int is 0;
  var arr : array[10] int;

  print(number);
  print(1000000000);
  while number > 0 loop
    lastDigit := number % 10;
    print(lastDigit);
    ifStatement(lastDigit);
    number := number / 10;
    numberOfDigits := numberOfDigits + 1;
  end
  print(1000000000);
  print(numberOfDigits);
  return 0;
end
