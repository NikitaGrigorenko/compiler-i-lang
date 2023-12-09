type ourFloatType is float;

routine parityChecker(number : int) : bool is
  var flag : bool is false;
  if number % 2 = 0 then
    flag := true;
  end
  return flag;
end

routine main(): ourFloatType is
    var flagDivident : bool is false;
    var flagDivisor : bool is true;
    var dividend : int is 28;
    var divisor : int is 4;

    flagDivident := parityChecker(dividend);
    flagDivisor := parityChecker(divisor);

    if flagDivident and flagDivisor then
      print(222222222);
    else
      print(111111111);
    end
    var quotient : int is dividend / divisor;
    var remainder : int is dividend % divisor;
    print(quotient);
    print(remainder);
    return 0.5;
end
