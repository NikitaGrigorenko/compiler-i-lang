/*
/ Main function
/ return x
*/
routine main():int is
  // arr for counts digits
  var arr : array[10] int;
  var number : integer is 22455588;

  print(number);
  var lastDigit : int;
  for i in 0..9 loop
    arr[i] := 0;
  end

  // Iterate through number
  while number > 0 loop
    lastDigit := number % 10;
    arr[lastDigit] := arr[lastDigit] + 1;
    number := number / 10;
  end
  // Finish
  arr[8] := arr[8] - 1;
  // Printing of the array
  for i in 0..9 loop
    print(arr[i]);
  end
  return 0;
end
