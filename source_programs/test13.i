routine main():int is
  var x : int;
  var a: array[11] int;
  a[0] := 0;
  a[1] := 10;
  while a[1] > a[0] loop
      x := x + 1;
      a[1] := a[1] - 1;
  end
  print(x);
  return 0;
end
