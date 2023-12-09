routine fib(x : int) : int is
    var n : int is x;
    if n > 1 then
       n := fib(n - 1) + fib(n - 2);
    end
    return n;
end

routine main() : int is
    var arr : array[10] int;
    for i in reverse -1..9 loop
      arr[i] := fib(i);
      print(fib(i));
    end

    var fibNum : int is arr[5];
    var fibSum : int is 0;
    print(fibNum);

    while fibNum > 0 loop
      fibSum := fibSum + arr[fibNum];
      fibNum := fibNum - 1;
    end

    print(fibSum);
    return 0;
end
