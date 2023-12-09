routine main():int is
    var b: int;
    var a: array[10] int;
    for j in reverse 5 .. -1 loop
        print(j);
        a[j] := 100 * j;
    end
    print(100000);
    b := a[4];
    print(b);
    return 0;
end
