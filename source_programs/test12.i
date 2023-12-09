routine main():int is
    var b: int;
    var a: array[11] int;
    a[10] := 5;
    a[11] := -1;
    for j in reverse a[10] .. a[11] loop
        print(j);
        a[j] := 100 * j;
        print(a[j]);
        print(1000000000);
    end
    return 0;
end
