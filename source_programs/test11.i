routine main():int is
    var b: int;
    var a: array[10] int;
    a[10] := 5;
    if 10 > a[10] then
       a[0] := 228;
    else
       a[0] := 0;
    end
    b := a[0];
    print(b);
    return 0;
end
