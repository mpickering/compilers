(* lab2/case.p *)

begin
  i := 0;
  while i < 10 do
    case i of
      1, 3, 5:
        i := i + 1;
        i := i + 2
    | 2, 6: 
        i := i - 1;
    | 8:
        i := i + 2;
    else
      i := i + 1
    end;
    print i; newline
  end
end.

(*<<
 1
 4
 5
 8
 10
>>*)
