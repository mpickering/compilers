(* lab2/repeat.p *)

begin
  i := 0;
  repeat
    i := i + 1; 
    i := i * i;
    print i; newline
  until i mod 5 = 0
end.

(*<<
 1
 4
 25
>>*)
