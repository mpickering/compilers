(* lab3/array.p *)

var a: array 10 of integer;
var i: integer;

begin
  i := 2; a[0] := 1; a[1] := 1;
  while i < 10 do
    a[i] := a[i-2] + a[i-1];
    i := i+1
  end;
  i := 0;
  while i < 10 do
    print a[i]; newline;
    i := i+1;
  end
end.

(*<<
 1
 1
 2
 3
 5
 8
 13
 21
 34
 55
>>*)
