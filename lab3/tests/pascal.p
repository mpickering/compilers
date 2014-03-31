(* lab3/pascal.p *)

var a: array 5 of array 5 of integer;
var i, j: integer;

begin
  i := 0;
  while i < 5 do
    a[i][0] := 1; j := 1;
    print a[i][0];
    while j <= i do
      a[i][j] := a[i-1][j-1] + a[i-1][j];
      print a[i][j];
      j := j+1
    end;
    newline;
    i := i+1
  end 
end.

(*<<
 1
 1 1
 1 2 1
 1 3 3 1
 1 4 6 4 1
>>*)
