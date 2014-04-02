(* lab4/gcd.p *)

var x, y;

begin
  
  x := 3 * 37; y := 5 * 37;
  while x <> y do
    if x > y then
      x := x - y
    else
      y := y - x
    end
  end;
  print x; newline
end.

(*<<
 37
>>*)
