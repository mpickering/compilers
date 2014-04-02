(* lab4/digits.p *)

var q;

proc search(k, n, avail);

  var d, nn;

  proc avail1(x);
  begin
    if x <> d then
      return avail(x)
    else
      return 0
    end
  end;

begin
  if k = 10 then
    print n; newline
  else
    d := 1;
    while d < 10 do
      nn := 10 * n + d;
      if avail(d) and (nn mod k = 0) then
        q := search(k+1, nn, avail1)
      end;
      d := d+1
    end
  end;
  return 0
end;

proc all(x);
begin
  return 1
end;

begin
  q := search(1, 0, all)
end.

(*<<
 381654729
>>*)
