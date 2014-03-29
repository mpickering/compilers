
(* lab2/loop.p *)

begin
  i := 0;
  loop
    print 5 * i; newline;
    loop
      i := i + 1;
      if i mod 2 = 0 then exit end;
    end;
    if i > 6 then exit end;
  end
end.

(*<<
 0
 10
 20
 30
>>*)
