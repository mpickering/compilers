(* lab2/loop.p *)

begin
  x := 12345;
  i := 1;
  loop
    print i; newline;
    j := 2 * i; 
    if j > x then exit end;
    i := j
  end
end.

(*<<
 1
 2
 4
 8
 16
 32
 64
 128
 256
 512
 1024
 2048
 4096
 8192
>>*)
