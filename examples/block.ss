let g = "global";
{
  let l1 = "local1";
  let l2 = "local2";
  l2 = "local_2";
  l1 = "local_1";
  print l2;
  print l1;
  print g;
}