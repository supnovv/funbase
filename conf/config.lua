workers = 1
services = 1024
logfile = "lnlylib"
script = ""

http_default = {
  ip = "127.0.0.1";
  port = 80;
  rxmax = 1024*8;
}

test = {
  i = 1;
  n = 0.1;
  s = "a";
  t = {i = 2; n = 0.2; s = "b"; };
}

