#! /bin/bash -v
tar -zxvf lua-5.3.5.tar.gz
tar -zxvf lpeg-1.0.1.tar.gz
cd lua-5.3.5
sudo apt-get install libreadline-dev
make linux
sudo make install
cd ../lpeg-1.0.1
make LUADIR="../lua-5.3.5/src" linux
cp lpeg.so ../../lualib/c/
make test
cd ..
rm -rf lua-5.3.5
rm -rf lpeg-1.0.1
