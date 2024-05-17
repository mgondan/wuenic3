# wuenic3

Please make sure that both xsb64.bat and swipl.exe are on the PATH. xsb is used by the Prolog program, swipl.exe is needed to determine the SHA checksum.

For a single country
````
cd wuenic3
estimate.bat bgd
sha1sum.bat bgd
````
The output file is found in out\bgd.txt.

For all countries
````
cd wuenic3
all.bat
sha1all.bat > v3.txt
````
