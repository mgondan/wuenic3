rem estimate.bat afg
call xsb64 --quietload --nobanner --noprompt -e "consult('xsb/wuenic_ver_3.pl'), consult('xsb/%1.pl'), estimate, halt." 2>warn30.txt
