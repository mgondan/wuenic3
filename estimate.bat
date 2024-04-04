call xsb64 --quietload --nobanner --noprompt -e "consult('xsb/wuenic_ver_3.pl'), consult('xsb/%1'), estimate, halt." 2>warn30.txt
call xsb64 --quietload --nobanner --noprompt -e "consult('xsb/wuenic_ver_3_9.P'), consult('xsb/%1'), estimate, halt." 2>warn39.txt
swipl -g "consult('xsb/wuenic_ver_4.pl'), consult('xsb/%1'), estimate" -g halt 2>warn40.txt
R --quiet --no-echo --file=R\wuenic_ver_4.R --args %1 2>warn4r.txt
swipl -g "(file_sha1('out/%1.v30.txt', V30), file_sha1('out/%1.v39.txt', V39), file_sha1('out/%1.v40.txt', V40), file_sha1('out/%1.R.txt', R), writeln(V30-v30), writeln(V39-v39), writeln(V40-v40), writeln(R-r))" -g halt
R --quiet --no-echo --file=R\diff.R --args %1
