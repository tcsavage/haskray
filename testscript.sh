#1.1.1.1
time ./run.sh -w 500 -h 500 -s 1 -i testscenes/cornellbox.scn -o testresults/1.1.1.1.bmp +RTS -K20M

#1.1.1.2
time ./run.sh -w 500 -h 500 -s 1 -i testscenes/infinitefloor.scn -o testresults/1.1.1.2.bmp +RTS -K20M

#1.1.1.3
time ./run.sh -w 500 -h 500 -s 1 -i testscenes/triangletest.scn -o testresults/1.1.1.3.bmp +RTS -K20M

#1.1.1.4
time ./run.sh -w 500 -h 500 -s 5 -i testscenes/creeperflat.scn -o testresults/1.1.1.4.bmp +RTS -K20M

#1.1.1.5
time ./run.sh -w 500 -h 500 -s 1 -i testscenes/1.1.1.5-1.scn -o testresults/1.1.1.5-1.bmp +RTS -K20M
time ./run.sh -w 500 -h 500 -s 1 -i testscenes/1.1.1.5-2.scn -o testresults/1.1.1.5-2.bmp +RTS -K20M

#1.1.2.1.1
time ./run.sh -w 500 -h 500 -s 1 -i testscenes/flatcolours.scn -o testresults/1.1.2.1.1.bmp +RTS -K20M

#1.1.2.1.2
time ./run.sh -w 500 -h 500 -s 5 -i testscenes/texturetest.scn -o testresults/1.1.2.1.2.bmp +RTS -K20M

#1.1.2.1.3
time ./run.sh -w 500 -h 500 -s 10 -i testscenes/shadowtest.scn -o testresults/1.1.2.1.3.bmp +RTS -K20M

#1.1.2.1.4
time ./run.sh -w 500 -h 500 -s 10 -g 1 -i testscenes/cornellbox.scn -o testresults/1.1.2.1.4.bmp +RTS -K20M

#1.1.2.2
time ./run.sh -w 500 -h 500 -s 10 -i testscenes/cornellbox.scn -o testresults/1.1.2.2.bmp +RTS -K20M

#1.1.2.3
time ./run.sh -w 500 -h 500 -s 5 -i testscenes/transmissivetest.scn -o testresults/1.1.2.3.bmp +RTS -K20M

#1.1.2.4
time ./run.sh -w 500 -h 500 -s 5 -i testscenes/1.1.2.4-1.scn -o testresults/1.1.2.4-1.bmp +RTS -K20M
time ./run.sh -w 500 -h 500 -s 5 -i testscenes/1.1.2.4-2.scn -o testresults/1.1.2.4-2.bmp +RTS -K20M

#1.2.1
time ./run.sh -w 500 -h 500 -s 5 -i testscenes/1.2.1-1.scn -o testresults/1.2.1-1.bmp +RTS -K20M
time ./run.sh -w 500 -h 500 -s 5 -i testscenes/1.2.1-2.scn -o testresults/1.2.1-2.bmp +RTS -K20M

#1.2.2
time ./run.sh -w 500 -h 500 -s 5 -i testscenes/1.2.2-1.scn -o testresults/1.2.2-1.bmp +RTS -K20M
time ./run.sh -w 500 -h 500 -s 5 -i testscenes/1.2.2-2.scn -o testresults/1.2.2-2.bmp +RTS -K20M

#1.2.3
time ./run.sh -w 500 -h 500 -s 5 -i testscenes/1.2.3-1.scn -o testresults/1.2.3-1.bmp +RTS -K20M
time ./run.sh -w 500 -h 500 -s 5 -i testscenes/1.2.3-2.scn -o testresults/1.2.3-2.bmp +RTS -K20M

#1.2.4
time ./run.sh -w 500 -h 500 -s 5 -i testscenes/1.2.4-1.scn -o testresults/1.2.4-1.bmp +RTS -K20M
time ./run.sh -w 500 -h 500 -s 5 -i testscenes/1.2.4-2.scn -o testresults/1.2.4-2.bmp +RTS -K20M

#2.1.1
time ./run.sh -w 100 -h 100 -s 1 -i testscenes/cornellbox.scn -o testresults/2.1.1-100x100.bmp +RTS -K20M
time ./run.sh -w 100 -h 50 -s 1 -i testscenes/cornellbox.scn -o testresults/2.1.1-100x50.bmp +RTS -K20M
time ./run.sh -w 50 -h 100 -s 1 -i testscenes/cornellbox.scn -o testresults/2.1.1-50x100.bmp +RTS -K20M
time ./run.sh -w 1 -h 1000 -s 1 -i testscenes/cornellbox.scn -o testresults/2.1.1-1x1000.bmp +RTS -K20M

#2.3
time ./run.sh -w 500 -h 500 -s 1 -i testscenes/cornellbox.scn -o testresults/2.3-1s.bmp +RTS -K20M
time ./run.sh -w 500 -h 500 -s 5 -i testscenes/cornellbox.scn -o testresults/2.3-5s.bmp +RTS -K20M
time ./run.sh -w 500 -h 500 -s 10 -i testscenes/cornellbox.scn -o testresults/2.3-10s.bmp +RTS -K20M
