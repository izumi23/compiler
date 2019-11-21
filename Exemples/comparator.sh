#!/bin/bash
#pour pouvoir l'executer chmod +x comparator.sh
#puis ./comparator.sh filename.c
NAME="filec"


if [ $# -eq 1 ]
then
    NAME=$1

echo "make" 
make clean
make 

NAMEC="$NAME.s"
echo "compile (translate) the file $NAME with mcc to $NAMEC"
./mcc -E $NAME > $NAMEC

NAMEG="$NAME.gcc.s"
echo "compile (translate) the file $NAME With gcc to $NAMEG"
gcc -S -fno-asynchronous-unwind-tables $NAME -o "$NAMEG"

echo "open both"
emacs $NAMEC $NAMEG &
else
echo "no file!"
fi    
