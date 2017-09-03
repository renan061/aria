#!/bin/sh

# Test type identifier
if [ $1 = "" ]
then
	echo "test script error: missing argument"
	exit 1
fi

cd tests/$1

# Removing old diff and output test reports
rm -f ./diff/*.txt
rm -f ./output/*.out

# Testing
OK=0
for INPUT in input/*.aria
do
	NAME=${INPUT##*/}
	NAME=${NAME%%.aria}
	ANSWER="answer/"$NAME".asw"
	OUTPUT=$NAME".out"
	DIFF="diff.txt"

	../../bin/"$1""test" $INPUT $2 > $OUTPUT 2>&1

	diff -W 200 -a --suppress-common-lines $ANSWER $OUTPUT > $DIFF
	if [ -s $DIFF ]
	then
		echo "FAIL $1 test <"$NAME">"
		mv $DIFF "diff/"$NAME".txt"
		OK=$(($OK+1))
	else
	    echo "OK $1 test <"$NAME">"
	    rm $DIFF
	fi

	mv $OUTPUT "output/"$OUTPUT
done

exit $OK
