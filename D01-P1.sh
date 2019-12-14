cat input | while read line; do echo $((line / 3 - 2)); done | tr '\n' '+' | echo "`cat`0" | bc
