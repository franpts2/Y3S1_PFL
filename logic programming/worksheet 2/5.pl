factorial(N,F):- factorial(N,F,1).

% When N is 0 or 1, the result F is whatever is in the Acc.
factorial(0,F,F).
factorial(1,F,F).

factorial(N,F,Acc):- N > 1,
                     N1 is N-1,
                     Acc1 is Acc * N,
                     factorial(N1,F,Acc1).


sum_rec(N,Sum):- sum_rec(N,Sum,0).

sum_rec(0,Sum,Sum).

sum_rec(N,Sum,Acc):-
    N > 0,
    N1 is N-1,
    Acc1 is Acc+N,
    sum_rec(N1,Sum,Acc1).

% ---
pow_rec(X,Y,P):- 
    Y>=0,
    pow_rec(X,Y,P,1).

pow_rec(_,0,Acc,Acc).

pow_rec(X,Y,P,Acc):-
    Y > 0,
    Y1 is Y-1,
    Acc1 is Acc*X,
    pow_rec(X,Y1,P,Acc1).

% ---
square_rec(0,0).

square_rec(N,S):-
    N > 0,
    square_aux(N,1,1,3,S). % start at 1, cur square is 1, next odd is 3


square_aux(N,N,S,_,S). % when we reach N, result is the Acc

square_aux(N,Count,Acc,NextOdd,S):-
    Count < N,
    NewCount is Count+1,
    NewAcc is Acc + NextOdd,
    NewNextOdd is NextOdd + 2,
    square_aux(N,NewCount,NewAcc,NewNextOdd,S).

% ---
fibonacci(N,F):-
	N >= 0,
	fibonacci(N,0,1,F).

fibonacci(0,A,_,A).

fibonacci(N,A,B,F):-
	N > 0,
	N1 is N-1,
	Next is A+B,
	fibonacci(N1,B,Next,F).


/*
REGULAR RECURSION:
| ?- fibonacci(7,F).
        1      1 Call: fibonacci(7,_809) ? 
        2      2 Call: 7>1 ? 
        2      2 Exit: 7>1 ? 
        3      2 Call: _1841 is 7-1 ? 
        3      2 Exit: 6 is 7-1 ? 
        4      2 Call: _1859 is 7-2 ? 
        4      2 Exit: 5 is 7-2 ? 
        5      2 Call: fibonacci(6,_1879) ? 
        6      3 Call: 6>1 ? 
        6      3 Exit: 6>1 ? 
        7      3 Call: _9183 is 6-1 ? 
        7      3 Exit: 5 is 6-1 ? 
        8      3 Call: _9201 is 6-2 ? 
        8      3 Exit: 4 is 6-2 ? 
        9      3 Call: fibonacci(5,_9221) ? 
       10      4 Call: 5>1 ? 
       10      4 Exit: 5>1 ? 
       11      4 Call: _16401 is 5-1 ? 
       11      4 Exit: 4 is 5-1 ? 
       12      4 Call: _16419 is 5-2 ? 
       12      4 Exit: 3 is 5-2 ? 
       13      4 Call: fibonacci(4,_16439) ? 
       14      5 Call: 4>1 ? 
       14      5 Exit: 4>1 ? 
       15      5 Call: _23619 is 4-1 ? 
       15      5 Exit: 3 is 4-1 ? 
       16      5 Call: _23637 is 4-2 ? 
       16      5 Exit: 2 is 4-2 ? 
       17      5 Call: fibonacci(3,_23657) ? 
       18      6 Call: 3>1 ? 
       18      6 Exit: 3>1 ? 
       19      6 Call: _30837 is 3-1 ? 
       19      6 Exit: 2 is 3-1 ? 
       20      6 Call: _30855 is 3-2 ? 
       20      6 Exit: 1 is 3-2 ? 
       21      6 Call: fibonacci(2,_30875) ? 
       22      7 Call: 2>1 ? 
       22      7 Exit: 2>1 ? 
       23      7 Call: _38055 is 2-1 ? 
       23      7 Exit: 1 is 2-1 ? 
       24      7 Call: _38073 is 2-2 ? 
       24      7 Exit: 0 is 2-2 ? 
       25      7 Call: fibonacci(1,_38093) ? 
?      25      7 Exit: fibonacci(1,1) ? 
       26      7 Call: fibonacci(0,_38105) ? 
?      26      7 Exit: fibonacci(0,0) ? 
       27      7 Call: _30875 is 1+0 ? 
       27      7 Exit: 1 is 1+0 ? 
?      21      6 Exit: fibonacci(2,1) ? 
       28      6 Call: fibonacci(1,_30887) ? 
?      28      6 Exit: fibonacci(1,1) ? 
       29      6 Call: _23657 is 1+1 ? 
       29      6 Exit: 2 is 1+1 ? 
?      17      5 Exit: fibonacci(3,2) ? 
       30      5 Call: fibonacci(2,_23669) ? 
       31      6 Call: 2>1 ? 
       31      6 Exit: 2>1 ? 
       32      6 Call: _57669 is 2-1 ? 
       32      6 Exit: 1 is 2-1 ? 
       33      6 Call: _57687 is 2-2 ? 
       33      6 Exit: 0 is 2-2 ? 
       34      6 Call: fibonacci(1,_57707) ? 
?      34      6 Exit: fibonacci(1,1) ? 
       35      6 Call: fibonacci(0,_57719) ? 
?      35      6 Exit: fibonacci(0,0) ? 
       36      6 Call: _23669 is 1+0 ? 
       36      6 Exit: 1 is 1+0 ? 
?      30      5 Exit: fibonacci(2,1) ? 
       37      5 Call: _16439 is 2+1 ? 
       37      5 Exit: 3 is 2+1 ? 
?      13      4 Exit: fibonacci(4,3) ? 
       38      4 Call: fibonacci(3,_16451) ? 
       39      5 Call: 3>1 ? 
       39      5 Exit: 3>1 ? 
       40      5 Call: _75211 is 3-1 ? 
       40      5 Exit: 2 is 3-1 ? 
       41      5 Call: _75229 is 3-2 ? 
       41      5 Exit: 1 is 3-2 ? 
       42      5 Call: fibonacci(2,_75249) ? 
       43      6 Call: 2>1 ? 
       43      6 Exit: 2>1 ? 
       44      6 Call: _82429 is 2-1 ? 
       44      6 Exit: 1 is 2-1 ? 
       45      6 Call: _82447 is 2-2 ? 
       45      6 Exit: 0 is 2-2 ? 
       46      6 Call: fibonacci(1,_82467) ? 
?      46      6 Exit: fibonacci(1,1) ? 
       47      6 Call: fibonacci(0,_82479) ? 
?      47      6 Exit: fibonacci(0,0) ? 
       48      6 Call: _75249 is 1+0 ? 
       48      6 Exit: 1 is 1+0 ? 
?      42      5 Exit: fibonacci(2,1) ? 
       49      5 Call: fibonacci(1,_75261) ? 
?      49      5 Exit: fibonacci(1,1) ? 
       50      5 Call: _16451 is 1+1 ? 
       50      5 Exit: 2 is 1+1 ? 
?      38      4 Exit: fibonacci(3,2) ? 
       51      4 Call: _9221 is 3+2 ? 
       51      4 Exit: 5 is 3+2 ? 
?       9      3 Exit: fibonacci(5,5) ? 
       52      3 Call: fibonacci(4,_9233) ? 
       53      4 Call: 4>1 ? 
       53      4 Exit: 4>1 ? 
       54      4 Call: _105133 is 4-1 ? 
       54      4 Exit: 3 is 4-1 ? 
       55      4 Call: _105151 is 4-2 ? 
       55      4 Exit: 2 is 4-2 ? 
       56      4 Call: fibonacci(3,_105171) ? 
       57      5 Call: 3>1 ? 
       57      5 Exit: 3>1 ? 
       58      5 Call: _112351 is 3-1 ? 
       58      5 Exit: 2 is 3-1 ? 
       59      5 Call: _112369 is 3-2 ? 
       59      5 Exit: 1 is 3-2 ? 
       60      5 Call: fibonacci(2,_112389) ? 
       61      6 Call: 2>1 ? 
       61      6 Exit: 2>1 ? 
       62      6 Call: _119569 is 2-1 ? 
       62      6 Exit: 1 is 2-1 ? 
       63      6 Call: _119587 is 2-2 ? 
       63      6 Exit: 0 is 2-2 ? 
       64      6 Call: fibonacci(1,_119607) ? 
?      64      6 Exit: fibonacci(1,1) ? 
       65      6 Call: fibonacci(0,_119619) ? 
?      65      6 Exit: fibonacci(0,0) ? 
       66      6 Call: _112389 is 1+0 ? 
       66      6 Exit: 1 is 1+0 ? 
?      60      5 Exit: fibonacci(2,1) ? 
       67      5 Call: fibonacci(1,_112401) ? 
?      67      5 Exit: fibonacci(1,1) ? 
       68      5 Call: _105171 is 1+1 ? 
       68      5 Exit: 2 is 1+1 ? 
?      56      4 Exit: fibonacci(3,2) ? 
       69      4 Call: fibonacci(2,_105183) ? 
       70      5 Call: 2>1 ? 
       70      5 Exit: 2>1 ? 
       71      5 Call: _139183 is 2-1 ? 
       71      5 Exit: 1 is 2-1 ? 
       72      5 Call: _139201 is 2-2 ? 
       72      5 Exit: 0 is 2-2 ? 
       73      5 Call: fibonacci(1,_139221) ? 
?      73      5 Exit: fibonacci(1,1) ? 
       74      5 Call: fibonacci(0,_139233) ? 
?      74      5 Exit: fibonacci(0,0) ? 
       75      5 Call: _105183 is 1+0 ? 
       75      5 Exit: 1 is 1+0 ? 
?      69      4 Exit: fibonacci(2,1) ? 
       76      4 Call: _9233 is 2+1 ? 
       76      4 Exit: 3 is 2+1 ? 
?      52      3 Exit: fibonacci(4,3) ? 
       77      3 Call: _1879 is 5+3 ? 
       77      3 Exit: 8 is 5+3 ? 
?       5      2 Exit: fibonacci(6,8) ? 
       78      2 Call: fibonacci(5,_1891) ? 
       79      3 Call: 5>1 ? 
       79      3 Exit: 5>1 ? 
       80      3 Call: _159815 is 5-1 ? 
       80      3 Exit: 4 is 5-1 ? 
       81      3 Call: _159833 is 5-2 ? 
       81      3 Exit: 3 is 5-2 ? 
       82      3 Call: fibonacci(4,_159853) ? 
       83      4 Call: 4>1 ? 
       83      4 Exit: 4>1 ? 
       84      4 Call: _167033 is 4-1 ? 
       84      4 Exit: 3 is 4-1 ? 
       85      4 Call: _167051 is 4-2 ? 
       85      4 Exit: 2 is 4-2 ? 
       86      4 Call: fibonacci(3,_167071) ? 
       87      5 Call: 3>1 ? 
       87      5 Exit: 3>1 ? 
       88      5 Call: _174251 is 3-1 ? 
       88      5 Exit: 2 is 3-1 ? 
       89      5 Call: _174269 is 3-2 ? 
       89      5 Exit: 1 is 3-2 ? 
       90      5 Call: fibonacci(2,_174289) ? 
       91      6 Call: 2>1 ? 
       91      6 Exit: 2>1 ? 
       92      6 Call: _181469 is 2-1 ? 
       92      6 Exit: 1 is 2-1 ? 
       93      6 Call: _181487 is 2-2 ? 
       93      6 Exit: 0 is 2-2 ? 
       94      6 Call: fibonacci(1,_181507) ? 
?      94      6 Exit: fibonacci(1,1) ? 
       95      6 Call: fibonacci(0,_181519) ? 
?      95      6 Exit: fibonacci(0,0) ? 
       96      6 Call: _174289 is 1+0 ? 
       96      6 Exit: 1 is 1+0 ? 
?      90      5 Exit: fibonacci(2,1) ? 
       97      5 Call: fibonacci(1,_174301) ? 
?      97      5 Exit: fibonacci(1,1) ? 
       98      5 Call: _167071 is 1+1 ? 
       98      5 Exit: 2 is 1+1 ? 
?      86      4 Exit: fibonacci(3,2) ? 
       99      4 Call: fibonacci(2,_167083) ? 
      100      5 Call: 2>1 ? 
      100      5 Exit: 2>1 ? 
      101      5 Call: _201083 is 2-1 ? 
      101      5 Exit: 1 is 2-1 ? 
      102      5 Call: _201101 is 2-2 ? 
      102      5 Exit: 0 is 2-2 ? 
      103      5 Call: fibonacci(1,_201121) ? 
?     103      5 Exit: fibonacci(1,1) ? 
      104      5 Call: fibonacci(0,_201133) ? 
?     104      5 Exit: fibonacci(0,0) ? 
      105      5 Call: _167083 is 1+0 ? 
      105      5 Exit: 1 is 1+0 ? 
?      99      4 Exit: fibonacci(2,1) ? 
      106      4 Call: _159853 is 2+1 ? 
      106      4 Exit: 3 is 2+1 ? 
?      82      3 Exit: fibonacci(4,3) ? 
      107      3 Call: fibonacci(3,_159865) ? 
      108      4 Call: 3>1 ? 
      108      4 Exit: 3>1 ? 
      109      4 Call: _218625 is 3-1 ? 
      109      4 Exit: 2 is 3-1 ? 
      110      4 Call: _218643 is 3-2 ? 
      110      4 Exit: 1 is 3-2 ? 
      111      4 Call: fibonacci(2,_218663) ? 
      112      5 Call: 2>1 ? 
      112      5 Exit: 2>1 ? 
      113      5 Call: _225843 is 2-1 ? 
      113      5 Exit: 1 is 2-1 ? 
      114      5 Call: _225861 is 2-2 ? 
      114      5 Exit: 0 is 2-2 ? 
      115      5 Call: fibonacci(1,_225881) ? 
?     115      5 Exit: fibonacci(1,1) ? 
      116      5 Call: fibonacci(0,_225893) ? 
?     116      5 Exit: fibonacci(0,0) ? 
      117      5 Call: _218663 is 1+0 ? 
      117      5 Exit: 1 is 1+0 ? 
?     111      4 Exit: fibonacci(2,1) ? 
      118      4 Call: fibonacci(1,_218675) ? 
?     118      4 Exit: fibonacci(1,1) ? 
      119      4 Call: _159865 is 1+1 ? 
      119      4 Exit: 2 is 1+1 ? 
?     107      3 Exit: fibonacci(3,2) ? 
      120      3 Call: _1891 is 3+2 ? 
      120      3 Exit: 5 is 3+2 ? 
?      78      2 Exit: fibonacci(5,5) ? 
      121      2 Call: _809 is 8+5 ? 
      121      2 Exit: 13 is 8+5 ? 
?       1      1 Exit: fibonacci(7,13) ? 
F = 13 ? 
yes


TAIL RECURSION:
| ?- fibonacci(7,F).
        1      1 Call: fibonacci(7,_809) ? 
        2      2 Call: 7>=0 ? 
        2      2 Exit: 7>=0 ? 
        3      2 Call: fibonacci(7,0,1,_809) ? 
        4      3 Call: 7>0 ? 
        4      3 Exit: 7>0 ? 
        5      3 Call: _4495 is 7-1 ? 
        5      3 Exit: 6 is 7-1 ? 
        6      3 Call: _4513 is 0+1 ? 
        6      3 Exit: 1 is 0+1 ? 
        7      3 Call: fibonacci(6,1,1,_809) ? 
        8      4 Call: 6>0 ? 
        8      4 Exit: 6>0 ? 
        9      4 Call: _11333 is 6-1 ? 
        9      4 Exit: 5 is 6-1 ? 
       10      4 Call: _11351 is 1+1 ? 
       10      4 Exit: 2 is 1+1 ? 
       11      4 Call: fibonacci(5,1,2,_809) ? 
       12      5 Call: 5>0 ? 
       12      5 Exit: 5>0 ? 
       13      5 Call: _18171 is 5-1 ? 
       13      5 Exit: 4 is 5-1 ? 
       14      5 Call: _18189 is 1+2 ? 
       14      5 Exit: 3 is 1+2 ? 
       15      5 Call: fibonacci(4,2,3,_809) ? 
       16      6 Call: 4>0 ? 
       16      6 Exit: 4>0 ? 
       17      6 Call: _25009 is 4-1 ? 
       17      6 Exit: 3 is 4-1 ? 
       18      6 Call: _25027 is 2+3 ? 
       18      6 Exit: 5 is 2+3 ? 
       19      6 Call: fibonacci(3,3,5,_809) ? 
       20      7 Call: 3>0 ? 
       20      7 Exit: 3>0 ? 
       21      7 Call: _31847 is 3-1 ? 
       21      7 Exit: 2 is 3-1 ? 
       22      7 Call: _31865 is 3+5 ? 
       22      7 Exit: 8 is 3+5 ? 
       23      7 Call: fibonacci(2,5,8,_809) ? 
       24      8 Call: 2>0 ? 
       24      8 Exit: 2>0 ? 
       25      8 Call: _38685 is 2-1 ? 
       25      8 Exit: 1 is 2-1 ? 
       26      8 Call: _38703 is 5+8 ? 
       26      8 Exit: 13 is 5+8 ? 
       27      8 Call: fibonacci(1,8,13,_809) ? 
       28      9 Call: 1>0 ? 
       28      9 Exit: 1>0 ? 
       29      9 Call: _45523 is 1-1 ? 
       29      9 Exit: 0 is 1-1 ? 
       30      9 Call: _45541 is 8+13 ? 
       30      9 Exit: 21 is 8+13 ? 
       31      9 Call: fibonacci(0,13,21,_809) ? 
?      31      9 Exit: fibonacci(0,13,21,13) ? 
?      27      8 Exit: fibonacci(1,8,13,13) ? 
?      23      7 Exit: fibonacci(2,5,8,13) ? 
?      19      6 Exit: fibonacci(3,3,5,13) ? 
?      15      5 Exit: fibonacci(4,2,3,13) ? 
?      11      4 Exit: fibonacci(5,1,2,13) ? 
?       7      3 Exit: fibonacci(6,1,1,13) ? 
?       3      2 Exit: fibonacci(7,0,1,13) ? 
?       1      1 Exit: fibonacci(7,13) ? 
F = 13 ? 
yes
*/