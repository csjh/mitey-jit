// step 0. parse the non function sections (copy from interpreter)
// step 1. generate array of bytecode sizes, partial summing as we go
// 

/*

matrix:
--------------------------------------------------------------------------------
| cnst \ instr | br                 | start                   | end            |
|------------------------------------------------------------------------------|
| function     | jump to postlude   | nop                     | nop (postlude) |
| if           | jump to end        | if (false) jump to end  | nop            |
| if with else | jump to end        | if (false) jump to else | jump to end    |
| else         | jump to end        | not possible            | nop            |
| loop         | jump to start      | nop                     | nop            |
| block        | jump to end        | nop                     | nop            |
-----------------------------------------------------------------------------|

all brs have to move results down the stack, all starts can assume only results remain

*/

