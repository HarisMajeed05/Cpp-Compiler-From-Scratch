start:
li x1, 5
li x2, 10
add x3, x1, x2

loop:
addi x3, x3, -1
bne x3, x0, loop