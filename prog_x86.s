start:
    mov eax, 0      ; eax = 0
    mov ebx, 5      ; ebx = 5
loop:
    add eax, ebx    ; eax += ebx
    dec ebx
    cmp ebx, 0
    jne loop
    ret
