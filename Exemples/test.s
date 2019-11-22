        .text
        .globl main
        .type  main, @function
main:
        pushq  %rbp
        movq   %rsp, %rbp
        movq   $45, %rax
        movq   %rax, -4(%rbp)
        movq   $32, %rax
        movq   %rax, -8(%rbp)
        leave
        ret
