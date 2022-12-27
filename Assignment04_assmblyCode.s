.globl __start

.text
.align 4

__start:

        la t0, Input_data    #store address of Input_data
        la t1, Output_data   #store address of Output_data
        la s3, Output_data   #store index to exit 'validate'
        li t2, 32
        
loop:  
        #insert sorting algorithm
        beq t2, zero, end    #if every word is checked, exit this loop
        lw s0, 0(t0)         #load number from current Input_data address
        sw s0, 0(t1)         #first, store loaded number in current Output_data address
        mv t3, t1            #save current Output_data address in t3 for loop index
        addi t0, t0, 4       #increase current Input_data address
        addi t1, t1, 4       #increase current Ouput_data address
        addi t2, t2, -1      #decrease left word count
        j validate           #validate Ouput_data 
        
validate:
        beq s3, t3, loop     #if current Output_data address is equal to start address of Ouput_data, we don't have to check. go back to loop
        mv t4, t3            #store current Output_data address to t4
        addi t4, t4, -4      #make t4 a right before address of current Output_data
        lw s0, 0(t3)         #load word from current Output_data
        lw s1, 0(t4)         #load word from before Output_data
        blt s0, s1, loop     #if current number is smaller, which means this comparison doesn't make change,then go back to loop
        sw s0, 0(t4)         #if not, swap each number by store s0(current word) to t4(before address)
        sw s1, 0(t3)         #and s1(before word) to t4(current address)
        addi t3, t3, -4      #decrease current Output_data address
        j validate           #validate Output_data recursively
        
end:
        li t5, 15           # result = 15;
        li t6, 0            # i=0
        li a6, 32           # loop end checker
forloop:
        bge t6, a6, loopend # for loop ends
        lw s4, 0(s3)        # s4 <= *Output_data
        bne s4, t5, ERROR   # if(*Ouput_data != result) goto ERROR
        addi s3, s3, 4      # Output_data++
        addi t5, t5, 1      # result++
        addi a6, a6, -1     # i--
        j forloop

loopend:
        lui a5, 0xffff2
        addi a5, a5, 12     #load address of Seg0
        li a4, 0x79
        sw a4, 0(a5)        #Display 1 on HEX0
Smile:
        j Smile

ERROR:
        lui a5, 0xffff2
        addi a5, a5, 16     #load address of Seg1
        li a4, 0x24        
        sw a4, 0(a5)        #Display 2 on HEX1
Frown:
        j Frown
        
.data
.align 4
Input_data: .word 2, 0, -7, -1, 3, 8, -4, 10
            .word -9, -16, 15, 13, 1, 4, -3, 14      
            .word -8, -10, -15, 6, -13, -5, 9, 12
            .word -11, -14, -6, 11, 5, 7, -2, -12
            
Output_data:.word 0, 0, 0, 0, 0, 0, 0, 0
            .word 0, 0, 0, 0, 0, 0, 0, 0
            .word 0, 0, 0, 0, 0, 0, 0, 0
            .word 0, 0, 0, 0, 0, 0, 0, 0