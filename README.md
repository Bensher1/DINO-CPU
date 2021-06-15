# DINO-CPU

### Motivation
I completed this CPU design for my Computer Architecture class. This design is used at UC Davis as a Teaching-focused RISC-V CPU. DINO CPU stands for Davis In-Order CPU. The CPU is designed in Chisel which is a hardware design language that allows advanced circuit generation. 

### Process
First I designed the ALU control unit for a single-cycle CPU design. Then I finished the single-cycle design by completing the CPU data path and the control logic for the processor. Once the single-cycle CPU was completed, I turned that design into a more realistic pipelined CPU design. This was done by splitting the single-cycle CPU into 5 different pipeline stages. The last part of the pipelined CPU design was implementing the hazard detection, forwarding logic, and branch predictor. After completing the pipelined CPU we analyzed the performance of the design on real applications. 
