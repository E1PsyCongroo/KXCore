# KXCore 项目文档

## 概述

KXCore 是一个基于 la32r 指令集的乱序多发射 CPU, 支持3发射队列，双提交同时具有分支预测，Cache等优化设计。

我们实现了 LoongArch32 精简版，支持：
- **基础指令集**：包含算术运算、逻辑运算、移位、分支跳转等指令
- **访存指令**：支持字节、半字、字的加载存储操作
- **系统指令**：ERTN、SYSCALL、EBREAK等特权指令
- **CSR操作**：CSRRD、CSRWR、CSRXCHG等控制状态寄存器指令
- **中断异常处理**：完整的异常处理机制和中断控制
- **AXI总线接口**：支持标准AXI3总线协议

### 核心参数
- **数据位宽**：32位
- **取指宽度**：四路取指 (fetchWidth = 4)
- **译码宽度**：2路译码 (coreWidth = 2)
- **发射宽度**：3个发射队列，总计4个执行单元
  - MEM IQ: 发射宽度1，队列深度12
  - UNQ IQ: 发射宽度1，队列深度12  
  - INT IQ: 发射宽度2，队列深度20
- **退休宽度**：2路退休 (retireWidth = 2)
- **物理寄存器**：80个物理寄存器，32个逻辑寄存器
- **ROB大小**：32个条目的重排序缓冲区
- **取指缓冲区**：32个条目 (fbNum = 32)
- **取指目标队列**：32个条目 (ftqNum = 32)
- **返回地址栈**：32个条目 (rasNum = 32)

## 总体架构

KXCore 使用了前后端10级流水线架构，其中前端为4级流水线，后端为6级流水线。

### 顶层模块层次
```
Core (核心处理器)
├── TLB (地址转换缓冲区)
├── CSR (控制状态寄存器)
├── FrontEnd (前端)
│   ├── ICache (指令缓存)
│   ├── BranchPredictor (分支预测器)
│   │   ├── BIM (分支指令缓冲区)
│   │   ├── BTB (分支目标缓冲区)
│   │   └── RAS (返回地址栈)
│   ├── FetchBuffer (取指缓冲区)
│   └── FTQ (取指目标队列)
└── BackEnd (后端)
    ├── Decoder (译码器)
    ├── RenameMapTable (重命名映射表)
    ├── RenameFreeList (重命名空闲列表)
    ├── RenameBusyTable (重命名忙表)
    ├── BasicDispatcher (基本分派器)
    ├── IssueUnits (发射单元) × 3
    │   ├── MemIssueUnit (内存发射单元)
    │   ├── UnqIssueUnit (特殊发射单元)
    │   └── IntIssueUnit (整数发射单元)
    ├── ExecutionUnits (执行单元) × 4
    │   ├── MemExeUnit (内存执行单元)
    │   ├── UniqueExeUnit (特殊执行单元)
    │   └── ALUExeUnit (ALU执行单元) × 2
    ├── FullyPortedRF (全端口寄存器文件)
    └── ReorderBuffer (重排序缓冲区)
```

### 前端设计

![前端架构图](KXCore_FrontEnd.png)

前端采用4级流水线设计，包含以下阶段：

#### Stage 0: 预取指阶段

- **PC生成**：顺序取指或者根据后端和多级分支预测期进行PC重定向选择。

#### Stage 1: 分支预测与取指阶段

##### 指令缓存 (ICache)
   
ICache是高性能指令获取的关键组件，采用3级流水线设计。我们的缓存支持灵活的参数配置，设置容量、相联度和块大小，用于性能调优。同时采用物理标记虚拟索引(PIPT)设计，避免别名问题。同时集成TLB访问，完整支持虚拟内存管理。支持CACOP指令做精确的缓存控制操作(缓存初始化、无效化、写回操作)和特权级访问控制。

##### 分支预测器系统器 (BranchPredictor)
   
我们实现了先进的混合分支预测器架构，包含了分支指令缓冲区(BIM)、分支目标缓冲区(BTB)和目标地址计算三个核心组件。
   
**分支指令缓冲区(BIM)** 

BIM拥有2048组的分支历史表，每个条目采用2位饱和计数器，使用了经典的2位饱和计数器预测分支方向算法，基于取指PC的低位进行组索引，同时支持每周期4条指令的并行预测。

```scala
// 2位饱和计数器更新逻辑
def bimWrite(v: UInt, taken: Bool): UInt = {
  val old_bim_sat_taken  = v === 3.U
  val old_bim_sat_ntaken = v === 0.U
  Mux(old_bim_sat_taken && taken, 3.U, 
    Mux(old_bim_sat_ntaken && !taken, 0.U, 
    Mux(taken, v + 1.U, v - 1.U)))
}
```
   
**分支目标缓冲区 (BTB)**：
我们的BTB采用了128组2路组相联结构，优化存储效率，目标地址计算支持相对偏移和绝对地址。使用LRU算法做缓存替换。使用EBTB专门处理长距离跳转的目标地址存储。

- **标签匹配**

```scala
val hitOHs = VecInit((0 until fetchWidth).map { i =>
  VecInit((0 until nWays).map { w =>
    io.req.bits.meta(w)(i).tag === tag
})
})
```

- **目标地址计算**
```scala
io.resp.bits.pred(i).target.bits := Mux(
  btb.extended,
  io.req.bits.ebtb,  // 扩展BTB用于长距离跳转
  params.fetchAlign(io.req.bits.fetchPC) + (i << log2Ceil(instBytes)).U +
  Sext(btb.offset, instWidth),  // 相对偏移计算
)
```

**预测融合与结果生成**

BIM和BTB想和配合，BIM提供方向预测，BTB提供目标和类型信息。条件分支(`isBr`)需要方向预测，无条件跳转 (`isJmp`)直接跳转，间接跳转结合BTB的目标预测。

```scala
io.resp.bits.pred zip bim.io.resp.bits.pred zip btb.io.resp.bits.pred foreach { 
  case ((resp, bim), btb) =>
    resp := btb                    // 基础信息来自BTB
    resp.taken := bim.taken        // 方向预测来自BIM
}
```

**并行处理与同步机制**
   
我们采用了双通道并行处理架构。
   
- **双通道设计**：
```scala
// ICache和BPU并行处理
icacheStage1.io.req.valid := pipeStage0to1.io.out.valid && 
                             icacheStage0to1.io.resp.valid
bpuStage1.io.req.valid := pipeStage0to1.io.out.valid && 
                          bpuStage0to1.io.resp.valid
```
   
- **数据流同步**：
     ```scala
     // 从Stage 0获取预处理数据
     bpuStage1.io.req.bits.bim := bpuStage0to1.io.resp.bits.bim
     bpuStage1.io.req.bits.btb.meta := bpuStage0to1.io.resp.bits.btb.meta
     ```
   
   - **握手协议**：确保两个通道的结果同步
```scala
val s1_fire = pipeStage0to1.io.out.valid && 
              icacheStage0to1.io.resp.valid && 
              bpuStage0to1.io.resp.valid &&
              icacheStage1to2.io.req.ready && 
              pipeStage1to2.io.in.ready
```

**即时重定向机制**
   
我们在Stage 1内部实现了快速的PC重定向，同时优先处理第一个预测跳转的指令
   
- **重定向条件检测**：
```scala
val s1_redirects = (0 until fetchWidth).map { i =>
  s1_fetchMask(i) && s1_bpuResp.pred(i).target.valid &&
  (s1_bpuResp.pred(i).isJmp ||
  (s1_bpuResp.pred(i).isBr && s1_bpuResp.pred(i).taken))
}
```
   
- **重定向目标选择**：
```scala
stage1Redirect.bits := Mux(
  s1_redirects.reduce(_ || _),
  s1_bpuResp.pred(PriorityEncoder(s1_redirects)).target.bits,
  nextFetch(s1_fetchPC),  // 顺序取指
)
```

#### Stage 2: 预解码阶段 

预解码阶段是KXCore前端设计的核心优化阶段，通过提前识别和处理控制流指令，显著提高分支预测精度和取指效率。

##### 控制流指令识别与分类

KXCore在预解码阶段实现了完整的控制流指令识别系统，识别分支指令(`isBr`)、跳转指令(`isB`)、间接跳转(`isJIRL`)、特殊指令(`isIDLE`)

##### 函数调用、返回识别和返回地址栈(RAS)管理

通过精确识别函数调用和返回模式，使用RAS专门优化函数调用和返回预测，提高分支预测准确率。我们的RAS有32个条目的深度栈，支持深度嵌套函数调用，函数调用时自动压入返回地址，返回时自动弹出预测地址，自动维护栈顶指针，支持栈回滚恢复。
   
- 函数调用识别(`isCall`)
```scala
// BL指令直接调用
(inst === BL.inst) || 
// JIRL r1, rj, 0 形式的调用
(inst === JIRL.inst && inst(4,0) === 1.U && inst(25,10) === 0.U)
```
   
- 函数返回识别 (`isRet`)
```scala
// JIRL r0, r1, 0 标准返回模式
inst === JIRL.inst && inst(4,0) === 0.U && 
inst(9,5) === 1.U && inst(25,10) === 0.U
```

- 检测到函数调用时，压入下一条指令地址

```scala
ras.io.write.valid := s2_fire && !s2_exception.valid && 
                      s2_fetchBundle.cfiIdx.valid && s2_callMask(s2_cfiIdx)
ras.io.write.addr := s2_pcs(s2_cfiIdx +& 1.U)
```

##### 控制流指令优先级处理
   
预解码阶段实现了智能的控制流指令选择机制，在取指宽度内检测第一个控制流指令，控制流指令后的指令被自动屏蔽，避免错误执行，根据控制流类型计算新的取指目标。
   
```scala
val s2_cfiMask = PriorityEncoderOH(VecInit((0 until fetchWidth).map { i =>
  s2_fetchMask(i) && (s2_jmpMask(i) || (s2_brMask(i) && s2_bpuResp.pred(i).taken))
}).asUInt)
```

##### 分支预测结果验证
   
预解码阶段对Stage 1的分支预测结果进行验证和修正，包括验证条件分支的taken/not-taken预测，验证跳转目标地址的正确性，并在发现预测错误时立即触发重定向

#### Stage 3: 发送指令阶段

发送指令阶段是前端流水线的最后一个阶段，主要负责缓冲前端处理结果并向后端提供稳定的指令流。该阶段包含两个关键组件：取指缓冲区(FetchBuffer)和取指目标队列(FTQ)。

##### 取指缓冲区 (FetchBuffer)

取指缓冲区是连接前端和后端的关键缓冲结构，将前端的取指包转换为后端可以处理的微操作序列。缓冲区总共32个条目，按行组织存储结构，每行包含2个条目(coreWidth=2)，共16行，支持高效的并行访问，采用一热编码的头尾指针，简化指针管理和冲突检测。

**入队操作机制**
取指缓冲区接收来自Stage 2的完整取指包，并转换为微操作格式。

- **取指包解析**：将FetchBundle中的4条指令逐一转换为MicroOp格式
```scala
for (i <- 0 until fetchWidth) {
  val pc = io.enq.bits.pcs(i)
  in_uops(i).pcLow := pc(log2Ceil(fetchBytes) - 1, 0)
  in_uops(i).inst  := io.enq.bits.insts(i)
  in_uops(i).ftqIdx := io.enq.bits.ftqIdx
  in_uops(i).isBr   := io.enq.bits.brMask(i)
  in_uops(i).isB    := io.enq.bits.bMask(i)
  in_uops(i).isJirl := io.enq.bits.jirlMask(i)
}
```

- **动态写入索引生成**：使用循环递增的方式为每条有效指令分配存储位置
```scala
var enq_idx = tail
for (i <- 0 until fetchWidth) {
  enq_idxs(i) := enq_idx
  enq_idx = Mux(in_mask(i), inc(enq_idx), enq_idx)
}
```

- **冲突检测机制**：检测写入操作是否会与读取头指针冲突
```scala
val do_enq = !(atHead && maybeFull || mightHitHead)
```

**出队操作机制**
缓冲区每周期向后端提供最多2条微操作(coreWidth=2)。

- **行级读取**：一次读取一整行数据，提供2个微操作给后端译码器
- **冲突避免**：检测读取操作是否会与写入尾指针冲突
```scala
val tail_collisions = VecInit((0 until fbNum).map(i => 
  head(i / coreWidth) && (!maybeFull || (i % coreWidth != 0).B))).asUInt & tail
val do_deq = io.deq.ready && !will_hit_tail
```

- **有效性管理**：为每个输出的微操作提供有效性标记，支持变长输出

**流水线刷新处理**
当收到流水线刷新信号时，立即清空所有缓冲区内容，将头尾指针重置为初始状态，为新的指令流做准备
```scala
when(io.flush) {
  head      := 1.U
  tail      := 1.U
  maybeFull := false.B
}
```

##### 取指目标队列 (FTQ)

取指目标队列是专门管理取指地址和分支预测信息的队列结构，为分支预测更新和异常处理提供关键支持。

**队列管理架构**
32个条目的环形队列，支持高吞吐量的取指操作，每个FTQ条目包含完整的取指和分支预测信息：

```scala
class FTQBundle extends Bundle {
  val fetchPC   = UInt(vaddrWidth.W)      // 取指PC地址
  val rasIdx    = UInt(log2Ceil(rasNum).W) // RAS栈指针
  val brMask    = UInt(fetchWidth.W)       // 分支指令掩码
  val cfiIdx    = Valid(UInt(log2Ceil(fetchWidth).W)) // 控制流指令索引
  val cfiIsB    = Bool()                   // 是否为无条件跳转
  val cfiIsBr   = Bool()                   // 是否为条件分支
  val cfiIsJirl = Bool()                   // 是否为间接跳转
  val meta      = new BranchPredictionMeta // 分支预测元数据
}
```

**入队操作处理**
FTQ与FetchBuffer同步入队，记录每次取指操作的完整信息：

```scala
when(do_enq) {
  val new_entry = Wire(new FTQBundle)
  new_entry.fetchPC      := io.enq.bits.pc
  new_entry.rasIdx       := io.enq.bits.rasIdx
  new_entry.brMask       := io.enq.bits.brMask
  new_entry.cfiIdx       := io.enq.bits.cfiIdx
  // 根据CFI类型设置控制流标记
  new_entry.cfiIsB       := io.enq.bits.bMask(io.enq.bits.cfiIdx.bits)
  new_entry.cfiIsBr      := io.enq.bits.brMask(io.enq.bits.cfiIdx.bits)
  new_entry.cfiIsJirl    := io.enq.bits.jirlMask(io.enq.bits.cfiIdx.bits)
}
```

**分支预测更新支持**
FTQ提供分支预测器的延迟更新机制，确保预测准确性，当bpu_ptr追上deq_ptr时，触发分支预测更新，使用下一个条目的fetchPC作为当前分支的目标地址

```scala
when(bpu_ptr =/= deq_ptr) {
  bpu_ptr := WrapInc(bpu_ptr, ftqNum)
  val bpuEntry = ram(bpu_ptr)
  val target   = ram(WrapInc(bpu_ptr, ftqNum)).fetchPC
  
  io.bpuUpdate.valid   := bpuEntry.cfiIdx.valid || bpuEntry.brMask =/= 0.U
  io.bpuUpdate.fetchPC := bpuEntry.fetchPC
  io.bpuUpdate.target  := target
}
```

**重定向和恢复机制**
FTQ支持分支预测错误和异常的快速恢复，当后端发生重定向时，回滚FTQ到正确的状态，同时提供RAS指针的恢复信息，确保函数调用栈的正确性。

```scala
when(io.redirect.valid) {
  enq_ptr    := WrapInc(io.redirect.idx, ftqNum)
  maybe_full := false.B
  
  // 如果有分支恢复信息，更新CFI状态
  when(io.redirect.brRecovery.valid) {
    redirect_new_entry.cfiIdx    := io.redirect.brRecovery.cfiIdx
    redirect_new_entry.cfiIsB    := io.redirect.brRecovery.cfiIsB
    redirect_new_entry.cfiIsBr   := io.redirect.brRecovery.cfiIsBr
    redirect_new_entry.cfiIsJirl := io.redirect.brRecovery.cfiIsJirl
  }
}
```

**后端接口支持**
FTQ为后端执行单元提供PC查询接口，支持3个并发的PC查询请求，满足多个执行单元的需求，为正在入队的条目提供实时的PC信息
```scala
for (i <- 0 until 3) {
  val idx = io.reqs(i)
  io.resps(i).valid := idx =/= enq_ptr || io.enq.fire
  io.resps(i).entry := ram(idx)
  when(idx === enq_ptr) {
    io.resps(i).entry.fetchPC := io.enq.bits.pc  // 实时数据
  }
}
```

### 后端设计

![后端架构图](KXCore_BackEnd.png)

#### Stage 0: 译码与重命名1阶段 
**主要模块**：[`Decoder`](../superscalar/src/KXCore/superscalar/core/backend/Decode/Decoder.scala)

- **指令译码**：将LoongArch32指令译码为内部微操作(MicroOp)
  - 支持算术运算、逻辑运算、访存、分支跳转、CSR操作等指令类型
  - 通过译码表确定指令的功能单元类型(FUType)和发射队列类型(IQType)
- **操作数识别**：识别源寄存器(lrs1/lrs2)和目标寄存器(ldst)
- **功能单元分配**：根据指令类型分配到对应的功能单元
  - ALU指令 → INT发射队列 → ALU执行单元
  - 访存指令 → MEM发射队列 → 内存执行单元
  - 乘除法指令 → UNQ发射队列 → 乘除法执行单元
  - CSR/系统指令 → UNQ发射队列 → 特殊执行单元
- **寄存器重命名**：逻辑寄存器到物理寄存器的映射，消除WAR和WAW冲突

#### Stage 1: 重命名2与分派阶段 
**主要模块**：[`BasicDispatcher`](../superscalar/src/KXCore/superscalar/core/backend/Dispatch/Dispatcher.scala)、重命名相关模块

- **依赖关系解析**：建立指令间的数据依赖关系，确保正确的执行顺序
- **物理寄存器分配**：
  - `RenameFreeList`：维护空闲物理寄存器列表
  - `RenameMapTable`：维护逻辑寄存器到物理寄存器的映射表
  - `RenameBusyTable`：跟踪物理寄存器的忙/闲状态
- **ROB分配**：为每条指令分配重排序缓冲区条目，支持按序提交
- **发射队列选择**：根据指令的IQType将指令分发到对应发射队列
  - 采用BasicDispatcher策略，假设最坏情况所有指令都发送到同一个队列
- **资源可用性检查**：确保有足够的物理寄存器、ROB条目和发射队列空间

#### Stage 2: 发射阶段
**主要模块**：发射单元 ([`IssueUnit`](../superscalar/src/KXCore/superscalar/core/backend/Issue/IssueUnit.scala))

KXCore配置了3个专用发射队列，采用乱序发射策略：

1. **内存发射队列 (MEM IQ)**：
   - 处理所有访存指令 (LD_B/LD_H/LD_W/ST_B/ST_H/ST_W等)
   - 发射宽度：1，队列深度：12
   - 支持地址计算和数据转发
   
2. **特殊发射队列 (UNQ IQ)**：
   - 处理特殊指令：
     - 乘除法指令 (MUL_W/MULH_W/DIV_W/MOD_W等)
     - CSR操作 (CSRRD/CSRWR/CSRXCHG系列)
     - 系统指令 (BREAK/SYSCALL/ERTN)
     - 计数器指令 (RDCNTID_W/RDCNTVH_W/RDCNTVL_W)
   - 发射宽度：1，队列深度：12
   
3. **整数发射队列 (INT IQ)**：
   - 处理整数运算指令 (ADD/SUB/AND/OR/XOR/SLT等)
   - 发射宽度：2，队列深度：20
   - 支持双发射，提高整数运算吞吐量

**发射机制**：
- 基于数据依赖的动态调度
- 支持唤醒机制，当操作数就绪时唤醒等待的指令
- 每个发射队列独立管理，避免队列间冲突

#### Stage 3: 执行阶段
**主要模块**：执行单元 ([`ExecutionUnit`](../superscalar/src/KXCore/superscalar/core/backend/Execute/ExecutionUnit.scala))

KXCore配置了4个执行单元，支持并行执行：

1. **ALU执行单元 (ALUExeUnit)** × 2：
   - 支持基本算术运算：ADD、SUB、SLT、SLTU等
   - 支持逻辑运算：AND、OR、XOR、NOR等
   - 支持移位运算：SLL、SRL、SRA等
   - 支持分支指令：BEQ、BNE、BLT、BGE等
   - 每个ALU单元支持1个读端口，可并行执行两条整数指令

2. **内存执行单元 (MemExeUnit)**：
   - 集成数据缓存(DCache)的访存执行单元
   - 支持字节、半字、字的加载存储操作
   - 集成地址计算和TLB访问
   - 通过AXI总线与外部内存交互
   - 支持异常处理（地址异常、TLB缺失等）

3. **特殊执行单元 (UniqueExeUnit)**：
   - **乘法器**：3级流水线Wallace乘法器，支持32位乘法
   - **除法器**：Booth除法器，支持有符号和无符号除法
   - **CSR操作**：完整的控制状态寄存器访问和管理
   - **系统指令**：BREAK、SYSCALL、ERTN等特权指令
   - **计数器访问**：稳定计数器读取

**执行特性**：
- 支持数据转发，减少数据冒险
- 异常检测和处理
- 分支预测验证和重定向

#### Stage 4: 写回阶段
**主要功能**：执行结果写回和状态更新

- **结果写回**：执行结果写回物理寄存器文件 (`FullyPortedRF`)
  - 支持多个写端口，对应不同执行单元
  - ALU单元：2个写端口
  - 内存单元：1个写端口  
  - 特殊单元：1个写端口（乘法）+ 1个写端口（除法）
- **唤醒机制**：通过写回端口唤醒等待相关数据的指令
  - 广播写回的物理寄存器号到所有发射队列
  - 更新发射队列中等待指令的操作数就绪状态
- **ROB更新**：将执行结果和状态写入重排序缓冲区
- **异常信息收集**：收集执行阶段产生的异常信息

#### Stage 5: 提交阶段
**主要模块**：重排序缓冲区 ([`ReorderBuffer`](../superscalar/src/KXCore/superscalar/core/backend/Commit/ReorderBuffer.scala))

- **按序提交**：保证指令按程序顺序提交，维护程序语义正确性
  - ROB采用环形缓冲区设计，支持32个条目
  - 每周期最多提交2条指令 (retireWidth = 2)
  - 只有ROB头部的指令才能提交
- **异常处理**：处理执行阶段的异常和中断
  - 支持精确异常：异常指令之前的指令正常提交，之后的指令被丢弃
  - 异常类型：地址异常、指令异常、系统调用、中断等
  - 异常处理时自动刷新流水线和分支预测状态
- **分支重定向**：处理分支预测错误
  - 验证分支预测结果，如有错误触发重定向
  - 刷新错误路径上的所有指令
  - 恢复分支预测器状态和寄存器重命名状态
- **架构状态更新**：更新架构可见的处理器状态
  - 释放已提交指令占用的物理寄存器
  - 更新程序计数器(PC)
  - 更新CSR寄存器状态
  - 向前端发送提交信息，更新取指目标队列(FTQ)

## 关键优化设计

### 乱序执行核心机制

#### 寄存器重命名系统
KXCore采用了业界先进的寄存器重命名技术：
- **物理寄存器池**：80个物理寄存器 vs 32个逻辑寄存器，提供2.5倍的寄存器容量
- **重命名映射表 (RenameMapTable)**：维护逻辑到物理寄存器的动态映射
  - 支持回滚操作，异常时快速恢复寄存器状态
  - 提交时的映射更新和旧寄存器释放
- **空闲列表 (RenameFreeList)**：高效管理可用物理寄存器
  - 支持每周期2个寄存器的分配和释放
  - 采用循环队列设计，避免分配冲突
- **忙表 (RenameBusyTable)**：跟踪寄存器数据就绪状态
  - 与唤醒机制紧密集成，支持快速状态更新
  - 消除WAR和WAW数据冲突，提高并行度

#### 动态调度机制
- **基于数据流的调度**：指令根据操作数就绪状态动态发射
- **年龄矩阵选择**：保证老指令优先发射，维护程序语义
- **唤醒传播网络**：写回结果快速传播到所有等待指令
- **发射队列独立管理**：3个发射队列并行工作，提高发射带宽

#### 推测执行与恢复
- **分支预测推测**：基于分支预测结果的推测执行
- **检查点机制**：关键状态的快照和恢复
- **流水线刷新**：预测错误时的快速流水线清空
- **状态回滚**：寄存器重命名状态的精确恢复

#### 精确异常处理
- **ROB序列化**：通过重排序缓冲区保证指令按序提交
- **异常检测点**：在提交阶段统一处理所有异常
- **上下文保存**：异常时的完整处理器状态保存
- **异常向量表**：支持多种异常类型的向量化处理
- **数据依赖检测**：在发射队列中检测操作数就绪状态
- **唤醒机制**：写回结果时广播唤醒依赖指令
- **乱序发射**：操作数就绪的指令可以乱序发射执行
- **按序提交**：通过ROB保证指令按程序顺序提交

### 分支预测机制
KXCore采用了先进的混合分支预测器架构，实现高精度的分支预测：

#### 多级分支预测器组合
- **分支指令缓冲区 (BIM)**：2048组的分支历史表，采用2位饱和计数器预测分支方向
  - 支持动态学习和更新机制，能够适应不同的分支行为模式
  - 通过双端口设计支持并发读写操作
- **分支目标缓冲区 (BTB)**：128组2路组相联设计，预测分支目标地址
  - 支持扩展BTB设计，处理长距离跳转的目标地址存储
  - 采用LRU替换策略，优化缓存命中率
  - 集成分支类型识别（条件分支、无条件跳转、间接跳转）
- **返回地址栈 (RAS)**：32条目的函数调用栈，专门优化函数返回预测
  - 硬件实现的栈结构，避免软件栈管理开销
  - 支持深度嵌套函数调用的高精度预测

#### 高级预测特性
- **分支预测融合**：BIM和BTB协同工作，分别处理方向和目标预测
- **预测失败恢复**：支持分支预测失败时的快速恢复机制和状态回滚
- **多取指支持**：支持每周期4条指令的并行分支预测
- **预测更新策略**：采用写回阶段的延迟更新，避免预测器状态不一致

### 缓存系统
KXCore实现了高性能的缓存层次结构：

#### 指令缓存 (ICache)
- **缓存配置**：支持可配置的容量和相联度
- **预取机制**：支持指令预取和缓存一致性协议
- **多级流水线**：3级流水线设计，优化访问延迟
- **CACOP指令支持**：支持缓存操作指令，实现精确的缓存控制

#### 数据缓存 (DCache) 
- **集成设计**：与内存执行单元紧密集成
- **写回策略**：支持写回和写穿策略
- **缓存一致性**：支持多级缓存一致性协议
- **异常处理**：集成缓存异常检测和处理机制

### 虚拟内存管理系统

#### 高性能TLB设计
KXCore实现了完整的LoongArch32虚拟内存管理：
- **TLB条目管理**：支持可配置数量的TLB条目
- **页面大小支持**：同时支持4KB和2MB页面
- **地址空间标识符(ASID)**：10位ASID支持多进程地址空间隔离
- **全局页面支持**：支持全局页面标记，优化内核页面访问
- **特权级检查**：硬件特权级验证，支持4级特权模式

#### TLB操作指令
- **TLBSRCH**：TLB搜索指令，支持虚拟地址查找
- **TLBRD**：TLB读取指令，读取指定TLB条目
- **TLBWR**：TLB写入指令，更新指定TLB条目  
- **TLBFILL**：TLB填充指令，处理TLB缺失
- **INVTLB**：TLB无效指令，支持多种无效策略

#### 内存访问类型(MAT)
- **缓存一致性控制**：支持多种内存访问类型
- **页面权限管理**：读、写、执行权限的硬件检查
- **脏位管理**：自动脏位更新和检查

#### 直接映射窗口(DMW)
- **快速地址转换**：绕过TLB的直接地址映射
- **特权级控制**：支持不同特权级的访问控制
- **段式地址映射**：支持虚拟段到物理段的直接映射

### 高性能算术运算单元

#### Wallace树乘法器
KXCore实现了先进的Wallace树乘法器：
- **Booth编码优化**：采用Booth-2编码减少部分积数量
- **3级流水线设计**：深度流水线优化，支持每周期一次乘法操作
- **混合精度支持**：支持有符号和无符号32位乘法
- **并行压缩树**：使用压缩树结构减少加法器延迟

#### Booth除法器
高效的除法运算实现：
- **Booth算法**：支持有符号和无符号除法
- **逐位除法**：32位数据宽度的完整除法支持
- **零除检测**：硬件级零除异常检测和处理
- **余数计算**：同时输出商和余数，支持MOD指令

### 高级系统特性

#### 完整的CSR系统
KXCore实现了LoongArch32的完整控制状态寄存器系统：
- **处理器状态管理**：CRMD、PRMD、EUEN等状态寄存器
- **异常处理寄存器**：ERA、EENTRY、ECFG、ESTAT等
- **TLB管理寄存器**：TLBIDX、TLBEHI、TLBELO0/1等
- **定时器系统**：TCFG、TVAL、TICLR定时器寄存器
- **稳定计数器**：64位高精度系统计数器

#### 中断处理系统
- **8位中断输入**：支持多种中断源
- **中断优先级**：硬件中断优先级仲裁
- **中断屏蔽**：灵活的中断使能和屏蔽控制
- **中断向量化**：支持向量化中断处理

#### 调试和验证支持
- **Difftest接口**：完整的Difftest协议支持，便于功能验证
- **性能计数器**：内置性能监控和分析功能
- **调试寄存器**：支持实时寄存器状态监控
- **波形调试**：详细的内部信号输出，支持深度调试

#### 先进的设计方法学
- **参数化设计**：高度可配置的硬件参数
- **模块化架构**：清晰的模块边界和接口定义
- **流水线设计**：深度优化的10级流水线
- **时钟域管理**：统一的时钟和复位设计
  - 异常指令前的指令正常提交
  - 异常指令后的指令被取消
- **中断处理**：支持8位中断输入，完整的中断控制机制

## 技术创新与亮点

### 1. 先进的微架构设计
- **深度乱序执行**：10级流水线设计，支持高效的乱序执行
- **多发射架构**：3个发射队列，4个执行单元的并行处理能力
- **智能调度算法**：基于数据流的动态调度，最大化指令级并行

### 2. 高性能算术单元
- **Wallace树乘法器**：采用先进的Booth编码和压缩树技术
- **流水线除法器**：高效的Booth除法器实现
- **并行ALU设计**：支持双发射的整数运算单元

### 3. 先进的分支预测
- **混合预测器**：BIM、BTB、RAS的协同预测
- **高容量设计**：2048组BIM + 128组BTB，提供高预测精度
- **快速恢复机制**：预测失败时的高效状态恢复

### 主要数据结构

#### 微操作 (MicroOp)
KXCore的核心数据结构，包含指令执行所需的所有信息：
- **基本信息**：PC、指令编码、ROB索引、FTQ索引
- **寄存器信息**：逻辑源寄存器(lrs1/lrs2)、逻辑目标寄存器(ldst)
- **重命名信息**：物理源寄存器(prs1/prs2)、物理目标寄存器(pdst)
- **控制信息**：功能单元类型、发射队列类型、执行命令
- **调试信息**：用于调试和验证的附加信息

#### 发射队列槽位 (IssueSlot)
发射队列的基本单元，管理单条指令的发射：
- **有效性**：指示槽位是否包含有效指令
- **就绪检测**：检查操作数是否就绪
- **唤醒逻辑**：响应写回端口的唤醒信号
- **发射授权**：参与发射仲裁并获得执行许可

### 关键算法

#### 发射选择算法
采用年龄矩阵(Age Matrix)或优先级编码器选择最老的就绪指令：
```
for each issue queue:
  1. 找出所有操作数就绪的指令
  2. 在就绪指令中选择最老的指令
  3. 根据发射宽度选择多条指令发射
  4. 更新发射队列状态
```

#### 唤醒算法
写回结果时广播唤醒依赖指令：

对于每次写回
  1. 广播写回的物理寄存器号
  2. 所有发射队列检查依赖关系
  3. 更新等待该寄存器的指令状态
  4. 标记操作数就绪的指令可发射


### 性能特性

### 性能特性与优化

### 指令吞吐量优化
- **理论峰值性能**：每周期最多取指4条、译码2条、发射4条、提交2条指令
- **高并发设计**：
  - 前端：4路并行取指，支持跨Cache Line取指
  - 后端：3个独立发射队列，4个并行执行单元
  - 提交：2路并行提交，支持高吞吐量指令退休

### 延迟优化策略
- **数据转发网络**：执行单元间的快速数据转发，减少数据冒险延迟
- **分支预测优化**：多级分支预测器减少分支延迟惩罚
- **缓存层次优化**：指令和数据缓存的智能预取和替换策略
- **流水线平衡**：前端4级+后端6级的平衡设计，避免流水线气泡

### 功耗管理
- **时钟门控**：细粒度的时钟门控设计，降低动态功耗
- **模块化电源**：支持模块级电源管理
- **流水线优化**：减少不必要的状态转换和数据移动

### 面积效率
- **资源复用**：多个功能单元间的资源共享
- **存储器优化**：高效的存储器设计和布局
- **逻辑优化**：深度的逻辑综合和优化

## 技术实现细节

## 项目结构说明

### 源代码组织
```
KXCore/
├── common/                    # 通用模块
│   ├── src/KXCore/common/
│   │   ├── Instruction.scala  # 指令定义和解码
│   │   ├── Parameters.scala   # 参数配置
│   │   ├── Stage.scala       # 流水线阶段基类
│   │   ├── peripheral/       # 外设接口
│   │   └── utils/           # 工具类
├── superscalar/              # 超标量架构实现
│   ├── src/KXCore/superscalar/
│   │   ├── Parameters.scala  # 超标量参数
│   │   ├── Top.scala        # 顶层模块
│   │   └── core/            # 核心实现
│   │       ├── Core.scala   # 核心主模块
│   │       ├── frontend/    # 前端模块
│   │       └── backend/     # 后端模块
└── build/                   # 生成的Verilog文件
```

### 构建流程
1. **Scala编译**：使用mill构建工具编译Scala源代码
2. **Chisel生成**：通过Chisel将Scala硬件描述转换为Verilog
3. **SystemVerilog输出**：生成可综合的SystemVerilog代码
4. **仿真验证**：使用生成的代码进行功能和性能验证

### 配置参数
主要配置参数定义在`Parameters.scala`中：
- **CommonParameters**：通用参数（数据宽度、地址宽度等）
- **FrontendParameters**：前端参数（取指宽度、缓冲区大小等）
- **BackendParameters**：后端参数（发射宽度、ROB大小等）
- **CoreParameters**：整体核心参数，组合各模块参数

### 扩展性设计
- **参数化设计**：大部分配置通过参数控制，便于定制
- **模块化结构**：各功能模块独立，便于替换和升级
- **接口标准化**：使用标准的Chisel接口，便于集成
- **多核扩展**：设计支持多核扩展的可能性
