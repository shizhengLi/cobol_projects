 # 处理详细 - INSPMUPD 主程序

## 程序概述

INSPMUPD_EN 是保险会计系统的主程序，负责处理保费支付记录。程序从输入文件读取支付记录，验证保单ID是否存在，将有效的支付记录插入数据库，并生成处理报告。

## 处理详细说明

### 主程序框架 (0000-MAIN-PROCESS)

```cobol
0000-MAIN-PROCESS.
    PERFORM 1000-INIT-PROCESS
    PERFORM 2000-PROCESS-DATA UNTIL END-OF-FILE
    PERFORM 3000-END-PROCESS
    GOBACK.
```

- 调用初始化处理段 1000-INIT-PROCESS
- 重复调用数据处理段 2000-PROCESS-DATA，直到文件结束
- 调用结束处理段 3000-END-PROCESS
- 结束程序执行并返回

### 初始化处理 (1000-INIT-PROCESS)

```cobol
1000-INIT-PROCESS.
    MOVE 'N' TO WS-EOF-FLAG
    MOVE ZEROS TO WS-READ-COUNT
                  WS-PROCESS-COUNT
                  WS-ERROR-COUNT
                  
    MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-DATA
    
    STRING WS-CURRENT-YEAR
           WS-CURRENT-MONTH
      DELIMITED BY SIZE
      INTO WS-ACCOUNTING-PERIOD
    END-STRING
    
    OPEN INPUT PAYMENT-FILE
    OPEN OUTPUT REPORT-FILE
    
    MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-DATA
    STRING WS-CURRENT-YEAR DELIMITED BY SIZE
           '-'
           WS-CURRENT-MONTH DELIMITED BY SIZE
           '-'
           WS-CURRENT-DAY DELIMITED BY SIZE
      INTO HL-DATE
    END-STRING
    
    STRING WS-CURRENT-HOUR DELIMITED BY SIZE
           ':'
           WS-CURRENT-MINUTE DELIMITED BY SIZE
           ':'
           WS-CURRENT-SECOND DELIMITED BY SIZE
      INTO HL-TIME
    END-STRING
    
    WRITE REPORT-LINE FROM HL-HEADING-LINE-1
    WRITE REPORT-LINE FROM HL-HEADING-LINE-2
    WRITE REPORT-LINE FROM HL-HEADING-LINE-3
    WRITE REPORT-LINE FROM HL-HEADING-LINE-2
    
    MOVE 'I' TO LOG-LEVEL
    MOVE 'INSPMUPD' TO LOG-MODULE
    MOVE 'Premium payment processing started' TO LOG-MESSAGE
    CALL 'INSPMLOG_EN' USING LOG-PARAMS
    
    PERFORM 2100-READ-PAYMENT-FILE.
```

#### 处理步骤说明：

1. **初始化变量和计数器**：
   - 设置文件结束标志 WS-EOF-FLAG 为 'N'（未结束）
   - 初始化读取计数器、处理计数器和错误计数器为 0

2. **获取当前日期和时间**：
   - 使用 FUNCTION CURRENT-DATE 获取系统日期时间
   - 将结果存储到 WS-CURRENT-DATE-DATA 结构中

3. **设置会计期间**：
   - 将当前年月组合成会计期间（YYYYMM格式）
   - 存储到 WS-ACCOUNTING-PERIOD 变量中

4. **打开文件**：
   - 打开输入文件 PAYMENT-FILE
   - 打开输出文件 REPORT-FILE

5. **生成报表头部**：
   - 格式化当前日期为 YYYY-MM-DD 格式
   - 格式化当前时间为 HH:MM:SS 格式
   - 写入报表标题行、分隔行和列标题行

6. **记录开始日志**：
   - 设置日志级别为 'I'（信息）
   - 设置模块名为 'INSPMUPD'
   - 设置日志消息为 'Premium payment processing started'
   - 调用 INSPMLOG_EN 子程序记录日志

7. **读取第一条记录**：
   - 调用 2100-READ-PAYMENT-FILE 段读取第一条支付记录

### 数据处理 (2000-PROCESS-DATA)

```cobol
2000-PROCESS-DATA.
    ADD 1 TO WS-PROCESS-COUNT
    
    MOVE PAYMENT-RECORD-IN(1:10) TO PR-POLICY-ID
    MOVE PAYMENT-RECORD-IN(11:8) TO PR-PAYMENT-DATE
    MOVE PAYMENT-RECORD-IN(19:2) TO PR-PAYMENT-METHOD
    
    COMPUTE PR-AMOUNT = FUNCTION NUMVAL(PAYMENT-RECORD-IN(21:10))
    
    MOVE PAYMENT-RECORD-IN(31:12) TO PR-PAYMENT-ID
    MOVE PAYMENT-RECORD-IN(43:16) TO PR-REFERENCE-NO
    
    CALL 'INSPOLVD' USING PR-POLICY-ID SQLCODE
    
    IF SQLCODE = 0
        MOVE SPACES TO DL-RESULT
        PERFORM 2200-INSERT-PAYMENT
        IF SQLCODE = 0
            MOVE 'SUCCESS' TO DL-RESULT
        ELSE
            MOVE 'FAILED' TO DL-RESULT
            ADD 1 TO WS-ERROR-COUNT
        END-IF
    ELSE
        MOVE 'NO POLICY' TO DL-RESULT
        ADD 1 TO WS-ERROR-COUNT
    END-IF
    
    MOVE PR-POLICY-ID TO DL-POLICY-ID
    
    STRING PR-PAYMENT-DATE(1:4) DELIMITED BY SIZE
           '-'
           PR-PAYMENT-DATE(5:2) DELIMITED BY SIZE
           '-'
           PR-PAYMENT-DATE(7:2) DELIMITED BY SIZE
      INTO DL-PAYMENT-DATE
    END-STRING
    
    EVALUATE PR-PAYMENT-METHOD
        WHEN '01'
            MOVE 'BANK' TO DL-PAYMENT-METHOD
        WHEN '02'
            MOVE 'CREDIT' TO DL-PAYMENT-METHOD
        WHEN '03'
            MOVE 'CASH' TO DL-PAYMENT-METHOD
        WHEN OTHER
            MOVE 'UNKNOWN' TO DL-PAYMENT-METHOD
    END-EVALUATE
    
    MOVE PR-AMOUNT TO DL-AMOUNT
    MOVE PR-PAYMENT-ID TO DL-PAYMENT-ID
    MOVE PR-REFERENCE-NO TO DL-REFERENCE-NO
    
    WRITE REPORT-LINE FROM DL-DETAIL-LINE
    
    PERFORM 2100-READ-PAYMENT-FILE.
```

#### 处理步骤说明：

1. **增加处理计数**：
   - 处理计数器 WS-PROCESS-COUNT 加 1

2. **解析支付记录字段**：
   - 从输入记录 PAYMENT-RECORD-IN 中提取各个字段
   - 保单ID：取第1-10位
   - 支付日期：取第11-18位
   - 支付方式：取第19-20位
   - 支付金额：取第21-30位，并使用 FUNCTION NUMVAL 转换为数值
   - 支付ID：取第31-42位
   - 参考编号：取第43-58位

3. **验证保单**：
   - 调用 INSPOLVD 子程序，传入保单ID和SQL返回代码参数
   - 子程序验证保单ID是否存在于保单表中

4. **保单验证后处理**：
   - 如果保单存在（SQLCODE = 0）：
     - 调用 2200-INSERT-PAYMENT 段插入支付记录
     - 根据插入结果设置处理结果（SUCCESS 或 FAILED）
     - 如果插入失败，增加错误计数
   - 如果保单不存在：
     - 设置处理结果为 'NO POLICY'
     - 增加错误计数

5. **格式化输出数据**：
   - 设置报表明细行的各个字段
   - 格式化支付日期为 YYYY-MM-DD 格式
   - 根据支付方式代码转换为文本描述（BANK、CREDIT、CASH 或 UNKNOWN）
   - 设置金额、支付ID和参考编号

6. **写入报表明细行**：
   - 将格式化后的明细行写入报表文件

7. **读取下一条记录**：
   - 调用 2100-READ-PAYMENT-FILE 段读取下一条支付记录

### 读取支付文件 (2100-READ-PAYMENT-FILE)

```cobol
2100-READ-PAYMENT-FILE.
    READ PAYMENT-FILE INTO PAYMENT-RECORD-IN
        AT END
            MOVE 'Y' TO WS-EOF-FLAG
        NOT AT END
            ADD 1 TO WS-READ-COUNT
    END-READ.
```

#### 处理步骤说明：

1. **读取支付记录**：
   - 从输入文件 PAYMENT-FILE 读取一条记录到 PAYMENT-RECORD-IN

2. **处理文件结束**：
   - 如果达到文件末尾，设置文件结束标志 WS-EOF-FLAG 为 'Y'
   - 如果未达到文件末尾，增加读取计数器 WS-READ-COUNT

### 插入支付记录 (2200-INSERT-PAYMENT)

```cobol
2200-INSERT-PAYMENT.
    CALL 'INSPMINS' USING PAYMENT-RECORD
                           WS-ACCOUNTING-PERIOD
                           SQLCODE.
```

#### 处理步骤说明：

1. **调用插入子程序**：
   - 调用 INSPMINS 子程序插入支付记录
   - 传入支付记录、会计期间和SQL返回代码参数
   - 子程序将记录插入到保费支付表中

### 结束处理 (3000-END-PROCESS)

```cobol
3000-END-PROCESS.
    MOVE WS-PROCESS-COUNT TO TL-TOTAL-PROCESSED
    MOVE WS-ERROR-COUNT TO TL-TOTAL-ERRORS
    
    WRITE REPORT-LINE FROM HL-HEADING-LINE-2
    WRITE REPORT-LINE FROM TL-TOTAL-LINE
    
    MOVE 'I' TO LOG-LEVEL
    MOVE 'INSPMUPD' TO LOG-MODULE
    STRING 'Premium payment processing batch ended. Processed: ' 
           WS-PROCESS-COUNT
           ' Errors: ' 
           WS-ERROR-COUNT
      DELIMITED BY SIZE
      INTO LOG-MESSAGE
    END-STRING
    CALL 'INSPMLOG_EN' USING LOG-PARAMS
    
    CLOSE PAYMENT-FILE
    CLOSE REPORT-FILE.
```

#### 处理步骤说明：

1. **生成报表汇总**：
   - 将处理计数器值设置到报表汇总行的处理总数字段
   - 将错误计数器值设置到报表汇总行的错误总数字段
   - 写入分隔行和汇总行到报表文件

2. **记录结束日志**：
   - 设置日志级别为 'I'（信息）
   - 设置模块名为 'INSPMUPD'
   - 设置日志消息，包含处理总数和错误总数信息
   - 调用 INSPMLOG_EN 子程序记录日志

3. **关闭文件**：
   - 关闭输入文件 PAYMENT-FILE
   - 关闭输出文件 REPORT-FILE

## 输入输出说明

### 输入
- **PAYMENT-FILE**: 保费支付更新文件，包含保费支付记录
  - 格式: 固定长度记录，每条58字节
  - 字段: 保单ID(10)、支付日期(8)、支付方式(2)、支付金额(10)、支付ID(12)、参考编号(16)

### 输出
- **REPORT-FILE**: 处理报告文件，包含处理结果
  - 格式: 固定长度记录，每条132字节
  - 内容: 标题行、列标题行、明细行(每条支付记录一行)、汇总行(处理总数和错误总数)

## 调用的子程序
- **INSPOLVD**: 保单验证子程序
- **INSPMINS**: 支付记录插入子程序
- **INSPMLOG_EN**: 日志记录子程序

## 异常处理
- 如果保单验证失败，设置处理结果为 'NO POLICY'，增加错误计数器
- 如果支付记录插入失败，设置处理结果为 'FAILED'，增加错误计数器
- 所有处理结果记录在报表文件中
- 处理统计信息记录在日志中