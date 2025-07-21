# 处理详细 - INSPMINS 子程序

## 程序概述

INSPMINS 是保险会计系统的子程序，负责将支付记录插入到数据库中。该子程序接收支付记录结构和会计期间作为输入，向保费支付表插入记录，并返回SQL操作结果代码。在模拟环境中，通过检查特定支付ID来模拟数据库插入操作。

## 处理详细说明

### 主程序框架 (0000-MAIN-PROCESS)

```cobol
0000-MAIN-PROCESS.
    MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-DATA
    STRING WS-CURRENT-YEAR
           WS-CURRENT-MONTH
           WS-CURRENT-DAY
           WS-CURRENT-HOUR
           WS-CURRENT-MINUTE
           WS-CURRENT-SECOND
      DELIMITED BY SIZE
      INTO WS-TIMESTAMP
    END-STRING
    
    IF PR-PAYMENT-ID = 'PAY000000011' OR
       PR-PAYMENT-ID = 'PAY000000013' OR
       PR-PAYMENT-ID = 'PAY000000015'
        MOVE 0 TO SQLCODE
    ELSE
        MOVE -803 TO SQLCODE
    END-IF
    
    MOVE SQLCODE TO LS-SQLCODE
    
    GOBACK.
```

#### 处理步骤说明：

1. **生成时间戳**：
   - 使用 FUNCTION CURRENT-DATE 获取系统当前日期和时间
   - 将年、月、日、时、分、秒组合成时间戳字符串（YYYYMMDDHHMMSS格式）
   - 存储到 WS-TIMESTAMP 变量中

2. **模拟数据库插入操作**：
   - 检查支付ID (PR-PAYMENT-ID) 是否为特定值
   - 如果支付ID是 'PAY000000011'、'PAY000000013' 或 'PAY000000015'：
     - 将 SQLCODE 设置为 0，表示插入成功
   - 否则：
     - 将 SQLCODE 设置为 -803，表示记录已存在（模拟插入失败）

3. **返回结果**：
   - 将 SQLCODE 复制到输出参数 LS-SQLCODE
   - 返回到调用程序

## 实际环境中的SQL操作

在实际的DB2环境中，上述模拟逻辑将替换为类似如下的SQL操作：

```cobol
EXEC SQL
    INSERT INTO PREMIUM_PAYMENT
        (PAYMENT_ID,
         POLICY_ID,
         PAYMENT_DATE,
         AMOUNT,
         PAYMENT_METHOD,
         PAYMENT_STATUS,
         ACCOUNTING_PERIOD,
         REFERENCE_NO,
         CREATE_USER,
         CREATE_TIMESTAMP)
    VALUES
        (:PR-PAYMENT-ID,
         :PR-POLICY-ID,
         :PR-PAYMENT-DATE,
         :PR-AMOUNT,
         :PR-PAYMENT-METHOD,
         'P',
         :LS-ACCOUNTING-PERIOD,
         :PR-REFERENCE-NO,
         'INSACC',
         :WS-TIMESTAMP)
END-EXEC.

MOVE SQLCODE TO LS-SQLCODE.
```

此操作将：
1. 向 PREMIUM_PAYMENT 表插入新的支付记录
2. 插入的字段包括支付ID、保单ID、支付日期、金额、支付方式等
3. 设置支付状态为 'P'（已处理）
4. 使用生成的时间戳作为创建时间
5. 将SQL操作结果代码返回给调用程序

## 输入输出说明

### 输入
- **PAYMENT-RECORD**: 支付记录结构
  - PR-POLICY-ID: 保单ID，PIC X(10)
  - PR-PAYMENT-DATE: 支付日期，PIC X(8)
  - PR-PAYMENT-METHOD: 支付方式，PIC X(2)
  - PR-AMOUNT: 支付金额，PIC 9(8)V99
  - PR-PAYMENT-ID: 支付ID，PIC X(12)
  - PR-REFERENCE-NO: 参考编号，PIC X(16)
- **LS-ACCOUNTING-PERIOD**: 会计期间，PIC X(6)
  - 格式为YYYYMM

### 输出
- **LS-SQLCODE**: SQL返回代码，PIC S9(9) COMP
  - 0: 插入成功
  - -803: 记录已存在（重复键）
  - 其他负值: 其他SQL错误

## 数据处理逻辑

支付记录插入过程包括：
1. 生成当前时间戳，用于记录创建时间
2. 将支付记录和相关信息插入到保费支付表
3. 使用传入的会计期间关联财务会计处理
4. 设置支付状态为已处理('P')
5. 记录创建用户和创建时间

## 处理流程图

```
开始
  |
  v
生成当前时间戳(WS-TIMESTAMP)
  |
  v
检查支付ID是否为特定值
  |
  +--> 是 ---> 设置SQLCODE = 0 (成功)
  |                |
  +--> 否 ---> 设置SQLCODE = -803 (记录已存在)
                  |
                  v
            将SQLCODE复制到输出参数
                  |
                  v
                结束
```

## 注意事项

1. 此子程序在模拟环境中使用硬编码的支付ID检查，以模拟数据库插入操作。在实际生产环境中，应使用SQL插入语句操作实际数据库表。

2. 在实际环境中，常见的插入错误有：
   - SQLCODE = -803: 违反唯一键约束（重复记录）
   - SQLCODE = -530: 违反外键约束（引用了不存在的保单）
   - SQLCODE = -407: 插入了NULL值到NOT NULL列

3. 生成的时间戳格式为YYYYMMDDHHMMSS，符合DB2数据库的常见时间戳格式要求。

4. 会计期间参数用于财务报表生成和会计核算，通常为当前年月（YYYYMM）。 