# 处理详细 - INSPOLVD 子程序

## 程序概述

INSPOLVD 是保险会计系统的子程序，负责验证保单ID是否存在于数据库中。该子程序接收保单ID作为输入，查询保单表，并返回SQL操作结果代码，表示保单是否存在。在模拟环境中，通过检查预定义的保单ID列表来模拟数据库查询操作。

## 处理详细说明

### 主程序框架 (0000-MAIN-PROCESS)

```cobol
0000-MAIN-PROCESS.
    MOVE 0 TO WS-POLICY-COUNT
    
    IF LS-POLICY-ID = 'P000000001' OR 
       LS-POLICY-ID = 'P000000002' OR
       LS-POLICY-ID = 'P000000003' OR
       LS-POLICY-ID = 'P000000004' OR
       LS-POLICY-ID = 'P000000005' OR
       LS-POLICY-ID = 'P000000006' OR
       LS-POLICY-ID = 'P000000007' OR
       LS-POLICY-ID = 'P000000008'
        MOVE 1 TO WS-POLICY-COUNT
        MOVE 0 TO SQLCODE
    ELSE
        MOVE 0 TO WS-POLICY-COUNT
        MOVE 100 TO SQLCODE
    END-IF
    
    MOVE SQLCODE TO LS-SQLCODE
    
    GOBACK.
```

#### 处理步骤说明：

1. **初始化计数器**：
   - 将保单计数器 WS-POLICY-COUNT 初始化为 0

2. **检查保单ID是否存在**：
   - 比较输入的保单ID (LS-POLICY-ID) 与预定义的有效保单ID列表
   - 预定义的有效保单ID有：'P000000001'至'P000000008'

3. **设置返回结果**：
   - 如果保单ID存在于有效列表中：
     - 将保单计数器 WS-POLICY-COUNT 设置为 1
     - 将 SQLCODE 设置为 0，表示成功
   - 如果保单ID不存在于有效列表中：
     - 保持保单计数器 WS-POLICY-COUNT 为 0
     - 将 SQLCODE 设置为 100，表示未找到记录

4. **返回结果**：
   - 将 SQLCODE 复制到输出参数 LS-SQLCODE
   - 返回到调用程序

## 实际环境中的SQL操作

在实际的DB2环境中，上述模拟逻辑将替换为类似如下的SQL操作：

```cobol
EXEC SQL
    SELECT COUNT(*)
    INTO :WS-POLICY-COUNT
    FROM INSURANCE_POLICY
    WHERE POLICY_ID = :LS-POLICY-ID
    AND POLICY_STATUS = 'A'
END-EXEC.

IF SQLCODE = 0 AND WS-POLICY-COUNT > 0
    MOVE 0 TO LS-SQLCODE
ELSE
    MOVE SQLCODE TO LS-SQLCODE
END-IF.
```

此操作将：
1. 查询 INSURANCE_POLICY 表，计算符合条件的记录数量
2. 条件是保单ID匹配输入的值，且保单状态为有效('A')
3. 查询结果存储到 WS-POLICY-COUNT 变量
4. 根据 SQL 操作结果和查询到的记录数，设置返回代码

## 输入输出说明

### 输入
- **LS-POLICY-ID**: 保单ID，PIC X(10)
  - 要验证的保单标识符

### 输出
- **LS-SQLCODE**: SQL返回代码，PIC S9(9) COMP
  - 0: 保单存在
  - 100: 保单不存在
  - 其他值: 发生SQL错误

## 数据验证规则

保单验证规则包括：
1. 保单ID必须存在于保单表中
2. 实际环境中，通常还会检查保单状态是否为有效状态

## 处理流程图

```
开始
  |
  v
初始化保单计数器(WS-POLICY-COUNT = 0)
  |
  v
检查保单ID是否在有效列表中
  |
  +--> 是 ---> 设置保单计数为1 ---> 设置SQLCODE = 0
  |                                    |
  +--> 否 ---> 保持保单计数为0 ---> 设置SQLCODE = 100
                                      |
                                      v
                              将SQLCODE复制到输出参数
                                      |
                                      v
                                    结束
```

## 注意事项

1. 此子程序在模拟环境中使用硬编码的保单ID列表，以模拟数据库查询。在实际生产环境中，应使用SQL查询实际数据库表。

2. 保单验证是确保支付记录有效性的关键步骤，防止系统处理不存在保单的支付记录。

3. SQL返回代码的处理遵循DB2数据库的标准约定：
   - SQLCODE = 0: 操作成功
   - SQLCODE = 100: 未找到记录
   - SQLCODE < 0: 发生错误 