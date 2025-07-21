# 处理详细 - INSPMLOG_EN 子程序

## 程序概述

INSPMLOG_EN 是保险会计系统的子程序，负责记录程序执行过程中的日志信息。该子程序接收日志参数（包括日志级别、模块名和消息），将格式化的日志记录写入到日志文件中。日志记录包含时间戳、级别标识、模块名和消息内容，为问题排查和审计提供支持。

## 处理详细说明

### 主程序框架 (0000-MAIN-PROCESS)

```cobol
0000-MAIN-PROCESS.
    MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-DATA
    STRING WS-CURRENT-YEAR DELIMITED BY SIZE
           '-'
           WS-CURRENT-MONTH DELIMITED BY SIZE
           '-'
           WS-CURRENT-DAY DELIMITED BY SIZE
           ' '
           WS-CURRENT-HOUR DELIMITED BY SIZE
           ':'
           WS-CURRENT-MINUTE DELIMITED BY SIZE
           ':'
           WS-CURRENT-SECOND DELIMITED BY SIZE
      INTO WS-TIMESTAMP
    END-STRING
    
    IF NOT FILE-IS-OPENED
        OPEN EXTEND LOG-FILE
        IF WS-FILE-STATUS = "35"
            OPEN OUTPUT LOG-FILE
        END-IF
        MOVE 'Y' TO WS-FILE-OPENED
    END-IF
    
    STRING WS-TIMESTAMP DELIMITED BY SIZE
           ' ['
           LOG-LEVEL DELIMITED BY SIZE
           '] '
           LOG-MODULE DELIMITED BY SIZE
           ' - '
           LOG-MESSAGE DELIMITED BY SIZE
      INTO WS-LOG-LINE
    END-STRING
    
    WRITE LOG-RECORD FROM WS-LOG-LINE
    
    MOVE '0' TO LOG-RETURN-CODE
    
    GOBACK.
```

#### 处理步骤说明：

1. **生成格式化时间戳**：
   - 使用 FUNCTION CURRENT-DATE 获取系统当前日期和时间
   - 格式化为 "YYYY-MM-DD HH:MM:SS" 格式的时间戳
   - 存储到 WS-TIMESTAMP 变量中

2. **检查并打开日志文件**：
   - 检查日志文件是否已打开（通过 FILE-IS-OPENED 条件）
   - 如果未打开，尝试以追加模式打开（OPEN EXTEND）
   - 如果文件不存在（状态码"35"），则以创建模式打开（OPEN OUTPUT）
   - 设置文件已打开标志 WS-FILE-OPENED 为 'Y'

3. **格式化日志记录**：
   - 组合时间戳、日志级别、模块名和消息内容
   - 格式为："YYYY-MM-DD HH:MM:SS [级别] 模块名 - 消息"
   - 存储到 WS-LOG-LINE 变量中

4. **写入日志记录**：
   - 将格式化的日志行写入到日志文件

5. **设置返回结果**：
   - 将返回代码 LOG-RETURN-CODE 设置为 '0'（成功）
   - 返回到调用程序

## 日志级别说明

程序支持四种日志级别，通过 LOG-LEVEL 参数指定：

1. **I**：信息级别，记录正常的处理信息（如程序开始、结束）
2. **W**：警告级别，记录潜在问题但不影响程序继续执行的情况
3. **E**：错误级别，记录导致处理失败的错误情况
4. **D**：调试级别，记录详细的程序执行信息，用于问题排查

## 输入输出说明

### 输入
- **LOG-PARAMS**: 日志参数结构
  - LOG-LEVEL: 日志级别，PIC X
    - 'I'：信息
    - 'W'：警告
    - 'E'：错误
    - 'D'：调试
  - LOG-MODULE: 模块名，PIC X(8)
    - 记录日志的程序模块标识符
  - LOG-MESSAGE: 日志消息，PIC X(100)
    - 要记录的详细消息内容

### 输出
- **LOG-RETURN-CODE**: 返回代码，PIC X
  - '0'：成功
  - '9'：失败（当前实现总是返回'0'）
- **LOG-FILE**: 日志文件
  - 包含格式化的日志记录

## 日志格式

每条日志记录的格式为：

```
YYYY-MM-DD HH:MM:SS [级别] 模块名 - 消息内容
```

例如：
```
2025-07-15 08:30:45 [I] INSPMUPD - Premium payment processing started
2025-07-15 08:31:02 [E] INSPMUPD - Failed to validate policy ID P000000010
2025-07-15 08:32:15 [I] INSPMUPD - Premium payment processing batch ended. Processed: 5 Errors: 1
```

## 处理流程图

```
开始
  |
  v
生成格式化时间戳
  |
  v
检查日志文件是否已打开
  |
  +--> 否 ---> 尝试以追加模式打开文件
  |             |
  |             v
  |          检查文件是否存在
  |             |
  |             +--> 否 ---> 以创建模式打开文件
  |             |               |
  |             +--> 是 ----------+
  |                               |
  |                               v
  |                         设置文件已打开标志
  |                               |
  +--> 是 -----------------------+
                                 |
                                 v
                          格式化日志记录
                                 |
                                 v
                           写入日志记录
                                 |
                                 v
                          设置返回代码为'0'
                                 |
                                 v
                               结束
```

## 错误处理

1. **文件操作错误**：
   - 程序处理文件不存在的情况（状态码"35"），通过创建新文件解决
   - 其他文件操作错误未在当前实现中处理

2. **数据验证**：
   - 程序不对日志参数进行验证，依赖调用程序提供有效参数
   - LOG-LEVEL、LOG-MODULE 和 LOG-MESSAGE 值的合法性由调用程序负责

## 注意事项

1. 本子程序采用英文版实现（INSPMLOG_EN），适用于国际化系统环境。

2. 日志文件通过环境变量 LOGFILE 指定，在系统启动时设置。

3. 日志文件以追加模式打开，确保新的日志记录添加到现有日志的末尾，保留历史日志。

4. 当日志文件不存在时，程序会自动创建，确保日志记录不会因为文件不存在而失败。

5. 实际部署中，可能需要考虑日志轮转机制，防止日志文件过大。 