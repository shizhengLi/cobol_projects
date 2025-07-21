-- 保险会计数据库表结构
-- 这些表模拟DB2表结构

-- 保单表（主表）
CREATE TABLE INSURANCE_POLICY (
    POLICY_ID CHAR(10) NOT NULL PRIMARY KEY,  -- 保单ID
    CUSTOMER_ID CHAR(8) NOT NULL,             -- 客户ID
    POLICY_TYPE CHAR(2) NOT NULL,             -- 保单类型代码：01=人寿，02=健康，03=意外
    POLICY_STATUS CHAR(1) NOT NULL,           -- 状态：A=有效，C=取消，E=过期
    ISSUE_DATE CHAR(8) NOT NULL,              -- 签发日期 YYYYMMDD
    EXPIRE_DATE CHAR(8) NOT NULL,             -- 到期日期 YYYYMMDD
    PREMIUM_AMOUNT DECIMAL(12, 2) NOT NULL,   -- 保费金额
    COVERAGE_AMOUNT DECIMAL(15, 2) NOT NULL,  -- 保障金额
    CREATE_USER CHAR(8),                      -- 创建用户
    CREATE_TIMESTAMP CHAR(14),                -- 创建时间戳 YYYYMMDDHHMMSS
    UPDATE_USER CHAR(8),                      -- 更新用户
    UPDATE_TIMESTAMP CHAR(14)                 -- 更新时间戳 YYYYMMDDHHMMSS
);

-- 客户表
CREATE TABLE CUSTOMER (
    CUSTOMER_ID CHAR(8) NOT NULL PRIMARY KEY, -- 客户ID
    LAST_NAME VARCHAR(30) NOT NULL,           -- 姓
    FIRST_NAME VARCHAR(30) NOT NULL,          -- 名
    BIRTH_DATE CHAR(8),                       -- 出生日期 YYYYMMDD
    GENDER CHAR(1),                           -- 性别：M=男，F=女
    EMAIL VARCHAR(50),                        -- 电子邮件
    PHONE VARCHAR(15),                        -- 电话号码
    ADDRESS VARCHAR(100),                     -- 地址
    CREATE_USER CHAR(8),                      -- 创建用户
    CREATE_TIMESTAMP CHAR(14),                -- 创建时间戳
    UPDATE_USER CHAR(8),                      -- 更新用户
    UPDATE_TIMESTAMP CHAR(14)                 -- 更新时间戳
);

-- 保费支付记录表
CREATE TABLE PREMIUM_PAYMENT (
    PAYMENT_ID CHAR(12) NOT NULL PRIMARY KEY, -- 支付ID
    POLICY_ID CHAR(10) NOT NULL,              -- 保单ID
    PAYMENT_DATE CHAR(8) NOT NULL,            -- 支付日期 YYYYMMDD
    AMOUNT DECIMAL(12, 2) NOT NULL,           -- 支付金额
    PAYMENT_METHOD CHAR(2) NOT NULL,          -- 支付方式：01=银行转账，02=信用卡，03=现金
    PAYMENT_STATUS CHAR(1) NOT NULL,          -- 支付状态：P=已处理，F=失败，W=待处理
    ACCOUNTING_PERIOD CHAR(6) NOT NULL,       -- 会计期间 YYYYMM
    REFERENCE_NO VARCHAR(20),                 -- 参考编号
    CREATE_USER CHAR(8),                      -- 创建用户
    CREATE_TIMESTAMP CHAR(14),                -- 创建时间戳
    UPDATE_USER CHAR(8),                      -- 更新用户
    UPDATE_TIMESTAMP CHAR(14)                 -- 更新时间戳
); 