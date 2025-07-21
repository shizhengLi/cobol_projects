#!/bin/bash

# 创建SQLite数据库并初始化表结构
DB_PATH="./insurance_accounting.db"

# 如果数据库文件已存在，则先删除
if [ -f "$DB_PATH" ]; then
    rm "$DB_PATH"
fi

# 创建表结构
sqlite3 "$DB_PATH" < create_tables.sql

# 插入测试数据
sqlite3 "$DB_PATH" < insert_data.sql

echo "数据库初始化完成: $DB_PATH" 