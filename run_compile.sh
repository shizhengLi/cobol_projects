#!/bin/bash

# 设置工作目录
WORK_DIR=$(pwd)
echo "Working Directory: $WORK_DIR"

# 创建输出目录
mkdir -p bin output

# 检查输入文件
echo "Checking input file..."
INPUT_FILE="$WORK_DIR/data/input/payment_update.dat"
if [ -f "$INPUT_FILE" ]; then
    echo "Input file exists, content:"
    cat "$INPUT_FILE"
else
    echo "Error: Input file does not exist: $INPUT_FILE"
    exit 1
fi

# 初始化数据库
echo "Initializing SQLite database..."
cd $WORK_DIR/db
./init_db.sh

# 返回工作目录
cd $WORK_DIR

# 编译子程序为模块
echo "Compiling subroutines as modules..."
cobc -c -o bin/INSPOLVD.o src/INSPOLVD.cbl
cobc -c -o bin/INSPMINS.o src/INSPMINS.cbl
cobc -c -o bin/INSPMLOG_EN.o src/INSPMLOG_EN.cbl

# 检查编译是否成功
if [ ! -f "bin/INSPMLOG_EN.o" ]; then
    echo "Failed to compile INSPMLOG_EN.cbl. Checking if file exists:"
    ls -la src/INSPMLOG_EN.cbl
    exit 1
fi

# 编译主程序，链接子程序模块
echo "Compiling main program and linking modules..."
cobc -x -o bin/INSPMUPD_EN src/INSPMUPD_EN.cbl bin/INSPOLVD.o bin/INSPMINS.o bin/INSPMLOG_EN.o

# 如果编译失败，退出
if [ $? -ne 0 ]; then
    echo "Compilation failed! Please check error messages."
    exit 1
fi

# 设置环境变量 - 使用绝对路径
echo "Setting environment variables..."
export PAYFILE="$WORK_DIR/data/input/payment_update.dat"
export REPFILE="$WORK_DIR/output/payment_report_en.txt"
export LOGFILE="$WORK_DIR/output/program_en.log"

# 确保输出目录和文件存在
echo "Ensuring output files exist..."
touch "$REPFILE"
touch "$LOGFILE"
chmod 666 "$REPFILE" "$LOGFILE"

echo "PAYFILE=$PAYFILE"
echo "REPFILE=$REPFILE"
echo "LOGFILE=$LOGFILE"

# 运行程序
echo "Running main program..."
cd $WORK_DIR/bin
./INSPMUPD_EN

# 检查执行结果
if [ $? -ne 0 ]; then
    echo "Program execution failed! Please check error messages."
    exit 1
fi

# 显示结果
echo "Program execution completed. Output files are in the output directory."
if [ -f "$WORK_DIR/output/payment_report_en.txt" ]; then
    echo "Report file content:"
    cat "$WORK_DIR/output/payment_report_en.txt"
else
    echo "Report file does not exist"
fi

if [ -f "$WORK_DIR/output/program_en.log" ]; then
    echo "Log file content:"
    cat "$WORK_DIR/output/program_en.log"
else
    echo "Log file does not exist"
fi 