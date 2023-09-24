#!/usr/bin/bash

echo "Trying to compile your submission..."

# Create temporary directory
temp_dir=$(mktemp -d)

# Compiling Parser.hs

echo ""
echo "(1) Trying to compile Parser.hs..."
ghc -XSafe Parser.hs -odir $temp_dir -hidir $temp_dir

if [ $? -ne 0 ]
then
    echo ""
    echo "Parser.hs did not compile (with '-XSafe')."
    echo "Please fix it before submitting."
    exit 1
fi

if ! [ -f "$temp_dir/Parser.o" ]
then
    echo ""
    echo "The module name in Parser.hs does match not the filename Parser."
    echo "Please make sure that:"
    echo -e "\t(i) you did not change the file name;"
    echo -e "\t(ii) you did not change the top of the template;"
    echo "and try again."
    exit 1
fi

# Compiling Interpreter.hs

echo ""
echo "(2) Trying to compile Interpreter.hs..."
ghc -XSafe Interpreter.hs -odir $temp_dir -hidir $temp_dir

if [ $? -ne 0 ]
then
    echo ""
    echo "Interpreter.hs did not compile (with '-XSafe')."
    echo "Please fix it before submitting."
    exit 1
fi

if ! [ -f "$temp_dir/Interpreter.o" ]
then
    echo ""
    echo "The module name in Interpreter.hs does match not the filename Interpreter."
    echo "Please make sure that:"
    echo -e "\t(i) you did not change the file name;"
    echo -e "\t(ii) you did not change the top of the template;"
    echo "and try again."
    exit 1
fi

# Compiling Optimization.hs

echo ""
echo "(3) Trying to compile Optimization.hs..."
ghc -XSafe Optimization.hs -odir $temp_dir -hidir $temp_dir

if [ $? -ne 0 ]
then
    echo ""
    echo "Optimization.hs did not compile (with '-XSafe')."
    echo "Please fix it before submitting."
    exit 1
fi

if ! [ -f "$temp_dir/Optimization.o" ]
then
    echo ""
    echo "The module name in Optimization.hs does match not the filename Optimization."
    echo "Please make sure that:"
    echo -e "\t(i) you did not change the file name;"
    echo -e "\t(ii) you did not change the top of the template;"
    echo "and try again."
    exit 1
fi

# Done

echo ""
echo "All checks passed."
echo "You are ready to submit!"

# Cleanup temporary directory
rm -r $temp_dir
