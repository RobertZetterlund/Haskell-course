import os
import sys
# Lab requirments
MAX_LENGTH = 78 # A line can't contain more than 78 characters

# Helper functions
def isComment(line):
    return ("--"  in line)

# 'Main'
file = sys.argv[1]
if os.path.exists(file):
    # file exists, open it in read mode
    lab = open(file, "r")
    # Check if it meets requirments
    lineCount = 0
    inBlockComment = False
    for line in lab:
        lineCount+=1
        # Check if current line contains block comment character
        if("TODO" in line):
            print("There's more to do on line: " + str(lineCount))
        if("{-" in line):
            inBlockComment = True
        if("-}" in line and inBlockComment):
            inBlockComment = False

        if((len(line) >= MAX_LENGTH) and (not isComment(line)) and (not inBlockComment)):
            # A line is too long, tell user which line is too long
            print("Too many characters on line: " + str(lineCount) + "    : no chars: " + str(len(line)))
            print(line)

    # Run Hlint on file
    os.system("hlint " + file)
