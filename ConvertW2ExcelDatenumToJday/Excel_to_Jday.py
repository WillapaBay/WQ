import glob

def excelToJday(inputFilename, outputFilename, head):
    infile = open(inputFilename, "r")
    lines = infile.readlines()
    infileText = "".join(lines)
    infile.close()

    if "jday" not in infileText.lower():
        print "Error: %s is not a CE-QUAL-W2 input file"
        return None

    # Identify and separate header from rest of lines in input file
    headnum = 0
    for i in range(len(lines)):
        if "jday" in lines[i].lower():
            headnum = i
            
    header = lines[:headnum+1]
    lines = lines[headnum+1:]

    # Determine filetype from input filename
    csv = "csv" in inputFilename.lower()
    npt = "npt" in inputFilename.lower()
    other = not (csv or npt)

    blanks1 = 0
    for i in range(len(lines[0])):
        if (lines[0])[i] != " ":
            if (lines[0])[i] == "\t":
                blanks1 = 2
            else:
                blanks1 = i
                break

    if csv:
        number = int(((lines[0][blanks1:]).split(","))[0])
    elif npt:
        number = int(((lines[0][blanks1:]).split(" "))[0])
    elif other:
        print "Error: %s is not a valid file" % inputFilename
        return None

    else:
        print "Unknown Error: %s" % inputFilename
        return None

    outfileDataList = []
    blanks = 0
    for line in lines:
        tabs = False
        for i, char in enumerate(line):
            if char != " ":
                if char == "\t":
                    blanks = 2
                    tabs = True
                else:
                    blanks = i
                    break
            else:
                blanks = 0
        if csv:
            if line.strip().replace(" ", "") != "":
                ln = line.split(",")
            
                lnum = float(ln[0][blanks:])
                lnum -= (number-1)
                ln[0] = str(lnum)
                if tabs:
                    outfileDataList.append("\t" + ",".join(ln))
                elif not tabs:
                    outfileDataList.append(" " * blanks + ",".join(ln))
        elif npt:
            if line.strip().replace(" ", "") != "": 
                ln = line[blanks:].split(" ", 1)
                ln0 = float(ln[0])
                ln0 -= (number-1)
                ln[0] = str(ln0)
                if tabs:
                    outfileDataList.append("\t" + " ".join(ln))
                elif not tabs:
                    outfileDataList.append(" " * blanks + " ".join(ln))
        elif other:
            break

    # for i in outfileText:
    #     header.append(i)

    outfileList = []
    outfileList.extend(header)
    outfileList.extend(outfileDataList)
    outfileText = "".join(outfileList)
    outfile = open(outputFilename, "w")
    outfile.write(outfileText)
    outfile.close()
    print "Processed %s" % inputFilename

def getinout(text):
    input_output = text.strip("\n").split(" ")
    input = input_output[0]
    if len(input_output) == 2:
        try:
            headerlines = int(float(input_output[1]))
            output = input
        except:
            headerlines = 3
            output = input_output[1]
    elif len(input_output) == 3:
        output = input_output[1]
        headerlines = input_output[2]
    elif len(input_output) == 1:
        output = input
        headerlines = 3
    else:
        print "Error: Unknown"
        return None
    return input, output, headerlines

if __name__ == "__main__":
    import glob
    inputFiles = open("files_to_convert.txt", "r")
    inputList = inputFiles.readlines()
    # Go through the list of files
    for file in inputList:
        # Check if the line contains a file
        if file.strip("\n").strip(" ") != '':
            # Check if any files with that name exist
            if len(glob.glob("*\\%s" % (file.split(" ")[0]))) > 0: 
                input, output, headerlines = getinout(file)
                # Process all files with the name of the file (in all directories within the project folder)
                # glob.glob returns a list of files with the asterisk as the wild card and the current input file name for the search. 
                for i in range(len(glob.glob("*\\%s" % (input)))):
                    inputFilename = glob.glob("*\\%s" % (input))[i]
                    outputFilename = glob.glob("*\\%s" % (input))[i].split("\\")[0] + "\\" + output
                    excelToJday(inputFilename, outputFilename, headerlines)
            else:
                print "%s does not exist or is not a valid file" % file.split(" ")[0]
    print "PROCESSED ALL FILES"


