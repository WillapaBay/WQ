import glob

def findFiles(infileNames, outfileText, outputExtension = False, numHeaderLines = 3):
    
    for infilename in infileNames:
        infile = open(infilename, 'r')
        
        lines = infile.readlines()
        infile.close()

        for line in lines:
            lcline = line.lower()
            if '.csv' in lcline or '.npt' in lcline and 'not used' not in lcline:
                line = line.split()
                if not outputExtension and numHeaderLines == 3:
                    outfileText.append(line[-1] + '\n')
                elif outputExtension and numHeaderLines == 3:
                    outfileText.append(line[-1] + " " + "jday_" + line[-1] + '\n')
                elif outputExtension and numHeaderLines != 3:
                    outfileText.append(line[-1] + " " + "jday_" + line[-1] + \
                              str(numHeaderLines) +'\n')
                elif not outputExtension and numHeaderLines != 3:
                    outfileText.append(line[-1] + " " + str(numHeaderLines) + '\n')

if __name__ == '__main__':
    
    # Specify control file containing the names of files in the project
    listfile = glob.glob("*\\w2_con.npt")
    outfilename = 'files_to_convert.txt'
    outfile = open(outfilename, 'w')
    outfiletext = []
    
    # If you wish to leave the original intact, set outputextension to True
    # and the number of header lines.
    # default: overwrite original and set number of header lines to 3
    findFiles(listfile, outfiletext, outputExtension = True, numHeaderLines = 3)
    for line in outfiletext:
        outfile.write(line)
    outfile.close()
