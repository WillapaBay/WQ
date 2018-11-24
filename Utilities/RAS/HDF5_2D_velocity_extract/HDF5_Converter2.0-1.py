#-----------------------
# Elliot Steissberg
# ERDC-EL
# Version 2.2
# November 15, 2018
#-----------------------
"""
The velocity path is in fort64.h5 or equivalent, the elevation path is in fort63.h5 or equivalent, 
and all others are found in the main {project name}.p04.hdf project hdf file or equivalent
"""
paths1 = {"Velocity":"Datasets/Depth-averaged Velocity (64)/Values",
          "Coordinates":"Geometry/2D Flow Areas/2D Interior Area/Cells Center Coordinate",
          "Times":"Results/Unsteady/Output/Output Blocks/Base Output/Unsteady Time Series/Time",
          "Depth":"Results/Unsteady/Output/Output Blocks/Base Output/Unsteady Time Series/2D Flow Areas/2D Interior Area/Depth",
          "TIN Info":"Geometry/Cross Section Interpolation Surfaces/TIN Info",
          "TIN Points":"Geometry/Cross Section Interpolation Surfaces/TIN Points",
          "TIN Triangles":"Geometry/Cross Section Interpolation Surfaces/TIN Triangles",
          "Elevation":"Datasets/Water Surface Elevation (63)/Values"
          }

def get_values(mainfile, sidefile1, sidefile2):
    """Returns a dictionary of values {x,y,u,v,time,basedepth,TIN}"""
    valuedict = {}
    velocityvalues = list(sidefile1[paths1["Velocity"]])
    u1 = velocityvalues[:][:][0]
    v1 = velocityvalues[:][:][1]
    times1 = list(mainfile[paths1["Times"]])
    coords1 = list(mainfile[paths1["Coordinates"]])
    basedepth = list(mainfile[paths1["Depth"]])
    TINfo = list(mainfile[paths1["TIN Info"]])
    TINts = list(mainfile[paths1["TIN Points"]])
    TINgles = list(mainfile[paths1["TIN Triangles"]])
    elevation = list(sidefile2[paths1["Elevation"]])
    x1 = coords1[:][0]
    y1 = coords1[:][1]
    valuedict["x"] = x1
    valuedict["y"] = y1
    valuedict["u"] = u1
    valuedict["v"] = v1
    valuedict["time"] = times1
    valuedict["basedepth"] = basedepth
    valuedict["elevation"] = elevation
    valuedict["TIN"] = {"TIN Info":TINfo,"TIN Points":TINts,"TIN Triangles":TINgles}
    
    return valuedict

def generate_center_depth(elevation, tin, tingrdf):
    """Calculate the depth values for eacg cell center using elevation, tin, and/or tin grid"""
    grdcoords = []
    le = len(elevation)
    i=1
    tingrdftext=tingrdftext.readlines()
    for ln in tingrdf:
        if not re.search(r"\t[1-%i]"%le, ln): # pass line if it isn't an indexed value
           pass
        else:
            tempcoord = re.split(r'\t+',ln.rstrip().lstrip())
            
            # If the index in the previous line was less than 727, append. Else, end the read.
            if i in range(1,le):
                i=tempcoord[0].split(" ")[0]
                x,y=tempcoord[0].split(" ")[1:]
                grdcoords.append(x, y, tempcoords[1])
            elif i==le:
                break
            else:
                break
    
    # Create the calculated depth array
    arrgrd = np.array(grdcoords)
    arrelev = np.array(elevation)
    new_depth = []
    for elev in arrelev:
        new_depth.append(elev-arrgrd[2][:])
    return new_depth

def writetocsv(dictofvals):
    """Creates the output txt files for each timestep from the dictionary created by get_values"""
    for timestep in dictofvals["time"]:
        timeindex = dictofvals["time"].index(timestep)
        timefile = open("%s_%s.csv"%(mainf.split(".")[0],parsedate(timestep)),"r+")
        lines = ["%s,%f,%f,%f,%f,%f"%(parsedate(timestep),x,y,u,v,depth) for x,y,u,v,depth in dictofvals["x"], dictofvals["y"], dictofvals["u"][timeindex], dictofvals["v"][timeindex], dictofvals["depth"][timeindex]
        timefile.write("\n".join(lines))
        timefile.close()
    
def parsedate(timestring):
    """(Hopefully) Works on the times from main output, not from optional files (fix the dang naming discrepancy in the datasets please)"""
    import numpy as np
    timeslot=str(timestring)
    months={"JAN":1,"FEB":2,"MAR":3,"APR":4,"MAY":5,"JUN":6,
                "JUL":7,"AUG":8,"SEP":9,"OCT":10,"NOV":11,"DEC":12}
    datestr=timeslot.split(" ")[0]
    timestr=timeslot.split(" ")[1]
    for i in months.keys():
        if i in datestr:
            month1=months[i]
            day1=datestr.split(i)[0]
            year1=datestr.split(i)[1]
            
    timestrlist=timestr.split(":")[0:2]
    time1=":".join(timestrlist)
    date1=day1+"/"+month1+"/"+year1
    finaltime=date1+" "+time1
    return finaltime
if __name__ == "__main__":
    import h5py
    import numpy as np
    import sys, os
    import re
    
    # This section gets filenames with either command-line args (semicolon-separated)
    # or with input from running it directly.
    try:
        mainf, sidef1, sidef2, tingrd = (" ".join(sys.argv[1:])).split(";")
    except:
        mainf = raw_input("Main project hdf file (p04): ")
        sidef1 = raw_input("Side/optional hdf velocity file (fort64.h5?): ")
        sidef2 = raw_input("Side/optional hdf elevation file (fort63.h5?): ")
        tingrd = raw_input("TIN grid file: ")
    
    # Create file objects for the filenames.
    mainfile = h5py.File(mainf,"r")
    sidefile1 = h5py.File(sidef1,"r")
    sidefile2 = h5py.File(sidef2,"r")
    tingrdf = open(tingrd,"r")
    
    # First, create the dictionary of relevant values from the read files,
    # and then calculate the depth values from the tin or grid file.
    valuedict = get_values(mainfile, sidefile1, sidefile2)
    valuedict["depth"] = generate_center_depth(valuedict["elevation"], valuedict["TIN"], tingrdf)
    writetocsv(valuedict)