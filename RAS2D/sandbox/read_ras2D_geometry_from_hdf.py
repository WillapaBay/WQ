import h5py

def printname(name):
    print(name)

f = h5py.File('Muncie.p04.hdf', 'r')
f.visit(printname)

# f['Geometry']['2D Flow Areas']['2D Interior Area'].visit(printname)
# x = f['Geometry']['2D Flow Areas']['Cell Info']
# f['Geometry']['2D Flow Areas']['Cell Points'].visit(printname)
# f['Geometry']['2D Flow Areas']['Polygon Info'].visit(printname)
# f['Geometry']['2D Flow Areas']['Polygon Points'].visit(printname)

# indexes = f['Geometry']['2D Flow Areas']['2D Interior Area']['Cells FacePoint Indexes'].value
# for i in indexes:
#     print(i)

s = [
    'Geometry/2D Flow Areas/2D Interior Area/Cells Center Coordinate',
    'Geometry/2D Flow Areas/2D Interior Area/Cells Face and Orientation Info',
    'Geometry/2D Flow Areas/2D Interior Area/Cells Face and Orientation Values',
    'Geometry/2D Flow Areas/2D Interior Area/Cells FacePoint Indexes',
    'Geometry/2D Flow Areas/2D Interior Area/Cells Minimum Elevation',
    'Geometry/2D Flow Areas/2D Interior Area/Cells Surface Area',
    'Geometry/2D Flow Areas/2D Interior Area/Cells Volume Elevation Info',
    'Geometry/2D Flow Areas/2D Interior Area/Cells Volume Elevation Values',
    'Geometry/2D Flow Areas/2D Interior Area/FacePoints Cell Index Values',
    'Geometry/2D Flow Areas/2D Interior Area/FacePoints Cell Info',
    'Geometry/2D Flow Areas/2D Interior Area/FacePoints Coordinate',
    'Geometry/2D Flow Areas/2D Interior Area/FacePoints Face and Orientation Info',
    'Geometry/2D Flow Areas/2D Interior Area/FacePoints Face and Orientation Values',
    'Geometry/2D Flow Areas/2D Interior Area/FacePoints Is Perimeter',
    'Geometry/2D Flow Areas/2D Interior Area/Faces Area Elevation Info',
    'Geometry/2D Flow Areas/2D Interior Area/Faces Area Elevation Values',
    'Geometry/2D Flow Areas/2D Interior Area/Faces Cell Indexes',
    'Geometry/2D Flow Areas/2D Interior Area/Faces FacePoint Indexes',
    'Geometry/2D Flow Areas/2D Interior Area/Faces Low Elevation Centroid',
    'Geometry/2D Flow Areas/2D Interior Area/Faces Minimum Elevation',
    'Geometry/2D Flow Areas/2D Interior Area/Faces NormalUnitVector and Length',
    'Geometry/2D Flow Areas/2D Interior Area/Faces Perimeter Info',
    'Geometry/2D Flow Areas/2D Interior Area/Faces Perimeter Values']

for i in s:
    print(f[i])

indexes = f['Geometry/2D Flow Areas/2D Interior Area/Cells FacePoint Indexes'].value # max value: 5773, shape(5765, 7)
print(indexes)
len(indexes)

m = []
for i in range(len(indexes)):
    x = indexes[i]
    m.append(x.max())

print('max value', max(m))

values = f['Geometry/2D Flow Areas/2D Interior Area/FacePoints Coordinate'].value # shape(5774, 2)
print(values)
len(values)

m = []
for i in range(len(values)):
    x = values[i]
    m.append(x.max())

print('max value', max(m))