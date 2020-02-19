from PyQt5 import QtWidgets as qtw
from PyQt5 import QtCore as qtc
from PyQt5 import QtGui as qtg
import pandas as pd
import sys

from plot_computational_mesh import *

window_width = 800
window_height = 500

# Color source
# https://www.rapidtables.com/web/color/green-color.html

# Red
lightsalmon = qtg.QColor(255, 160, 122)
salmon = qtg.QColor(250, 128, 114)
darksalmon = qtg.QColor(233, 150, 122)
lightcoral = qtg.QColor(240, 128, 128)
indianred = qtg.QColor(205, 92, 92)
crimson = qtg.QColor(220, 20, 60)
firebrick = qtg.QColor(178, 34, 34)
red = qtg.QColor(255, 0, 0)
darkred = qtg.QColor(139, 0, 0)
maroon = qtg.QColor(128, 0, 0)
palevioletred = qtg.QColor(219, 112, 147)

# Orange
coral = qtg.QColor(255, 127, 80)
tomato = qtg.QColor(255, 99, 71)
orangered = qtg.QColor(255, 69, 0)
gold = qtg.QColor(255, 215, 0)
orange = qtg.QColor(255, 165, 0)
darkorange = qtg.QColor(255, 140, 0)

# Blue
deepskyblue = qtg.QColor(0, 191, 255)
dodgerblue = qtg.QColor(30, 144, 255)
mediumslateblue = qtg.QColor(123, 104, 238)
blue = qtg.QColor(0, 0, 255)
mediumblue = qtg.QColor(0, 0, 205)
darkblue = qtg.QColor(0, 0, 139)
navy = qtg.QColor(0, 0, 128)
midnightblue = qtg.QColor(25, 25, 112)
blueviolet = qtg.QColor(138, 43, 226)
indigo = qtg.QColor(75, 0, 130)

# Brown
sandybrown = qtg.QColor(244, 164, 96)
goldenrod = qtg.QColor(218, 165, 32)
peru = qtg.QColor(205, 133, 63)
chocolate = qtg.QColor(210, 105, 30)
saddlebrown = qtg.QColor(139, 69, 19)
sienna = qtg.QColor(160, 82, 45)
brown = qtg.QColor(165, 42, 42)
maroon = qtg.QColor(128, 0, 0)

# Green
teal = qtg.QColor(0, 128, 128)
green = qtg.QColor(0, 128, 0)
darkgreen = qtg.QColor(0, 100, 0)
mediumseagreen = qtg.QColor(60, 179, 113)
lightseagreen = qtg.QColor(32, 178, 170)
seagreen = qtg.QColor(46, 139, 87)
olive = qtg.QColor(128, 128, 0)
darkolivegreen = qtg.QColor(85, 107, 47)
olivedrab = qtg.QColor(107, 142, 35)
limegreen = qtg.QColor(50, 205, 50)

# Purple
mediumpurple = qtg.QColor(147, 112, 219)
blueviolet = qtg.QColor(138, 43, 226)
darkviolet = qtg.QColor(148, 0, 211)
darkorchid = qtg.QColor(153, 50, 204)
darkmagenta = qtg.QColor(139, 0, 139)
purple = qtg.QColor(128, 0, 128)
indigo = qtg.QColor(75, 0, 130)

# Turquoise
paleturquoise = qtg.QColor(175, 238, 238)
turquoise = qtg.QColor(64, 224, 208)
mediumturquoise = qtg.QColor(72, 209, 204)
darkturquoise = qtg.QColor(0, 206, 209)

# Gray
gainsboro = qtg.QColor(220, 220, 220)
lightgray = qtg.QColor(211, 211, 211)
silver = qtg.QColor(192, 192, 192)
darkgray = qtg.QColor(169, 169, 169)
gray = qtg.QColor(128, 128, 128)
dimgray = qtg.QColor(105, 105, 105)
lightslategray = qtg.QColor(119, 136, 153)
slategray = qtg.QColor(112, 128, 144)
darkslategray = qtg.QColor(47, 79, 79)
darkgray = qtg.QColor(50, 50, 50)
charcoalgray = qtg.QColor(25, 25, 25)
black = qtg.QColor(0, 0, 0)
white = qtg.QColor(255, 255, 255)

colors = [firebrick, darkorange, orange, gold, goldenrod, darkgreen, seagreen, mediumseagreen,
          lightseagreen, limegreen, olivedrab, mediumblue, deepskyblue, dodgerblue, darkviolet,
          purple, salmon, turquoise, darkturquoise, brown, darkslategray]


class MapWidget(qtw.QWidget):

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.objects = []

    def add(self, obj):
        self.objects.append(obj)

    def paintEvent(self, paint_event):
        painter = qtg.QPainter(self)
        for obj in self.objects:
            obj.draw(painter)


class Point:

    def __init__(self, lon, lat, gis_map):
        self.lon = lon
        self.lat = lat
        self.map = gis_map
        xdist = self.map.xmax - self.map.xmin
        ydist = self.map.ymax - self.map.ymin
        scale_width = self.map.map_width - 2*self.map.buffer
        scale_height = self.map.map_height - 2*self.map.buffer
        self.x = scale_width/xdist * (lon - self.map.xmin) + self.map.buffer
        self.y = self.map.map_height - \
            (scale_height/ydist * (lat - self.map.ymin) + self.map.buffer)


class Map:

    def __init__(self, map_width=window_width, map_height=window_height, xmin=-180, xmax=180, ymin=-90, ymax=90, dx=5, dy=5, buffer=10):
        self.map_width = map_width
        self.map_height = map_height
        self.xmin = xmin
        self.xmax = xmax
        self.ymin = ymin
        self.ymax = ymax
        self.dx = dx
        self.dy = dy
        self.buffer = buffer

    def draw(self, painter):
        painter.setOpacity(1.0)
        brush = qtg.QBrush(white)
        painter.setBrush(brush)
        painter.drawRect(0, 0, self.map_width, self.map_height)
        self.draw_grid(painter)
        self.draw_border(painter)
        painter.setOpacity(1.0)

    def draw_border(self, painter):
        # Draw the background
        pen = qtg.QPen(black, 2, qtc.Qt.SolidLine)
        painter.setPen(pen)
        painter.setOpacity(0.5)
        width = self.map_width - 2*self.buffer
        height = self.map_height - 2*self.buffer
        painter.drawRect(0 + self.buffer, 0 + self.buffer, width, height)

    def draw_grid(self, painter):
        path = qtg.QPainterPath()
        painter.setRenderHint(qtg.QPainter.Antialiasing)
        painter.setPen(lightgray)

        for i in range(-180, 180 + self.dx, self.dx):
            if self.xmin <= i <= self.xmax:
                point1 = Point(i, self.ymin, self)
                point2 = Point(i, self.ymax, self)
                path.moveTo(point1.x, point1.y)
                path.lineTo(point2.x, point2.y)
        for i in range(-90, 90 + self.dy, self.dy):
            if self.ymin <= i <= self.ymax:
                point1 = Point(self.xmin, i, self)
                point2 = Point(self.xmax, i, self)
                path.moveTo(point1.x, point1.y)
                path.lineTo(point2.x, point2.y)

        painter.drawPath(path)


class State:

    def __init__(self, name, boundary, population, gis_map, color=orange, label=False):
        self.name = name
        self.boundary = boundary
        self.population = population
        self.map = gis_map
        self.color = color
        self.label = label

    def draw(self, painter):
        path = qtg.QPainterPath()
        painter.setPen(darkslategray)
        painter.setRenderHint(qtg.QPainter.Antialiasing)
        brush = qtg.QBrush(self.color)
        painter.setBrush(brush)
        first_coord = self.boundary[0]
        first_point = Point(first_coord[0], first_coord[1], self.map)
        path.moveTo(first_point.x, first_point.y)

        for coord in self.boundary:
            point = Point(coord[0], coord[1], self.map)
            path.lineTo(point.x, point.y)
        path.lineTo(first_point.x, first_point.y)
        painter.drawPath(path)
        if self.label:
            lonmin = min(self.boundary, key=lambda coord: coord[0])[0]
            lonmax = max(self.boundary, key=lambda coord: coord[0])[0]
            latmin = min(self.boundary, key=lambda coord: coord[1])[1]
            latmax = max(self.boundary, key=lambda coord: coord[1])[1]
            # Offset by 0.6 degrees to approx. center of state
            lon = (lonmin + lonmax)/2 - 0.6
            lat = (latmin + latmax)/2
            point1 = Point(lon, lat, self.map)
            painter.setFont(qtg.QFont('Helvetica', 16, qtg.QFont.ExtraBold))
            painter.drawText(point1.x, point1.y, self.name)


class City:

    def __init__(self, name, state, center, population, gis_map):
        self.name = name
        self.state = state
        self.center = center
        self.population = population
        self.map = gis_map

    def draw(self, painter, color=blue, label=True):
        painter.setPen(color)
        point = Point(self.center[0], self.center[1], self.map)
        # Place a point for the city
        painter.setPen(qtg.QPen(seagreen, 1, qtc.Qt.SolidLine))
        painter.setBrush(qtg.QBrush(darkslategray, qtc.Qt.SolidPattern))
        painter.drawEllipse(point.x, point.y, 10, 10)
        # Label the city
        painter.setPen(qtg.QPen(black, 1, qtc.Qt.SolidLine))
        if label:
            painter.setFont(qtg.QFont('Helvetica', 12))
            painter.drawText(point.x + 8, point.y - 3, self.name +
                             ", Pop.: " + str(self.population))


def tolist(coord_dict):
    coordinates = []
    for c in coord_dict:
        coordinates.append([c['lng'], c['lat']])
    return coordinates


class US_and_Canada(qtw.QMainWindow):

    def __init__(self):
        super().__init__()

        map_widget = MapWidget(self)
        self.setCentralWidget(map_widget)

        xmin = -175
        xmax = -45
        ymin = 15
        ymax = 75
        gis_map = Map(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)
        map_widget.add(gis_map)

        d = pd.read_json('states.json')

        for i, state_name in enumerate(d.keys()):
            state_coord_dict = d[state_name]['Coordinates']
            coordinates = tolist(state_coord_dict)
            color = colors[i % len(colors)]
            state = State(state_name, coordinates,
                          5187582, gis_map, color=color)
            map_widget.add(state)

        map_widget.update()

        self.resize(window_width, window_height)
        self.show()


class Text:

    def __init__(self, x, y, text):
        self.x = x
        self.y = y
        self.text = text

    def draw(self, painter):
        painter.setFont(qtg.QFont('Helvetica', 12))
        painter.drawText(self.x, self.y, self.text)


class One_State(qtw.QMainWindow):

    def __init__(self):
        super().__init__()

        map_widget = MapWidget(self)
        self.setCentralWidget(map_widget)

        xmin = -112
        xmax = -100
        ymin = 36
        ymax = 42
        gis_map = Map(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, dx=1, dy=1)
        map_widget.add(gis_map)

        state = State("COLORADO", [[-109, 37], [-109, 41],
                                   [-102, 41], [-102, 37]], 5187582, gis_map, label=True)
        map_widget.add(state)

        cities = []

        cities.append(City("DENVER", state, [-104.98, 39.74], 634265, gis_map))
        cities.append(City("BOULDER", state, [-105.27, 40.02], 98889, gis_map))
        cities.append(City("DURANGO", state, [-107.88, 37.28], 17069, gis_map))

        biggest_city = max(cities, key=lambda city: city.population)
        western_city = min(cities, key=lambda city: city.center[0])
        northern_city = max(cities, key=lambda city: city.center[1])
        print('Largest city: %s' % biggest_city.name)
        print('Most western city: %s' % western_city.name)
        print('Most northern city: %s' % northern_city.name)

        map_widget.add(Text(window_width/2 - 50, window_height-60,
                            'Largest city: %s' % biggest_city.name))
        map_widget.add(Text(window_width/2 - 50, window_height-40,
                            'Most western city: %s' % western_city.name))
        map_widget.add(Text(window_width/2 - 50, window_height-20,
                            'Most northern city: %s' % northern_city.name))

        for city in cities:
            map_widget.add(city)

        map_widget.update()

        self.resize(window_width, window_height)
        self.show()


if __name__ == '__main__':
    app = qtw.QApplication(sys.argv)
    mw = US_and_Canada()
    mw2 = One_State()
    sys.exit(app.exec())
