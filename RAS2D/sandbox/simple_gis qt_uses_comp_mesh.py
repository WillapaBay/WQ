from PyQt5 import QtWidgets as qtw
from PyQt5 import QtCore as qtc
from PyQt5 import QtGui as qtg
import pandas as pd
import sys

from plot_computational_mesh import *


class State(Cell):

    def __init__(self, name, boundary, population, gis_map, color=orange, line_color=darkslategray, label=False):
        super().__init__(name, boundary, gis_map, line_color=line_color,
                         fill_color=color, text_color=black, label=label)
        self.population = population

    def draw(self, painter):
        path = qtg.QPainterPath()
        painter.setPen(self.line_color)
        painter.setRenderHint(qtg.QPainter.Antialiasing)
        brush = qtg.QBrush(self.fill_color)
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


class City(Node):

    def __init__(self, name, state, center, population, gis_map):
        super().__init__(name, center, gis_map)
        self.population = population

    def draw(self, painter, label=True):
        painter.setPen(self.line_color)
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

        # window_width = 800
        # window_height = 500
        window_width = 1200
        window_height = 750

        d = pd.read_json('states.json')

        map_widget = MapWidget(self)
        self.setCentralWidget(map_widget)

        xmin = -175
        xmax = -45
        ymin = 15
        ymax = 75
        gis_map = Map(window_width, window_height, xmin=xmin,
                      xmax=xmax, ymin=ymin, ymax=ymax, dx=5, dy=5)
        map_widget.add(gis_map)

        map_grid = MapGrid(gis_map)
        map_widget.add(map_grid)

        for i, state_name in enumerate(d.keys()):
            state_coord_dict = d[state_name]['Coordinates']
            coordinates = tolist(state_coord_dict)
            color = colors[i % len(colors)]
            state = State(state_name, coordinates,
                          5187582, gis_map, color=color)
            map_widget.add(state)

        map_scale = MapScale(0, 20, 5, -70.0, 17.5, 'degrees', 0.7, gis_map)
        map_widget.add(map_scale)

        north_arrow = NorthArrow(-50.0, 65.0, 1, 5, 3,
                                 3, gis_map, fill_color=white, line_color=black)
        map_widget.add(north_arrow)

        map_border = MapBorder(gis_map)
        map_widget.add(map_border)

        map_widget.update()

        self.resize(window_width, window_height)
        self.show()
        savefig(map_widget, 'us_and_canada_map.png')


class One_State(qtw.QMainWindow):

    def __init__(self):
        super().__init__()

        window_width = 1200
        window_height = 750

        map_widget = MapWidget(self)
        self.setCentralWidget(map_widget)

        xmin = -112
        xmax = -100
        ymin = 36
        ymax = 42

        gis_map = Map(window_width, window_height, xmin=xmin,
                      xmax=xmax, ymin=ymin, ymax=ymax, dx=1, dy=1)
        map_widget.add(gis_map)

        map_grid = MapGrid(gis_map)
        map_widget.add(map_grid)

        state = State("COLORADO", [[-109, 37], [-109, 41],
                                   [-102, 41], [-102, 37]], 5187582, gis_map, label=True)
        map_widget.add(state)

        map_scale = MapScale(0, 2, 1, -102.5, 36.5, 'degrees', 0.1, gis_map)
        map_widget.add(map_scale)

        north_arrow = NorthArrow(-100.5, 40, 0.15, 1, 0.4, 0.4, gis_map, fill_color=white, line_color=black)
        map_widget.add(north_arrow)

        map_border = MapBorder(gis_map)
        map_widget.add(map_border)

        cities = []
        cities.append(City("DENVER", state, [-104.98, 39.74], 634265, gis_map))
        cities.append(City("BOULDER", state, [-105.27, 40.02], 98889, gis_map))
        cities.append(City("DURANGO", state, [-107.88, 37.28], 17069, gis_map))

        for city in cities:
            map_widget.add(city)

        biggest_city = max(cities, key=lambda city: city.population)
        western_city = min(cities, key=lambda city: city.center[0])
        northern_city = max(cities, key=lambda city: city.center[1])
        print('Largest city: %s' % biggest_city.name)
        print('Most western city: %s' % western_city.name)
        print('Most northern city: %s' % northern_city.name)

        map_widget.add(Text(window_width/2 - 50, window_height-120,
                            'Largest city: %s' % biggest_city.name))
        map_widget.add(Text(window_width/2 - 50, window_height-100,
                            'Most western city: %s' % western_city.name))
        map_widget.add(Text(window_width/2 - 50, window_height-80,
                            'Most northern city: %s' % northern_city.name))

        map_widget.update()
        self.resize(window_width, window_height)
        self.show()
        savefig(map_widget, 'colorado_map.png')


if __name__ == '__main__':
    app = qtw.QApplication(sys.argv)
    mw = US_and_Canada()
    mw2 = One_State()
    sys.exit(app.exec())
