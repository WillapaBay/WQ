from PyQt5 import QtWidgets as qtw
from PyQt5 import QtCore as qtc
from PyQt5 import QtGui as qtg
import pandas as pd
import sys
import numpy as np

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


def hex_to_rgb(value):
    value = value.lstrip('#')
    lv = len(value)
    t = tuple(int(value[i:i + lv // 3], 16) for i in range(0, lv, lv // 3))
    return rgb(t[0], t[1], t[2])


def rgb(r, g, b):
    return qtg.QColor(r, g, b)


def get_color_index(value, vmin, vmax, ncolors):
    cmin = 0
    cmax = ncolors - 1
    return int((value - vmin)/(vmax - vmin) * (cmax - cmin))


def get_color(value, vmin, vmax, colors, step_type='linear'):
    R, G, B = [], [], []
    for c in colors:
        rgb = c.getRgb()
        R.append(rgb[0])
        G.append(rgb[1])
        B.append(rgb[2])
    ncolors = len(colors)
    cmin = 0
    cmax = ncolors - 1
    if step_type == 'step':
        ci = int((value - vmin)/(vmax - vmin) * (cmax - cmin))
    elif step_type == 'linear':
        ci = (value - vmin)/(vmax - vmin) * (cmax - cmin)
    else:
        ci = (value - vmin)/(vmax - vmin) * (cmax - cmin)
    r = np.interp(ci, np.arange(len(colors)), R)
    g = np.interp(ci, np.arange(len(colors)), G)
    b = np.interp(ci, np.arange(len(colors)), B)
    return qtg.QColor(r, g, b)


def savefig(widget, filename):
    widget.grab().save(filename)


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

    def __init__(self, mapx, mapy, gis_map):
        self.map = gis_map
        xdist = gis_map.xmax - gis_map.xmin
        ydist = gis_map.ymax - gis_map.ymin
        scale_width = gis_map.map_width - 2*gis_map.buffer
        scale_height = gis_map.map_height - 2*gis_map.buffer
        self.x = scale_width/xdist * (mapx - gis_map.xmin) + gis_map.buffer
        self.y = gis_map.map_height - \
            (scale_height/ydist * (mapy - gis_map.ymin) + gis_map.buffer)


class Dimensions:

    def __init__(self, width_in_map, height_in_map, gis_map):
        scale_width = gis_map.map_width - 2*gis_map.buffer
        scale_height = gis_map.map_height - 2*gis_map.buffer
        xdist = gis_map.xmax - gis_map.xmin
        ydist = gis_map.ymax - gis_map.ymin
        self.width = scale_width/xdist * width_in_map
        self.height = scale_height/ydist * height_in_map


class Map:

    def __init__(self, map_width, map_height, xmin=0, xmax=1, ymin=0, ymax=1, dx=0.1, dy=0.1, buffer=40):
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


class MapBorder:
    def __init__(self, gis_map):
        self.map = gis_map

    def draw(self, painter):
        # Draw the background
        painter.setOpacity(1.0)
        pen = qtg.QPen(black, 2, qtc.Qt.SolidLine)
        painter.setPen(pen)
        # brush = qtg.QBrush(qtg.QColor(0, 0, 0, 1.0))
        painter.setBrush(qtc.Qt.NoBrush)
        width = self.map.map_width - 2*self.map.buffer
        height = self.map.map_height - 2*self.map.buffer
        painter.drawRect(0 + self.map.buffer, 0 +
                         self.map.buffer, width, height)


class MapGrid:
    def __init__(self, gis_map):
        self.map = gis_map

    def draw(self, painter):
        path = qtg.QPainterPath()
        painter.setRenderHint(qtg.QPainter.Antialiasing)
        painter.setPen(lightgray)

        for i in np.arange(self.map.xmin, self.map.xmax + self.map.dx, self.map.dx):
            if self.map.xmin <= i <= self.map.xmax:
                point1 = Point(i, self.map.ymin, self.map)
                point2 = Point(i, self.map.ymax, self.map)
                path.moveTo(point1.x, point1.y)
                path.lineTo(point2.x, point2.y)
        for i in np.arange(self.map.ymin, self.map.ymax + self.map.dy, self.map.dy):
            if self.map.ymin <= i <= self.map.ymax:
                point1 = Point(self.map.xmin, i, self.map)
                point2 = Point(self.map.xmax, i, self.map)
                path.moveTo(point1.x, point1.y)
                path.lineTo(point2.x, point2.y)

        painter.drawPath(path)


class Cell:

    def __init__(self, name, boundary, gis_map, line_color=white, fill_color=blue, text_color=black, label=False):
        self.name = name
        self.boundary = boundary
        self.map = gis_map
        self.fill_color = fill_color
        self.line_color = line_color
        self.text_color = text_color
        self.label = label

    def draw(self, painter):
        painter.setOpacity(1.0)
        path = qtg.QPainterPath()
        pen = qtg.QPen(self.line_color, 0)
        painter.setPen(pen)
        painter.setRenderHint(qtg.QPainter.Antialiasing)
        if self.fill_color:
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
            mapx_min = min(self.boundary, key=lambda coord: coord[0])[0]
            mapx_max = max(self.boundary, key=lambda coord: coord[0])[0]
            mapy_min = min(self.boundary, key=lambda coord: coord[1])[1]
            mapy_max = max(self.boundary, key=lambda coord: coord[1])[1]
            # Offset by 0.6 degrees to approx. center of state
            mapx = (mapx_min + mapx_max)/2 - 0.6
            mapy = (mapy_min + mapy_max)/2
            point1 = Point(mapx, mapy, self.map)
            painter.setPen(self.text_color)
            painter.setFont(qtg.QFont('Helvetica', 16, qtg.QFont.ExtraBold))
            painter.drawText(point1.x, point1.y, self.name)


class Node:

    def __init__(self, name, center, gis_map, fill_color=black, line_color=black, text_color=black, marker_size=2, label=False):
        self.name = name
        self.center = center
        self.map = gis_map
        self.label = label
        self.fill_color = fill_color
        self.line_color = line_color
        self.text_color = text_color
        self.marker_size = marker_size

    def draw(self, painter):
        painter.setOpacity(1.0)
        point = Point(self.center[0], self.center[1], self.map)
        painter.setPen(qtg.QPen(self.line_color, 0, qtc.Qt.SolidLine))
        painter.setBrush(qtg.QBrush(self.fill_color, qtc.Qt.SolidPattern))
        painter.drawEllipse(
            point.x, point.y, self.marker_size, self.marker_size)
        # Label the node
        if self.label:
            painter.setPen(qtg.QPen(self.text_color, 1, qtc.Qt.SolidLine))
            painter.setFont(qtg.QFont('Helvetica', 12))
            painter.drawText(point.x + 3, point.y - 3, self.name)


class Text:

    def __init__(self, x, y, text, gis_map, color=black, font='Helvetica', font_size=24):
        self.x = x
        self.y = y
        self.text = text
        self.map = gis_map
        self.text_color = color
        self.font = font
        self.font_size = font_size

    def draw(self, painter):
        point = Point(self.x, self.y, self.map)
        painter.setPen(qtg.QPen(self.text_color, 1, qtc.Qt.SolidLine))
        painter.setFont(qtg.QFont(self.font, self.font_size))
        painter.drawText(point.x, point.y, self.text)


class MapScale:

    def __init__(self, minval, maxval, step, mapx, mapy, units, height, gis_map, colors=[black, white]):
        self.minval = minval
        self.maxval = maxval
        self.step = step
        self.mapx = mapx
        self.mapy = mapy
        self.units = units
        self.height = height
        self.map = gis_map
        self.colors = colors

    def draw(self, painter):
        ci = 0
        val = self.minval
        write_units = True
        for i in np.arange(self.mapx, self.mapx + (self.maxval - self.minval) + self.step, self.step):
            color = self.colors[ci % len(self.colors)]
            brush = qtg.QBrush(color)
            pen = qtg.QPen(black)
            painter.setBrush(brush)
            painter.setPen(pen)
            point = Point(i, self.mapy, self.map)
            dimensions = Dimensions(self.step, self.height, self.map)

            # Draw scale (don't draw last scale box)
            if i <= self.mapx + self.maxval - self.step:
                painter.drawRect(point.x, point.y - dimensions.height,
                                 dimensions.width, dimensions.height)

            # Draw tick marks
            brush = qtg.QBrush(black)
            pen = qtg.QPen(black)
            painter.setPen(pen)
            painter.setBrush(brush)
            if i <= self.mapx + self.maxval - self.step:
                # Note, this draws downward on the figure
                painter.drawRect(point.x, point.y, 0, 5)
            else:
                # Note, this draws downward on the figure
                painter.drawRect(point.x - 0.5, point.y, 0, 5)

            # Label units
            if write_units:
                font_size = 16
                painter.setFont(
                    qtg.QFont('Helvetica', font_size, qtg.QFont.Bold))
                painter.drawText(point.x - font_size *
                                 len(self.units)/2 - 0, point.y, self.units)
                write_units = False

            # Label all values, including right edge of last box
            painter.drawText(point.x, point.y + 20, str(val))

            # Increment counters
            val += self.step
            ci += 1

class ColorScale:

    def __init__(self, minval, maxval, step, mapx, mapy, scale_factor, units, height, gis_map, colors):
        self.minval = minval
        self.maxval = maxval
        self.step = step
        self.mapx = mapx
        self.mapy = mapy
        self.scale_factor = scale_factor
        self.units = units
        self.height = height
        self.map = gis_map
        self.colors = colors

    def draw(self, painter):
        ci = 0
        val = self.minval
        write_units = True
        map_min = self.mapx
        map_step = self.step * self.scale_factor
        map_max = self.mapx + (self.maxval - self.minval) * self.scale_factor + map_step
        for i in np.arange(map_min, map_max, map_step):
            color = self.colors[ci % len(self.colors)]
            brush = qtg.QBrush(color)
            pen = qtg.QPen(black)
            painter.setBrush(brush)
            painter.setPen(pen)
            point = Point(i, self.mapy, self.map)
            dimensions = Dimensions(map_step, self.height, self.map)

            # Draw scale (don't draw last scale box)
            if i <= map_min + map_max - map_step:
                painter.drawRect(point.x, point.y - dimensions.height,
                                 dimensions.width, dimensions.height)

            # Draw tick marks
            brush = qtg.QBrush(black)
            pen = qtg.QPen(black)
            painter.setPen(pen)
            painter.setBrush(brush)
            if i <= map_min + map_max - map_step:
                # Note, this draws downward on the figure
                painter.drawRect(point.x, point.y, 0, 5)
            else:
                # Note, this draws downward on the figure
                painter.drawRect(point.x - 0.5, point.y, 0, 5)

            # Label units
            if write_units:
                font_size = 16
                painter.setFont(
                    qtg.QFont('Helvetica', font_size, qtg.QFont.Bold))
                painter.drawText(point.x - font_size *
                                 len(self.units)/2 - 0, point.y, self.units)
                write_units = False

            # Label all values, including right edge of last box
            painter.drawText(point.x, point.y + 20, str(val))

            # Increment counters
            val += self.step
            ci += 1


class NorthArrow:

    def __init__(self, mapx, mapy, base_width, base_height, arrowhead_width, arrowhead_height, gis_map, fill_color=white, line_color=black):
        self.mapx = mapx
        self.mapy = mapy
        self.base_width = base_width
        self.base_height = base_height
        self.arrowhead_width = arrowhead_width
        self.arrowhead_height = arrowhead_height
        self.map = gis_map
        self.fill_color = fill_color
        self.line_color = line_color

    def draw(self, painter):
        # Draw base
        brush = qtg.QBrush(self.fill_color)
        pen = qtg.QPen(self.line_color)
        painter.setBrush(brush)
        painter.setPen(pen)
        bottom_left_base_point = Point(self.mapx, self.mapy, self.map)
        # bottom_center_base_point = Point(self.mapx + self.base_width/2, self.mapy, self.map)
        top_left_base_point = Point(
            self.mapx, self.mapy + self.base_height, self.map)
        # top_center_base_point = Point(self.mapx + self.base_width/2, self.mapy + self.base_height, self.map)
        base_dimensions = Dimensions(
            self.base_width, self.base_height, self.map)
        painter.drawRect(top_left_base_point.x, top_left_base_point.y,
                         base_dimensions.width, base_dimensions.height)

        # Draw arrowhead
        arrowhead_dimensions = Dimensions(
            self.arrowhead_width, self.arrowhead_height, self.map)
        path = qtg.QPainterPath()
        left_point = Point(self.mapx + self.base_width/2 -
                           self.arrowhead_width/2, self.mapy + self.base_height, self.map)
        tip_point = Point(self.mapx + self.base_width/2, self.mapy +
                          self.base_height + self.arrowhead_height, self.map)
        right_point = Point(self.mapx + self.base_width/2 +
                            self.arrowhead_width/2, self.mapy + self.base_height, self.map)
        path.moveTo(left_point.x, left_point.y)
        path.lineTo(tip_point.x, tip_point.y)
        path.lineTo(right_point.x, right_point.y)
        path.lineTo(left_point.x, left_point.y)
        painter.drawPath(path)

        # Label arrow
        painter.setFont(qtg.QFont('Helvetica', 16, qtg.QFont.ExtraBold))
        label_base_point = Point(
            self.mapx, self.mapy + self.base_height + self.arrowhead_height/10, self.map)
        painter.drawText(label_base_point.x - 1, label_base_point.y, 'N')


def tolist(coord_dict):
    coordinates = []
    for c in coord_dict:
        coordinates.append([c['lng'], c['lat']])
    return coordinates


def read_elements(infile, skiprows=0):
    f = open(infile, 'r')
    lines = f.readlines()
    lines = lines[skiprows:]
    elements = []
    for line in lines:
        data = line.strip().split()
        data = list(map(int, data))
        elements.append(data)
    return elements


def read_nodes(infile, skiprows=0):
    f = open(infile, 'r')
    lines = f.readlines()
    lines = lines[skiprows:]
    nodes = []
    for line in lines:
        data = line.strip().split()
        data = list(map(float, data))
        nodes.append(data)
    return nodes


def read_ras_elements(infile, skiprows=0):
    f = open(infile, 'r')
    lines = f.readlines()
    lines = lines[skiprows:]
    elements = []
    for line in lines:
        data = line.strip().split()
        data = list(map(int, data[2:]))
        elements.append(data)
    return elements


def read_ras_nodes(infile, skiprows=0):
    f = open(infile, 'r')
    lines = f.readlines()
    lines = lines[skiprows:]
    nodes = []
    for line in lines:
        data = line.strip().split()
        data = list(map(float, data[1:3]))
        nodes.append(data)
    return nodes


def read_values(infile):
    f = open(infile, 'r')
    lines = f.readlines()
    values = []
    for line in lines:
        values.append(float(line.strip()))
    return values


class Dragon(qtw.QMainWindow):

    def __init__(self):
        super().__init__()

        window_width = 500
        window_height = 500

        map_widget = MapWidget(self)
        self.setCentralWidget(map_widget)

        xmin = 0
        xmax = 1
        ymin = 0
        ymax = 1
        gis_map = Map(window_width, window_height, xmin=xmin,
                      xmax=xmax, ymin=ymin, ymax=ymax)
        map_widget.add(gis_map)

        map_grid = MapGrid(gis_map)
        map_widget.add(map_grid)

        # Sample mesh obtained from:
        # http://people.math.sc.edu/Burkardt/data/hex_mesh/hex_mesh.html

        elements = read_elements('dragon_elements.txt', skiprows=3)
        nodes = read_nodes('dragon_nodes.txt', skiprows=3)

        for i, node_points in enumerate(elements):
            boundary = []
            for n in node_points:
                boundary.append(nodes[n-1])
            color = colors[i % len(colors)]
            cell = Cell(str(i+1), boundary, gis_map,
                        line_color=black, fill_color=color, label=False)
            map_widget.add(cell)

        for i, coords in enumerate(nodes):
            node = Node(str(i+1), coords, gis_map, fill_color=black,
                        line_color=black, text_color=black, label=True)
            map_widget.add(node)

        map_border = MapBorder(gis_map)
        map_widget.add(map_border)

        map_widget.update()

        self.resize(window_width, window_height)
        self.show()
        savefig(map_widget, 'dragon.png')


class Lake(qtw.QMainWindow):

    def __init__(self):
        super().__init__()

        window_width = 800
        window_height = 800

        map_widget = MapWidget(self)
        self.setCentralWidget(map_widget)

        # scale_colors = [crimson, tomato, orange, gold, green]
        # scale_colors = [rgb(247, 252, 240), rgb(224, 243, 219), rgb(204, 235, 197), rgb(168, 221, 181), rgb(
        #     123, 204, 196), rgb(78, 179, 211), rgb(43, 140, 190), rgb(8, 104, 172), rgb(8, 64, 129)]
        # scale_colors = [rgb(255,247,251),rgb(236,226,240),rgb(208,209,230),rgb(166,189,219),rgb(103,169,207),rgb(54,144,192),rgb(2,129,138),rgb(1,108,89),rgb(1,70,54)]
        # s = ["#f44321","#5091cd","#f9a541","#7ac143"]
        # scale_colors = [hex_to_rgb(x) for x in s]
        # scale_colors = [rgb(0, 163, 226), rgb(27, 165, 72), rgb(253, 200, 0), rgb(241, 134, 14), rgb(228, 27, 19)]
        # s = ["#e6261f","#eb7532","#f7d038","#a3e048","#49da9a","#34bbe6","#4355db","#d23be7"]
        # s = ["#e6261f","#eb7532","#f7d038","#a3e048","#49da9a","#34bbe6"]
        # s = ["#f7fcf0","#e0f3db","#ccebc5","#a8ddb5","#7bccc4","#4eb3d3","#2b8cbe","#0868ac","#084081"]
        s = ["#f5542e", "#f2c327", "#008b6e", "#00aede", "#0067ad"]
        scale_colors = [hex_to_rgb(x) for x in s[::-1]]

        xmin = 0
        xmax = 800
        ymin = 100
        ymax = 900
        gis_map = Map(window_width, window_height, xmin=xmin, xmax=xmax, ymin=ymin,
                      ymax=ymax, dx=100, dy=100)
        map_widget.add(gis_map)

        map_grid = MapGrid(gis_map)
        map_widget.add(map_grid)

        # Sample mesh obtained from:
        # http://people.math.sc.edu/Burkardt/data/fem2d/fem2d.html

        elements = read_elements('lake_elements.txt')
        nodes = read_nodes('lake_nodes.txt')
        values = read_values('lake_values.txt')
        print('min: ', min(values))
        print('max: ', max(values))

        for i, node_points in enumerate(elements):
            boundary = []

            p = node_points[0]
            v = values[p-1]
            # idx = int(v * 20)
            idx = get_color_index(v, -2, 3, len(scale_colors))

            for n in node_points:
                boundary.append(nodes[n-1])
            color = scale_colors[idx % len(scale_colors)]
            cell = Cell(str(i+1), boundary, gis_map, line_color=lightgray,
                        fill_color=color, text_color=black, label=False)
            map_widget.add(cell)

        for i, coords in enumerate(nodes):
            node = Node(str(i+1), coords, gis_map, fill_color=orange,
                        line_color=orange, label=False)
            map_widget.add(node)

        map_scale = MapScale(0, 200, 50, 550, 130,
                             'meters', 10, gis_map)
        map_widget.add(map_scale)

        north_arrow = NorthArrow(
            750, 800, 10, 50, 30, 30, gis_map, fill_color=white, line_color=black)
        map_widget.add(north_arrow)

        map_border = MapBorder(gis_map)
        map_widget.add(map_border)

        map_widget.update()

        self.resize(window_width, window_height)
        self.show()
        savefig(map_widget, 'lake.png')


class Channel(qtw.QMainWindow):

    def __init__(self):
        super().__init__()

        window_width = 800
        window_height = 800

        map_widget = MapWidget(self)
        self.setCentralWidget(map_widget)

        xmin = -5
        xmax = 15
        ymin = -5
        ymax = 5
        gis_map = Map(window_width, window_height, xmin=xmin, xmax=xmax, ymin=ymin,
                      ymax=ymax, dx=1, dy=1)
        map_widget.add(gis_map)

        map_grid = MapGrid(gis_map)
        map_widget.add(map_grid)

        # Sample mesh obtained from:
        # http://people.math.sc.edu/Burkardt/data/fem2d/fem2d.html

        elements = read_elements('channel_elements.txt')
        nodes = read_nodes('channel_nodes.txt')

        for i, node_points in enumerate(elements):
            boundary = []
            for n in node_points:
                boundary.append(nodes[n-1])
            color = colors[i % len(colors)]
            cell = Cell(str(i+1), boundary, gis_map, line_color=lightgray,
                        fill_color=color, text_color=black, label=False)
            map_widget.add(cell)

        for i, coords in enumerate(nodes):
            node = Node(str(i+1), coords, gis_map, fill_color=orange,
                        line_color=orange, label=False)
            map_widget.add(node)

        map_border = MapBorder(gis_map)
        map_widget.add(map_border)

        map_widget.update()

        self.resize(window_width, window_height)
        self.show()
        savefig(map_widget, 'channel.png')


class Greenland(qtw.QMainWindow):

    def __init__(self):
        super().__init__()

        window_width = 1000
        window_height = 1000

        map_widget = MapWidget(self)
        self.setCentralWidget(map_widget)

        xmin = -50
        xmax = 350
        ymin = 0
        ymax = 550
        gis_map = Map(window_width, window_height, xmin=xmin, xmax=xmax, ymin=ymin,
                      ymax=ymax, dx=100, dy=100)
        map_widget.add(gis_map)

        map_grid = MapGrid(gis_map)
        map_widget.add(map_grid)

        # Sample mesh obtained from:
        # http://people.math.sc.edu/Burkardt/data/fem2d/fem2d.html

        elements = read_elements('greenland_elements.txt')
        nodes = read_nodes('greenland_nodes.txt')

        for i, node_points in enumerate(elements):
            boundary = []
            for n in node_points:
                boundary.append(nodes[n-1])
            # color = colors[i % len(colors)]
            color = lightseagreen
            cell = Cell(str(i+1), boundary, gis_map, line_color=blue,
                        fill_color=color, text_color=black, label=False)
            map_widget.add(cell)

        # for i, coords in enumerate(nodes):
        #     node = Node(str(i+1), coords, gis_map, fill_color=orange, line_color=orange, label=False)
        #     map_widget.add(node)

        map_border = MapBorder(gis_map)
        map_widget.add(map_border)

        map_widget.update()

        self.resize(window_width, window_height)
        self.show()
        savefig(map_widget, 'greenland.png')


class BigCavity(qtw.QMainWindow):

    def __init__(self):
        super().__init__()

        window_width = 800
        window_height = 800

        map_widget = MapWidget(self)
        self.setCentralWidget(map_widget)

        xmin = 0
        xmax = 1
        ymin = 0
        ymax = 1
        gis_map = Map(window_width, window_height, xmin=xmin, xmax=xmax, ymin=ymin,
                      ymax=ymax, dx=0.1, dy=0.1)
        map_widget.add(gis_map)

        map_grid = MapGrid(gis_map)
        map_widget.add(map_grid)

        # Sample mesh obtained from:
        # http://people.math.sc.edu/Burkardt/data/fem2d/fem2d.html

        elements = read_elements('big_cavity_elements.txt')
        nodes = read_nodes('big_cavity_nodes.txt')

        for i, node_points in enumerate(elements):
            boundary = []
            for n in node_points:
                boundary.append(nodes[n-1])
            color = colors[i % len(colors)]
            color = None
            cell = Cell(str(i+1), boundary, gis_map, line_color=lightgray,
                        fill_color=color, text_color=black, label=False)
            map_widget.add(cell)

        for i, coords in enumerate(nodes):
            node = Node(str(i+1), coords, gis_map, fill_color=orange,
                        line_color=orange, label=False)
            map_widget.add(node)

        map_border = MapBorder(gis_map)
        map_widget.add(map_border)

        map_widget.update()

        self.resize(window_width, window_height)
        self.show()
        savefig(map_widget, 'big_cavity.png')


class Web(qtw.QMainWindow):

    def __init__(self):
        super().__init__()

        window_width = 800
        window_height = 800

        map_widget = MapWidget(self)
        self.setCentralWidget(map_widget)

        xmin = -5
        xmax = 5
        ymin = -1
        ymax = 6
        gis_map = Map(window_width, window_height, xmin=xmin, xmax=xmax, ymin=ymin,
                      ymax=ymax, dx=1, dy=1)
        map_widget.add(gis_map)

        map_grid = MapGrid(gis_map)
        map_widget.add(map_grid)

        # Sample mesh obtained from:
        # http://people.math.sc.edu/Burkardt/data/fem2d/fem2d.html

        elements = read_elements('web_elements.txt', skiprows=2)
        nodes = read_nodes('web_nodes.txt', skiprows=4)

        for i, node_points in enumerate(elements):
            boundary = []
            for n in node_points:
                boundary.append(nodes[n-1])
            color = colors[i % len(colors)]
            cell = Cell(str(i+1), boundary, gis_map, line_color=lightgray,
                        fill_color=color, text_color=black, label=False)
            map_widget.add(cell)

        for i, coords in enumerate(nodes):
            node = Node(str(i+1), coords, gis_map, fill_color=orange,
                        line_color=orange, label=True)
            map_widget.add(node)

        map_border = MapBorder(gis_map)
        map_widget.add(map_border)

        map_widget.update()

        self.resize(window_width, window_height)
        self.show()
        savefig(map_widget, 'web.png')


class RAS2D_ADCIRC(qtw.QMainWindow):

    def __init__(self):
        super().__init__()

        window_width = 1500
        window_height = 1000

        map_widget = MapWidget(self)
        self.setCentralWidget(map_widget)

        xmin = 404500
        xmax = 412500
        ymin = 1800500
        ymax = 1805500
        gis_map = Map(window_width, window_height, xmin=xmin, xmax=xmax, ymin=ymin,
                      ymax=ymax, dx=500, dy=500)
        map_widget.add(gis_map)

        map_grid = MapGrid(gis_map)
        map_widget.add(map_grid)

        # NOTE: USING SPECIAL FUNCTIONS HERE FOR RAS
        elements = read_ras_elements('2D_Interior_Area_TIN_elements.txt')
        nodes = read_ras_nodes('2D_Interior_Area_TIN_nodes.txt')

        for i, node_points in enumerate(elements):
            boundary = []
            for n in node_points:
                boundary.append(nodes[n-1])
            # color = colors[i % len(colors)]
            color = lightseagreen
            cell = Cell(str(i+1), boundary, gis_map, line_color=black,
                        fill_color=color, text_color=black, label=False)
            map_widget.add(cell)

        # for i, coords in enumerate(nodes):
        #     node = Node(str(i+1), coords, gis_map, fill_color=white,
        #                 line_color=white, marker_size=1, label=False)
        #     map_widget.add(node)

        map_scale = MapScale(0, 1000, 250, 411250, 1800700,
                             'meters', 50, gis_map)
        map_widget.add(map_scale)

        north_arrow = NorthArrow(
            412250, 1804750, 50, 400, 150, 150, gis_map, fill_color=white, line_color=black)
        map_widget.add(north_arrow)

        map_border = MapBorder(gis_map)
        map_widget.add(map_border)

        map_widget.update()

        self.resize(window_width, window_height)
        self.show()
        savefig(map_widget, '2D_Interior_Area_TIN.png')


class RAS2D_HDF(qtw.QMainWindow):

    def __init__(self, timestep):
        super().__init__()

        window_width = 1500
        window_height = 1000

        map_widget = MapWidget(self)
        self.setCentralWidget(map_widget)

        xmin = 404500
        xmax = 412500
        ymin = 1800500
        ymax = 1805500
        gis_map = Map(window_width, window_height, xmin=xmin, xmax=xmax, ymin=ymin,
                      ymax=ymax, dx=500, dy=500)
        map_widget.add(gis_map)

        map_grid = MapGrid(gis_map)
        map_widget.add(map_grid)

        # NOTE: USING SPECIAL FUNCTIONS HERE FOR RAS
        import h5py
        f = h5py.File('Muncie.p04.hdf', 'r')
        # max value: 5773, shape(5765, 7)
        elements_array = f['Geometry/2D Flow Areas/2D Interior Area/Cells FacePoint Indexes'][()]
        # shape(5774, 2)
        nodes_array = f['Geometry/2D Flow Areas/2D Interior Area/FacePoints Coordinate'][()]

        depth = f['Results/Unsteady/Output/Output Blocks/Base Output/Unsteady Time Series/2D Flow Areas/2D Interior Area/Depth'][()
                                                                                                                                 ][timestep]
        node_x_vel = f['Results/Unsteady/Output/Output Blocks/Base Output/Unsteady Time Series/2D Flow Areas/2D Interior Area/Node X Vel'][()
                                                                                                                                           ][timestep]
        node_y_vel = f['Results/Unsteady/Output/Output Blocks/Base Output/Unsteady Time Series/2D Flow Areas/2D Interior Area/Node Y Vel'][()
                                                                                                                                           ][timestep]
        node_vel = np.sqrt(node_x_vel**2 + node_y_vel**2)

        s = ["#f5542e", "#f2c327", "#008b6e", "#00aede", "#0067ad"]
        scale_colors = [hex_to_rgb(x) for x in s[::-1]]

        elements = []
        nodes = []
        for i in range(len(elements_array)):
            elements.append(list(elements_array[i]))
        for i in range(len(nodes_array)):
            nodes.append(list(nodes_array[i]))

        for i, node_points in enumerate(elements):
            boundary = []
            for n in node_points:
                if n > -1:
                    boundary.append(nodes[n])
            # color = colors[i % len(colors)]
            # color = lightseagreen

            # Color cells by depth at specified time step, range 0 - 20
            value = depth[i]
            # ci = get_color_index(value, 0, 20, len(scale_colors))
            # color = scale_colors[ci]
            color = get_color(value, 0, 20, scale_colors)

            # Color cells by velocity magnitude at specified time step, range 0.0 - 3.5
            # value = node_vel[i]
            # color = get_color(value, 0, 2.5, scale_colors)

            cell = Cell(str(i+1), boundary, gis_map, line_color=navy,
                        fill_color=color, text_color=black, label=False)
            map_widget.add(cell)

        # for i, coords in enumerate(nodes):
        #     node = Node(str(i+1), coords, gis_map, fill_color=white,
        #                 line_color=white, marker_size=1, label=False)
        #     map_widget.add(node)

        color_scale = ColorScale(0, 20, 5, 405000, 1800700, 100, 'depth (ft)', 75, gis_map, scale_colors)
        map_widget.add(color_scale)

        # map_scale_colors = [crimson, tomato, orange, gold]
        map_scale_colors = [black, white]
        map_scale = MapScale(0, 1000, 250, 411250, 1800700,
                             'dist (ft)', 50, gis_map, colors=map_scale_colors)
        map_widget.add(map_scale)

        north_arrow = NorthArrow(
            412250, 1804750, 50, 400, 150, 150, gis_map, fill_color=white, line_color=navy)
        map_widget.add(north_arrow)

        # timestep_label = Text(404520, 1805490, 'Time step: %03d' % (timestep + 1))
        timestep_label = Text(
            404600, 1805300, 'Time step: %03d' % (timestep + 1), gis_map)
        map_widget.add(timestep_label)

        map_border = MapBorder(gis_map)
        map_widget.add(map_border)

        map_widget.update()

        self.resize(window_width, window_height)
        self.show()
        savefig(map_widget, '2D_Interior_Area_HDF_depth_%03d.png' % timestep)


if __name__ == '__main__':
    app = qtw.QApplication(sys.argv)
    # mesh1 = Dragon()
    # mesh2 = Lake()
    # mesh3 = Channel()
    # mesh4 = Greenland()
    # mesh5 = BigCavity()
    # mesh6 = Web()
    # mesh7 = RAS2D_ADCIRC()

    # for i in range(30, 32):
    for i in range(30, 175):
        mesh8 = RAS2D_HDF(i)
        mesh8.close()

    sys.exit(app.exec())
