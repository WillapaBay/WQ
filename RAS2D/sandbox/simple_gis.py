import turtle
import pandas as pd

turtle.screensize(200, 200)


class Map:
    def __init__(self, map_width=600, map_height=300, xmin=-180, xmax=180, ymin=-90, ymax=90):
        self.xmin = xmin
        self.xmax = xmax
        self.ymin = ymin
        self.ymax = ymax
        self.map_width = map_width
        self.map_height = map_height
        self.xdist = self.xmax - self.xmin
        self.ydist = self.ymax - self.ymin
        self.x_ratio = self.map_width / self.xdist
        self.y_ratio = self.map_height / self.ydist

    def draw_border(self, speed=0):
        pen = turtle.Turtle()
        pen.pen(shown=False)
        pen.speed(speed)
        buffer = 10
        pen.up()
        pen.goto([-self.map_width/2 - buffer, -self.map_height/2 - buffer])
        pen.down()
        pen.goto([self.map_width/2 + buffer, -self.map_height/2 - buffer])
        pen.goto([self.map_width/2 + buffer, self.map_height/2 + buffer])
        pen.goto([-self.map_width/2 - buffer, self.map_height/2 + buffer])
        pen.goto([-self.map_width/2 - buffer, -self.map_height/2 - buffer])
        pen.up()
        pen.goto([0, 0])

    def show_grid(self, dx=5, dy=5, speed=0):
        dx = dx
        dy = dy
        pen = turtle.Turtle()
        pen.pen(shown=False)
        pen.pencolor('gray')
        pen.speed(speed)
        pen.up()
        pen.goto(self.convert([-180, -90]))
        for i in range(-180, 180 + dx, dx):
            if self.xmin <= i <= self.xmax:
                pen.up()
                pen.goto(self.convert([i, self.ymin]))
                pen.down()
                pen.goto(self.convert([i, self.ymax]))
        for i in range(-90, 90 + dy, dy):
            if self.ymin <= i <= self.ymax:
                pen.up()
                pen.goto(self.convert([self.xmin, i]))
                pen.down()
                pen.goto(self.convert([self.xmax, i]))

    def convert(self, point):
        # Python turtle graphics start in the middle of the screen,
        # so we must offset the points so they are centered
        lon = point[0]
        lat = point[1]
        # x = self.map_width - ((self.xmax - lon) * self.x_ratio) - self.map_width/2
        # y = self.map_height - ((self.ymax - lat) * self.y_ratio) - self.map_height/2
        x = self.map_width/2 - ((self.xmax - lon) * self.x_ratio)
        y = self.map_height/2 - ((self.ymax - lat) * self.y_ratio)
        return [x, y]


class State:
    def __init__(self, name, boundary, population, gismap):
        self.name = name
        self.boundary = boundary
        self.population = population
        self.map = gismap

    def draw(self, color='orange', label=False, speed=9):
        pen = turtle.Turtle()
        pen.speed(speed)
        pen.pencolor('black')
        pen.fillcolor(color)
        pen.up()
        pen.begin_fill()
        first_pixel = None
        for point in self.boundary:
            pixel = self.map.convert(point)
            if not first_pixel:
                first_pixel = pixel
            pen.goto(pixel)
            pen.down()
        pen.goto(first_pixel)
        pen.up()
        pen.end_fill()
        pen.goto([0, 0])
        if label:
            pen.write(self.name, align="center", font=("Arial", 16, "bold"))
        pen.pen(shown=False)


class City:
    def __init__(self, name, state, center, population, gismap):
        self.name = name
        self.state = state  # State object
        self.center = center
        self.population = population
        self.map = gismap

    def draw(self, color='blue', label=False, speed=9):
        pen = turtle.Turtle()
        pen.speed(speed)
        pen.pencolor(color)
        pixel = self.map.convert(self.center)
        pen.up()
        pen.goto(pixel)
        # Place a point for the city
        pen.dot(10)
        # Label the city
        if label:
            pen.write(self.name + ", Pop.: " +
                      str(self.population), align="left")
        pen.up()
        pen.pen(shown=False)


def tolist(coord_dict):
    coordinates = []
    for c in coord_dict:
        coordinates.append([c['lng'], c['lat']])
    return coordinates


def one_state():
    # xmin = -109
    # xmax = -102
    # ymin = 37
    # ymax = 41
    xmin = -112
    xmax = -100
    ymin = 36
    ymax = 42
    gismap = Map(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)
    gismap.draw_border()
    gismap.show_grid(dx=1, dy=1)
    state = State("COLORADO", [[-109, 37], [-109, 41],
                               [-102, 41], [-102, 37]], 5187582, gismap)
    state.draw(label='true', speed=2)
    cities = []
    cities.append(City("DENVER", state, [-104.98, 39.74], 634265, gismap))
    cities.append(City("BOULDER", state, [-105.27, 40.02], 98889, gismap))
    cities.append(City("DURANGO", state, [-107.88, 37.28], 17069, gismap))

    biggest_city = max(cities, key=lambda city: city.population)
    western_city = min(cities, key=lambda city: city.center[0])
    northern_city = max(cities, key=lambda city: city.center[1])
    print('Largest city: %s' % biggest_city.name)
    print('Most western city: %s' % western_city.name)
    print('Most northern city: %s' % northern_city.name)

    pen = turtle.Turtle()
    pen.pen(shown=False)
    pen.up()
    pen.goto(0, -200)
    pen.write('Largest city: %s' % biggest_city.name)
    pen.goto(0, -220)
    pen.write('Most western city: %s' % western_city.name)
    pen.goto(0, -240)
    pen.write('Most northern city: %s' % northern_city.name)

    for city in cities:
        city.draw(label=True, speed=0)


def us_and_canada():
    xmin = -175
    xmax = -45
    ymin = 15
    ymax = 75
    gismap = Map(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)
    gismap.draw_border()
    gismap.show_grid()

    d = pd.read_json('states.json')

    colors = ['Firebrick', 'Dark Orange', 'Orange', 'Gold', 'Goldenrod', 'Dark Green', 'Sea Green', 'Medium Sea Green',
              'Light Sea Green', 'Lime Green', 'Olive Drab', 'Medium Blue', 'Deep Sky Blue', 'Dodger Blue', 'Dark Violet',
              'Purple', 'Salmon', 'Turquoise', 'Dark Turquoise', 'Brown', 'Dark Slate Gray']

    for i, state_name in enumerate(d.keys()):
        state_coord_dict = d[state_name]['Coordinates']
        coordinates = tolist(state_coord_dict)
        state = State(state_name, coordinates, 5187582, gismap)
        state.draw(color=colors[i % len(colors)], speed=0)


if __name__ == '__main__':
    one_state()
    # us_and_canada()

    turtle.done()
