import os
import matplotlib.pyplot as plt
import numpy as np


def plot_ellipse(a, b, h):
    def ellipse(a, b, h, x, y):
        return x**2 / a**2 + (y+h-b)**2 / b**2 - 1

    xs_min = -a - 0.1
    xs_max = a + 0.1
    xs = np.linspace(xs_min, xs_max)

    # y + h - b = +-b
    ys_min = -h - 0.1
    ys_max = 2 * b - h + 0.1
    ys = np.linspace(ys_min, ys_max)

    x, y = np.meshgrid(xs, ys)
    z = ellipse(a, b, h, x, y)

    plt.contour(x, y, z, levels=[0], colors='black')


def plot_liquid(a, b, h):
    def ellipse_bottom(a, b, h, x):
        return -b * np.sqrt(1 - x**2 / a**2) + b - h

    def ellipse_bottom_xs(a, b, h):
        x = np.sqrt(a**2 * h * (2*b - h) / b**2)
        return -x, x

    xs = np.linspace(*ellipse_bottom_xs(a, b, h))

    label = "H = {}".format(h)
    plt.fill_between(xs, ellipse_bottom(a, b, h, xs), label=label)


def main():
    dir_name = "plots_ellipse"

    for a, b in [(1, 1), (1, 2), (2, 1)]:
        for h in [0.5, 0.75, 1.0]:
            plot_ellipse(a, b, h)
            plot_liquid(a, b, h)

            title = "A = {}, B = {}".format(a, b)

            plt.title(title)
            plt.gca().set_aspect('equal')
            plt.grid()
            plt.gca().legend()

            file_name = "{}_{}_{}.png".format(a, b, h)
            file_path = os.path.join(dir_name, file_name)

            plt.savefig(file_path)
            plt.clf()


if __name__ == "__main__":
    main()
