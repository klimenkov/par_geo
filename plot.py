import os
from functools import partial

import matplotlib.pyplot as plt
import numpy as np


config = {
    "rcParams_custom": {
        "font.family": "Times New Roman",
        "mathtext.default": "regular",
        "xaxis.labellocation": "right",
        "axes.prop_cycle": plt.cycler('color', ["black"]),
        "lines.linewidth": 1.75}}

plt.rcParams.update(config["rcParams_custom"])


def get_params_gen(dir_1_path, dir_2_name):
    or_, a, tk = map(float, dir_2_name.split("_"))
    dir_path_2 = os.path.join(dir_1_path, dir_2_name)

    return or_, a, tk, dir_path_2


def save_plot(dir_1_path, file_name, col_i, suptitle, plot_file_path):
    dir_2_names = os.listdir(dir_1_path)
    dir_2_names_sorted = sorted(
        map(partial(get_params_gen, dir_1_path), dir_2_names))

    plots_n = len(dir_2_names)

    plt.suptitle(suptitle, **{"x": 0.12, "y": 0.92})

    for plot_i, (or_, a, tk, dir_path_2) in enumerate(dir_2_names_sorted, 1):
        print(or_, a)

        file_path = os.path.join(dir_path_2, file_name)
        data = np.loadtxt(file_path)

        plt.subplot(plots_n, 1, plot_i)

        subtitle = r'$\omega_1/\omega_2$: {0:.2f}, A: {1:.3f}'.format(or_, a)
        plt.title(subtitle)

        plt.grid()
        plt.xlabel("Т, с")
        plt.subplots_adjust(hspace=1)

        plt.plot(data[:, 0], data[:, col_i])

    plt.savefig(plot_file_path)


def main():
    data_dir_name = "output"
    plots_dir_name = "plots"

    file_name_col_i_dict = {"ksi.txt": 1, "sily.txt": 4, "perem.txt": 4}

    plt.figure(figsize=(20.24, 7.68))

    dir_1_names = os.listdir(data_dir_name)
    for dir_1_name in dir_1_names:
        ae, be, h = map(float, dir_1_name.split("_"))
        print(ae, be, h)

        dir_1_path = os.path.join(data_dir_name, dir_1_name)

        plot_dir_path = os.path.join(plots_dir_name, dir_1_name)

        if not os.path.exists(plot_dir_path):
            os.makedirs(plot_dir_path)

        for file_name, col_i in file_name_col_i_dict.items():
            suptitle = r'$H$: {}'.format(h)

            plot_file_name = "{}.png".format(file_name)
            plot_file_path = os.path.join(plot_dir_path, plot_file_name)

            save_plot(dir_1_path, file_name, col_i, suptitle, plot_file_path)
            plt.clf()

            # plt.show()


if __name__ == "__main__":
    main()
