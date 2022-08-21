import os
import subprocess
from functools import partial
from itertools import product
from operator import itemgetter

import numpy as np

from search import search

params_list_dict_default = {
    "ae":  [1],
    "be":  [2],
    "or_": [0.5, 0.9, 0.98, 1.0, 1.02, 1.1, 1.5],
    "tk":  [150],
    "h":   [0.5, 0.75, 1.0], }

params_keys = params_list_dict_default.keys()


def run(ae, be, or_, a, tk, h):
    subprocess.run(
        ["bin/giper_1.exe", *map(str, [ae, be, or_, a, tk, h])],
        stdout=subprocess.DEVNULL)


def get_max_ksi(tk):
    ksi = np.loadtxt('ksi.txt')
    max_abs = np.max(np.absolute(ksi[:, 1]))
    max_t = ksi[-1, 0]

    return max_abs if max_t >= tk else 100


def move(ae, be, or_, a, tk, h):
    file_names = [
        "ampli.txt", "ksi.txt", "perem.txt",
        "res.txt", "resm.xls", "sily.txt"]

    dir_name_1 = "_".join(map(str, [ae, be, h]))
    dir_name_2 = "_".join(map(str, [or_, a, tk]))

    move_dir_name = os.path.join("output", dir_name_1, dir_name_2)

    if not os.path.exists(move_dir_name):
        os.makedirs(move_dir_name)

    for file_name in file_names:
        move_file_path = os.path.join(move_dir_name, file_name)
        os.replace(file_name, move_file_path)


def main():
    def f(params_dict_default, a):
        params_dict = params_dict_default | {"a": a}
        run(**params_dict)

        tk = params_dict_default["tk"]
        return get_max_ksi(tk)

    for params_default in product(*params_list_dict_default.values()):
        params_dict_default = dict(zip(params_keys, params_default))

        a = search(0, 1, 0.15, partial(f, params_dict_default), 0.01)
        params_dict = params_dict_default | {"a": a}
        params = itemgetter("ae", "be", "or_", "a", "tk", "h")(params_dict)
        dir_name = "_".join(map(str, params))

        move(*params)


if __name__ == "__main__":
    main()
