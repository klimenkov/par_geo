from itertools import product
import numpy as np
import os
import subprocess


config = {
    "omega_ratio": [0.5, 0.75, 0.98, 1.02, 1.25],
    "a":           [0.5],
    "tk":          [50],
    "h":           [0.5], }


def main():
    for params in product(*config.values()):
        subprocess.run(
            ["bin/giper_1.exe", *map(str, params)],
            stdout=subprocess.DEVNULL)

        file_names = ["ampli.txt", "ksi.txt", "perem.txt", "res.txt", "resm.xls", "sily.txt"]
        move_dir_name = os.path.join("output", "_".join(map(str, params)))

        if not os.path.exists(move_dir_name):
            os.makedirs(move_dir_name)

        for file_name in file_names:
            move_file_path = os.path.join(move_dir_name, file_name)
            os.replace(file_name, move_file_path)


if __name__ == "__main__":
    main()

