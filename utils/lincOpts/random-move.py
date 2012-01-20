import os
import shutil
import argparse
import random

def make_args_parser():
    parser = argparse.ArgumentParser(
            description="Move N, randomly chosen files to a new directory.")
    parser.add_argument("from_dir",
            help="Source directory")
    parser.add_argument("to_dir",
            help="Destination directory")
    parser.add_argument("N", type=int,
            help="Number of files to move")
    # parser.add_argument("-e", "--regexp",
    #         help="Consider only files matching regexp")
    return parser

if __name__ == "__main__":
    parser = make_args_parser()
    args = parser.parse_args()

    files = os.listdir(args.from_dir)
    to_move = random.sample(files, args.N)
    for file_name in to_move:
        path_old = os.path.join(args.from_dir, file_name)
        path_new = os.path.join(args.to_dir, file_name)
        shutil.move(path_old, path_new)
