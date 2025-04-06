#!/usr/bin/python

import os
import argparse


class FZathuraParser(argparse.ArgumentParser):
    def __init__(self):
        super().__init__(
            prog="FZathura",
            description="A simple frontend for Zathura"
        )
        self.add_argument('-r', "--resume", action="store_true")
        self.add_argument('-m', "--menu", action="store_true")
        self.add_argument('-n', "--no-save-recent", action="store_true")
        self.add_argument('-p', "--pdf")


def set_up_root_dir() -> None:
    HOME: str | None = os.getenv("HOME")
    if HOME is None:
        print("Failed to fetch envvars")
        exit()
    os.chdir(HOME + "/Documents")

def get_all_pdfs_in_documents() -> list[str]:
    all_pdfs: list[str] = []
    for path_name, _, files in os.walk("."):
        pdf_files: list[str] = [ child for child in files if child.endswith(".pdf") ]
        rel_path_name: str = path_name.replace("./", "")
        for pdf_file in pdf_files:
            all_pdfs.append(os.path.join(rel_path_name, pdf_file))
    return all_pdfs


set_up_root_dir()
args = FZathuraParser().parse_args()
