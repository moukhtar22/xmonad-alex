#!/usr/bin/python

import os

def get_all_pdfs_in_documents() -> list[str]:
    def set_up_root_dir() -> None:
        HOME: str | None = os.getenv("HOME")
        if HOME is None:
            print("Failed to fetch envvars")
            exit()
        os.chdir(HOME + "/Documents")
    set_up_root_dir()
    all_pdfs: list[str] = []
    for path_name, _, files in os.walk("."):
        pdf_files: list[str] = [ child for child in files if child.endswith(".pdf") ]
        rel_path_name: str = path_name.replace("./", "")
        for pdf_file in pdf_files:
            all_pdfs.append(os.path.join(rel_path_name, pdf_file))
    return all_pdfs
