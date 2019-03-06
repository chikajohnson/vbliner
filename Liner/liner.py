from os import path as pt


class VBLiner:
    def append_line_number(self, file_name, start_line=0, end_line=10000):
        if pt.exists(file_name):
            with open("index.txt", "a") as myfile:
                for item in myfile.writelines():
                    item.write("First Line")
                    item.write("Second Line ")


